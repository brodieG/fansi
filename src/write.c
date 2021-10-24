/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
 */

#include "fansi.h"

/* GENERAL NOTES ON WRITING/ALLOCATING FUNCTONS
 *
 * - Measure / Write Pattern ---------------------------------------------------
 *
 * Writing functions starting with "FANSI_W_" in this file (or static ones
 * starting with "W_" in other files) operate in measure and write modes.  The
 * pattern is:
 *
 * 0. Reset the FANSI_buff object.
 * 1. Run in measure mode to get 'len'.
 * 2. Allocate the buffer with FANSI_size
 * 3. Re-run in write mode to write the buffer.
 *
 * The functions accept a pointer to a FANSI_buff object.  If the `.buff` member
 * points to NULL, the functions run in measure mode. Otherwise, they run in
 * write mode.
 *
 * Here is an example implementation that uses a loop to iterate between measure
 * and write mode:
 *
 *     struct FANSI_buff buff;
 *     FANSI_INIT_BUFF(&buff);
 *
 *     for(int k = 0; k < 2; ++k) {
 *       if(!k) FANSI_reset_buff(&buff);  // Read mode
 *       else   FANSI_size_buff(&buff);   // Write Mode
 *
 *       FANSI_W_fun1(&buff, ...);
 *       FANSI_W_fun2(&buff, ...);
 *     }
 *     // Do stuff
 *     ...
 *
 *     FANSI_release_buff(&buff, 1);
 *
 * NOTE: avoid using `R_alloc` in functions that use FANSI buffers, or in
 *       functions used by such functions (see "Buffer Allocation" below).
 *
 * Buffers must be reset prior to the measure pass.  Use FANSI_size_buff0 if you
 * know the size ahead of time and don't need the two pass measure/write
 * approach.
 *
 * The key workhorses are the macros FANSI_W_COPY and FANSI_W_MCOPY which
 * roughly mimic the semantics of `strcpy` and `memcpy` respectively.  Functions
 * that only use these functions to write to the buffer and accept the buffer by
 * reference can then be used as `FANSI_W_fun1/2` are used above.
 *
 *   vvvvvvvv
 * !> DANGER <!
 *   ^^^^^^^^
 *
 * Writing functions move the buffer pointer to point to the byte after the last
 * non-NULL byte they've written (this will be a NULL, for FANSI_W_M?COPY, and
 * other functions should try to ensure the same).
 *
 * This makes it simpler to code measure/write as a two iteration loop that uses
 * the same code for measuring and writing, except for allocating the buffer.
 *
 * These functions generally return void, but some return how many bytes
 * are/will be written.  They typically will check for overflow of int in the
 * measure part, and then for overflow or underuse of the allocated buffer on
 * the second pass.  Ideally functions will internally use FANSI_W_M?COPY, which
 * do the overflow checks.
 *
 * The `FANSI_mkChar*` functions also check that any provided buffer has had
 * written the expected number of bytes.  If a buffer is not written out with
 * `FANSI_mkChar*` the buffer should be checked with `FANSI_check_buff`.
 *
 * There is a performance trade-off in using the exact same code to measure the
 * buffer size and to write it.  Many of the measurments are knowable ahead of
 * time, but here we'll generate the substrings to measure them, only to
 * regenerate them again to write them.  The advantage is that it is easier to
 * keep the code in sync between measure and write modes.
 *
 * - Buffer Allocation ---------------------------------------------------------
 *
 * Buffers should be initialized with `FANSI_init_buff`, and released with
 * `FANSI_release_buff`, preferably in the same function.  `FANSI_size_buff`
 * must be called at least once to actually allocate memory (init does not do
 * this), and may be called repeatedly to resize the buffer.
 *
 * Internally, the same buffer is used if it is big enough to accomodate a new
 * `FANSI_size_buff` request, but semantically it is as if a fresh buffer is
 * requested each time (i.e. this is not a buffer designed to accomodate
 * variable length data).
 *
 * Each time the buffer is grown, we attempt to release the prior buffer to make
 * it eligible for garbage collection.  If only one buffer is ever in use at a
 * time, this will always work.  If multiple buffers are active (e.g. because a
 * sub-function also uses a buffer), the release only works if the buffers are
 * resized and released LIFO, e.g.:
 *
 *     // Bad
 *     FANSI_size_buff(&buff1);
 *     FANSI_size_buff(&buff2);
 *     FANSI_size_buff(&buff1);              // warning ...
 *
 *     // Okay
 *     FANSI_size_buff(&buff1);
 *     FANSI_size_buff(&buff2);
 *     FANSI_release_buff(&buff2, 1);        // no warning
 *     FANSI_size_buff(&buff1);              // no warning
 *
 * Avoid using `R_alloc` inside _W_ functions or their children unless you reset
 * the `vmax` values before existing.  Failure to do so (e.g. if you allocate a
 * buffer and don't release it before return) will prevent FANSI_release_buff
 * from freeing it's own buffers.
 *
 * - Testing -------------------------------------------------------------------
 *
 * See extra/notes/mem-alloc.md for how we tested the allocation/release
 * business is working as expected.
 */

void FANSI_init_buff(struct FANSI_buff * buff, const char * fun) {
  *buff = (struct FANSI_buff) {
    .buff=NULL,
    .buff0=NULL,
    .len=0,
    .len_alloc=0,
    .vheap_self=NULL,
    .vheap_prev=NULL,
    .fun=fun,
    .warned=0,
    .reset=0            // init does not reset
  };
}
// Strict requires that the buff be used exactly and completely, otherwise okay
// to under-use (but not over use, of course).  We explicitly check against
// target len, not allocated len to avoid missing overruns hidden by a generous
// allocation.

void FANSI_check_buff(struct FANSI_buff buff, R_xlen_t i, int strict) {
  if(buff.buff < buff.buff0)
    // nocov start
    error(
      "Internal Error: buffer reversed at index[%jd] (allocated by %s).",
      FANSI_ind(i), buff.fun
    );
    // nocov end

  if(
    (strict && buff.buff - buff.buff0 != buff.len) ||
    (buff.buff - buff.buff0 > buff.len)
  )
    // nocov start
    error(
      "%s[%jd](%td vs %d alloc'ed by %s).",
      "Internal Error: buffer not of specified length at index",
      FANSI_ind(i), buff.buff - buff.buff0, buff.len, buff.fun
    );
    // nocov end
}

/*
 * Attempts to remove a buffer from the R_alloc protection stack.
 *
 * This only works if the current buffer is at the end of the stack.  Each
 * `FANSI_init_buff` call should be paired with a release.  Some thought should
 * go into the sequence of initializations and releases when multiple such
 * buffers coexist.  They should be done on a LIFO basis as the release only
 * works if the buffer was the last to be allocated.  Additionally, calls to
 * size_buff should not be interleaved as that will preclude release of the
 * buffers:
 *
 * The purpose of the release is to make the last allocation eligible for GC by
 * removing it from the R_alloc stack.
 *
 * returns 0 if successfully releases buffer.  Zeroes the buffer in all cases.
 * A failure is not critical as the entire R_alloc stack will be released on
 * return to R, it just means peak memory usage will be higher than it might
 * otherwise be.
 */
int FANSI_release_buff(struct FANSI_buff * buff, int warn) {
  int failure = 0;
  if(buff->buff0) {
    if(buff->vheap_self == vmaxget()) vmaxset(buff->vheap_prev);
    else {
      if(warn && !buff->warned)
        warning(
          "%s %s %s",
          "Unable to release buffer allocated by",
          buff->fun,
          "while in native code. Buffer will be released on return to R."
        );
      failure = 1;
      buff->warned = 1;
    }
    buff->vheap_prev = NULL;
    buff->vheap_self = NULL;
    buff->buff = NULL;
    buff->buff0 = NULL;
    buff->len = 0;
    buff->len_alloc = 0;
  }
  return failure;
}

/*
 * Allocates a fresh chunk of memory if the existing one is not large enough.
 *
 * If allocation is needed the buffer will be either twice as large as it was
 * before, or size `size` if that is greater than twice the size.
 *
 * Total buffer allocation will be `size + 1` to allow for an additional NULL
 * terminator.
 *
 * Every call to FANSI_size_buff "zeroes" the buffer by setting the first byte
 * to 0 and the `.buff` member to point to the beginning of the buffer.
 *
 * The _buff0 version is when the size does not need to be measured explicitly.
 */
size_t FANSI_size_buff0(struct FANSI_buff * buff, int size) {
  if(size < 0)
    error(
      "Internal Error: negative buffer allocations disallowed in %s.", buff->fun
    );
  buff->reset = 0;

  // assumptions check that SIZE_T fits INT_MAX + 1
  size_t buff_max = (size_t)FANSI_lim.lim_int.max + 1;
  size_t size_req = (size_t)size + 1;
  size_t size_alloc = 0;
  if(size_req > buff_max)
    error(
      "%s (req: %zu vs lim: %zu), in %s.",
      "Internal Error: max allowed buffer size is INT_MAX + 1.",
      size_req, buff_max, buff->fun
    );

  if(size_req > buff->len_alloc) {
    if(!buff->len_alloc) {
      // in theory little penalty to ask this minimum
      if(size_req < 128 && FANSI_lim.lim_int.max >= 127)
        size_alloc = 128;
      else
        size_alloc = size_req;
    } else {
      // More generic case
      if(buff->len_alloc > buff_max - buff->len_alloc) {
        // can't double size
        size_alloc = buff_max;
      } else if (size_req > buff->len_alloc + buff->len_alloc) {
        // doubling not enough
        size_alloc = size_req;
      } else {
        // double size
        size_alloc = buff->len_alloc + buff->len_alloc;
      }
    }
    if(size_alloc < size_req)
      // nocov start
      error(
        "Internal Error: buffer size computation error (%zu vs %zu) in %s.",
        size_alloc, size_req, buff->fun
      );
      // nocov end

    FANSI_release_buff(buff, 1);
    // Keep this in sync with FANSI_release_buff!
    buff->vheap_prev = vmaxget();
    buff->len_alloc = size_alloc;
    buff->buff0 = buff->buff = R_alloc(buff->len_alloc, sizeof(char));
    buff->vheap_self = vmaxget();
  } else {
    buff->buff = buff->buff0;
  }
  if(!buff->buff)
    error("Internal Error: buffer not allocated in %s.", buff->fun);// nocov
  buff->len = size;
  *(buff->buff) = 0;  // Always reset the string, guaranteed one byte.
  return buff->len_alloc;
}
size_t FANSI_size_buff(struct FANSI_buff * buff) {
  if(!buff->reset)
    error("Internal Error: attempt to size buffer w/o reset in %s.", buff->fun);
  return FANSI_size_buff0(buff, buff->len);
}
/*
 * Prepare the buffer for the measure pass
 */
void FANSI_reset_buff(struct FANSI_buff * buff) {
  buff->len = 0;
  buff->buff = NULL;
  buff->reset = 1;    // Internal, only for _(reset|size)_buff
}

/*
 * Purely for testing if the prev/self scheme used by size_buff works as
 * intended.
 */

static void prot_test_help(
  int size, const char * lbl, struct FANSI_buff * buff, SEXP res, R_xlen_t i
) {
  char tmp[256];
  FANSI_size_buff0(buff, size);
  INTEGER(VECTOR_ELT(res, 1))[i] = buff->len_alloc;
  SET_STRING_ELT(VECTOR_ELT(res, 0), i, mkChar(lbl));
  sprintf(tmp, "%p", buff->vheap_self);
  SET_STRING_ELT(VECTOR_ELT(res, 3), i, mkChar(tmp));
  sprintf(tmp, "%p", buff->vheap_prev);
  SET_STRING_ELT(VECTOR_ELT(res, 2), i, mkChar(tmp));
}
SEXP FANSI_size_buff_prot_test() {
  struct FANSI_buff buff1, buff2;
  FANSI_INIT_BUFF(&buff1);
  FANSI_INIT_BUFF(&buff2);

  R_xlen_t n = 9;
  SEXP res = PROTECT(allocVector(VECSXP, 4));
  SEXP res_n = PROTECT(allocVector(INTSXP, n));
  SEXP res_lbl  = PROTECT(allocVector(STRSXP, n));
  SEXP res_self = PROTECT(allocVector(STRSXP, n));
  SEXP res_prev = PROTECT(allocVector(STRSXP, n));
  SET_VECTOR_ELT(res, 0, res_lbl);
  SET_VECTOR_ELT(res, 1, res_n);
  SET_VECTOR_ELT(res, 2, res_prev);
  SET_VECTOR_ELT(res, 3, res_self);
  UNPROTECT(4);

  // Big enough buffers so they are not in the small object heap so no confusion

  R_xlen_t i = 0;
  prot_test_help(4095, "first", &buff1, res, i++);
  prot_test_help(2047, "smaller 1.0", &buff1, res, i++);
  // This should cause 'self' to change, while, leaving 'prev' unchanged
  prot_test_help(8191, "grow 1.0", &buff1, res, i++);
  // New buffer, this should cause both 'self' and 'prev' to change
  prot_test_help(2047, "new buff", &buff2, res, i++);
  // Back to old buffer, no grow, no changes
  prot_test_help(2047, "smaller 1.1", &buff1, res, i++);
  // New buffer, no grow, no changes
  prot_test_help(1023, "smaller 2.0", &buff2, res, i++);
  // New buffer, grow, prev should change to buffer2, self should change
  prot_test_help(4095, "grow 2.0", &buff2, res, i++);
  // Growing old buffer should change prev
  prot_test_help(16383, "grow 1.1", &buff1, res, i++);
  // Growing new buffer should also change prev
  prot_test_help(8191, "grow 2.1", &buff2, res, i++);

  // Release LIFO, should be no warnings.  However, we don't release everything
  // because we had sequential allocations.
  FANSI_release_buff(&buff2, 1);
  FANSI_release_buff(&buff1, 1);

  if(i != n) error("Internal Error: wrong step count."); // nocov

  UNPROTECT(1);
  return res;
}
/*
 * To test allocation logic is doing what is expected.  This will allocate
 * as many bytes as each value in `x` so, don't do anything crazy.
 */
SEXP FANSI_size_buff_ext(SEXP x) {
  if(TYPEOF(x) != INTSXP)
    error("Argument `x` must be integer.");

  R_xlen_t i, len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(REALSXP, len));
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  for(i = 0; i < len; ++i) {
    size_t size = FANSI_size_buff0(&buff, INTEGER(x)[i]);
    REAL(res)[i] = (double) size;
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}

/*
 * Copy/Measure a NULL terminated string into the buffer.
 *
 * @len bytes already accumulated in the buffer (i.e. before the pointer).
 * @i index in overal character vector, needed to report overflow string.
 * @err_msg overflow error message
 */
int FANSI_W_copy(
  struct FANSI_buff * buff, const char * tmp, R_xlen_t i, const char * err_msg
) {
  size_t tmp_len = strlen(tmp);  // tmp must be NULL terminated
  if(tmp_len > (size_t) FANSI_lim.lim_int.max)
    FANSI_check_append_err(err_msg, i);

  if(buff->buff) {
    if((buff->buff - buff->buff0) + (int)tmp_len > buff->len)
      error("Internal Error: exceeded target buffer size in _copy.");
    strcpy(buff->buff, tmp); // strcpy copies the terminating NULL
    buff->buff += tmp_len;
  } else {
    FANSI_check_append(buff->len, tmp_len, err_msg, i);
    buff->len += tmp_len;
  }
  return (int) tmp_len;
}
/*
 * Like FANSI_w_copy, but uses memcpy and a known length to copy.
 */
int FANSI_W_mcopy(
  struct FANSI_buff * buff, const char * tmp, int tmp_len, R_xlen_t i,
  const char * err_msg
) {
  if(buff->buff) {
    if(buff->buff - buff->buff0 + tmp_len > buff->len)
      error("Internal Error: exceeded target buffer size in _mcopy.");
    memcpy(buff->buff, tmp, (size_t) tmp_len);
    buff->buff += tmp_len;
    *(buff->buff) = 0;  // as documented
  } else {
    FANSI_check_append(buff->len, tmp_len, err_msg, i);
    buff->len += tmp_len;
  }
  return tmp_len;
}
/*
 * Fill an array by repeating a charater
 */

void FANSI_W_fill(
  struct FANSI_buff * buff, const char tmp, int times,
  R_xlen_t i, const char * err_msg
) {
  if(buff->buff) {
    if(buff->buff - buff->buff0 + times > buff->len)
      error("Internal Error: exceeded allocated buffer in _fill.");

    for(int i = 0; i < times; ++i) *(buff->buff)++ = tmp;
    *(buff->buff) = 0;  // not necessary, but helps to debug
  } else {
    FANSI_check_append(buff->len, times, err_msg, i);
    buff->len += times;
  }
}

/*
 * End Active Sequences
 *
 * Inspects a state object, and produces the set of escape sequences that will
 * close just the open sequences, to the extent possible.
 *
 * Intended for compatibility with crayon.
 *
 * If buff is unallocated, how many bytes are required is computed.
 *
 * Ideally we would store all the styles in e.g. 2 uint64_t, and then maybe each
 * style would have an associated 2 uint64_t of what they turn on and off, and
 * somehow we would have a system to determine what the minimal combination of
 * styles required to turn off all active styles.  This would guarantee we can
 * keep the on-off styles in sync, at the cost of quite a bit of complexity.
 *
 * So instead we hard-code everything and hope we keep it in sync.
 */

void FANSI_W_sgr_close(
  struct FANSI_buff * buff, struct FANSI_sgr sgr, int normalize, R_xlen_t i
) {
  const char * err_msg = "Generating closing SGR";

  if(FANSI_sgr_active(sgr)) {
    if(normalize) {
      // We're deliberate in only closing things we know how to close in
      // both the state and in the ouptut string, that way we can check
      // state at the end to make sure we did actually close everything.

      if(sgr.font) {
        sgr.font = 0;
        FANSI_W_COPY(buff, "\033[10m");
      }
      unsigned int s_boldfaint = (1U << 1U | 1U << 2U);
      unsigned int s_frakital = (1U << 3U | 1U << 10U);
      unsigned int s_underline = (1U << 4U | 1U << 11U);
      unsigned int s_blink = (1U << 5U | 1U << 6U);
      unsigned int s_propspc = 1U << 12U;
      unsigned int s_inverse = 1U << 7U;
      unsigned int s_conceal = 1U << 8U;
      unsigned int s_strikethrough = 1U << 9U;

      if(sgr.style & s_boldfaint) {
        sgr.style &= ~s_boldfaint;
        FANSI_W_COPY(buff, "\033[22m");
      }
      if(sgr.style & s_frakital) {
        sgr.style &= ~s_frakital;
        FANSI_W_COPY(buff, "\033[23m");
      }
      if(sgr.style & s_underline) {
        sgr.style &= ~s_underline;
        FANSI_W_COPY(buff, "\033[24m");
      }
      if(sgr.style & s_blink) {
        sgr.style &= ~s_blink;
        FANSI_W_COPY(buff, "\033[25m");
      }
      // 26 is opening prop spacing (50 to close)
      if(sgr.style & s_inverse) {
        sgr.style &= ~s_inverse;
        FANSI_W_COPY(buff, "\033[27m");
      }
      if(sgr.style & s_conceal) {
        sgr.style &= ~s_conceal;
        FANSI_W_COPY(buff, "\033[28m");
      }
      if(sgr.style & s_strikethrough) {
        sgr.style &= ~s_strikethrough;
        FANSI_W_COPY(buff, "\033[29m");
      }
      // Colors
      if(sgr.color >= 0) {
        sgr.color = -1;
        FANSI_W_COPY(buff, "\033[39m");
      }
      if(sgr.bg_color >= 0) {
        sgr.bg_color = -1;
        FANSI_W_COPY(buff, "\033[49m");
      }
      // Prop spacing
      if(sgr.style & s_propspc) {
        sgr.style &= ~s_propspc;
        FANSI_W_COPY(buff, "\033[50m");
      }
      // Border and ideogram
      if(sgr.border & (1U << 1U | 1U << 2U)) {
        sgr.border &= ~(1U << 1U | 1U << 2U);
        FANSI_W_COPY(buff, "\033[54m");
      }
      if(sgr.border & (1U << 3U)) {
        sgr.border &= ~(1U << 3U);
        FANSI_W_COPY(buff, "\033[55m");
      }
      if(sgr.ideogram > 0U) {
        for(unsigned int k = 0; k < 5; ++k) sgr.ideogram &= ~(1U << k);
        FANSI_W_COPY(buff, "\033[65m");
      }

      // Make sure we're not out of sync with has_style
      if(FANSI_sgr_active(sgr))
        error(
          "Internal Error: %s (clr: %d bg: %d st: %u bd: %u id %u).",
          "did not successfully close all styles",
          sgr.color, sgr.bg_color, sgr.style, sgr.border, sgr.ideogram
        );
    } else {
      // Full close
      FANSI_W_COPY(buff, "\033[0m");
    }
  }
}
/*
 * End Active URL
 */
void FANSI_W_url_close(
  struct FANSI_buff * buff, struct FANSI_url url, R_xlen_t i
) {
  const char * err_msg = "Generating URL end";
  if(FANSI_url_active(url)) FANSI_W_COPY(buff, "\033]8;;\033\\");
}

/*
 * Helper to make an SGR token, possibly full SGR if in normalize mode
 */
static char * make_token(char * buff, const char * val, int normalize) {
  if(strlen(val) > 2)
    error("Internal error: token maker limited to 2 chars max."); // nocov
  if(!normalize) {
    strcpy(buff, val);
    strcat(buff, ";");
  } else {
    char * buff_track = buff;
    *(buff_track++) = '\033';
    *(buff_track++) = '[';
    strcpy(buff_track, val);
    strcat(buff_track, "m");
  }
  return buff;
}
/*
 * Write extra color info to string
 *
 * buff should be at least 20 bytes.
 * largest: "\033[48;2;255;255;255m", 19 chars + NULL
 */
static char * color_token(
  char * buff, int color, int * color_extra, int mode, int normalize
) {
  if(mode != 3 && mode != 4)
    error("Internal Error: color mode must be 3 or 4");  // nocov

  char * buff_track = buff;

  if(normalize) {
    *(buff_track++) = '\033';
    *(buff_track++) = '[';
  }
  if(color >= 0 && color < 10) {  // should this be < 9?
    *(buff_track++) = '0' + mode;
    *(buff_track++) = '0' + color;
    if(color == 8) {
      *(buff_track++) = ';';
      int write_chrs = 0;
      if(color_extra[0] == 2) {
        write_chrs = sprintf(
          buff_track, "2;%d;%d;%d",
          color_extra[1], color_extra[2], color_extra[3]
        );
      } else if(color_extra[0] == 5) {
        write_chrs = sprintf(buff_track, "5;%d", color_extra[1]);
      } else error("Internal Error: unexpected color code.");  // nocov
      if(write_chrs < 0)
        error("Internal Error: failed writing color code.");   // nocov
      buff_track += write_chrs;
    }
  } else if(color >= 100 && color <= 107) {
    // bright colors, we don't actually need to worry about bg vs fg since the
    // actual color values are different
    *(buff_track++) = '1';
    *(buff_track++) = '0';
    *(buff_track++) = '0' + color - 100;
  } else if(color >= 90 && color <= 97) {
    *(buff_track++) = '9';
    *(buff_track++) = '0' + color - 90;
  } else {
    error("Internal Error: unexpected color code.");  // nocov
  }
  if(normalize) *(buff_track++) = 'm';
  else *(buff_track++) = ';';
  *buff_track = 0;
  if(buff_track - buff > 19)  // too late if this happened...
    error("Internal Error: exceeded color buffer.");  // nocov
  return buff;
}
/*
 * Output an SGR state as a string.
 *
 * Set buff to NULL to get size instead of writing.
 *
 * Return how many needed / written bytes.
 */
void FANSI_W_sgr(
  struct FANSI_buff * buff, struct FANSI_sgr sgr, int normalize, R_xlen_t i
) {
  /****************************************************\
  | IMPORTANT:                                         |
  | KEEP THIS ALIGNED WITH state_as_html               |
  | although right now ignoring rare escapes in html   |
  |                                                    |
  | DO NOT CHANGE ORDER of writing.  Added tokens      |
  | go at end.  We picked a bad order at the beginning |
  | and now we're stuck.                               |
  \****************************************************/

  const char * err_msg = "Writing SGR tokens"; // for FANSI_W_COPY
  // biggest would be "\033[XXm" + NULL, won't fit e.g bright color codes
  // CAREFUL if we modify code to use `tmp` for other purposes.
  char tmp[6] = {0};

  if(FANSI_sgr_active(sgr)) {
    if(!normalize) FANSI_W_COPY(buff, "\033[");
    // styles
    char tokval[2] = {0};
    for(unsigned int i = 1; i < 10; i++) {
      if((1U << i) & sgr.style) {
        *tokval = '0' + (char) i;
        FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    } }
    // styles outside 0-9

    if(sgr.style & (1 << 10)) {
      // fraktur
      FANSI_W_COPY(buff, make_token(tmp, "20", normalize));
    }
    if(sgr.style & (1 << 11)) {
      // double underline
      FANSI_W_COPY(buff, make_token(tmp, "21", normalize));
    }
    if(sgr.style & (1 << 12)) {
      // prop spacing
      FANSI_W_COPY(buff, make_token(tmp, "26", normalize));
    }
    // colors
    if(sgr.color > -1) {
      char tokval[17] = {0};  // largest: "38;2;255;255;255", 16 chars + NULL
      FANSI_W_COPY(
        buff,
        color_token(tokval, sgr.color, sgr.color_extra, 3, normalize)
      );
    }
    if(sgr.bg_color > -1) {
      char tokval[17] = {0};
      FANSI_W_COPY(
        buff,
        color_token(tokval, sgr.bg_color, sgr.bg_color_extra, 4, normalize)
      );
    }
    // Borders
    if(sgr.border) {
      char tokval[3] = {'5', '0'};
      for(int i = 1; i < 4; ++i) {
        if((1 << i) & sgr.border) {
          tokval[1] = '0' + i;
          FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    } } }
    // Ideogram
    if(sgr.ideogram) {
      char tokval[3] = {'6', '0'};
      for(int i = 0; i < 5; ++i){
        if((1 << i) & sgr.ideogram) {
          tokval[1] = '0' + i;
          FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    } } }
    // font
    if(sgr.font) {
      char tokval[3] = {'1', '0'};
      tokval[1] = '0' + (sgr.font % 10);
      FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    }
    // Finalize (replace trailing ';' with 'm')
    if(buff->buff) {
      *((buff->buff) - 1) = 'm';
    }
  }
  // for debugging, buff always should have 1 byte
  else if(buff->buff) *(buff->buff) = 0;
}
/*
 * Output an URL state as a string.
 *
 * Set buff to NULL to get size instead of writing.
 *
 * Return how many needed / written bytes.
 */
void FANSI_W_url(
  struct FANSI_buff * buff, struct FANSI_url url, int normalize, R_xlen_t i
) {
  /****************************************************\
  | IMPORTANT:                                         |
  | KEEP THIS ALIGNED WITH state_as_html               |
  | See _W_sgr                                         |
  \****************************************************/

  const char * err_msg = "Writing URL"; // for FANSI_W_M?COPY
  FANSI_W_COPY(buff, "\033]8;");
  if(normalize) {
    if(url.id.val)
      FANSI_W_MCOPY(buff, url.id.val, url.id.len);
  } else if(url.params.val) {
    FANSI_W_MCOPY(buff, url.params.val, url.params.len);
  }
  FANSI_W_COPY(buff, ";");
  FANSI_W_MCOPY(buff, url.url.val, url.url.len);
  FANSI_W_COPY(buff, "\033\\");  // ST
}
