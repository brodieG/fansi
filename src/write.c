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

/* GENERAL NOTES ON WRITING
 *
 * Writing functions starting with "FANSI_W_" in this file (or static ones
 * starting with "W_" in other files) operate in measure and write modes.  The
 * pattern is:
 *
 * 1. Run in measure mode to get 'len'.
 * 2. Allocate 'len + 1' ('len' does not include terminating NULL).
 * 3. Re-run in write mode to write the buffer.
 *
 * The functions take a pointer a to a pointer to the first character in a
 * buffer (i.e. char ** buff).  If that points to NULL, then the function runs
 * in measure mode.  Otherwise, it runs in write mode.  The writing functions
 * should also accept an 'int len' parameter that measures how many bytes have
 * (or will be) written by other functions.
 *
 * Here is an example implementation that uses a loop to iterate between measure
 * and write mode.  Not all uses of write functions are in this form.
 *
 *     // Measure Mode
 *     struct FANSI_buff buff = {.buff=NULL, .len = 0};
 *     char * buff_track = buff.buff;
 *     int len = 0;
 *
 *     for(int k = 0; k < 2; ++k) {
 *       if(k) {
 *         // Write Mode
 *         FANSI_size_buff(&buff, len); # buff.buff is allocated
 *         buff_track = buff.buff;
 *         len = 0;  // reset len
 *       }
 *       len += FANSI_W_fun1(&(buff_track), len, ...);
 *       len += FANSI_W_fun2(&(buff_track), len, ...);
 *     }
 *     // Check
 *     if(buff_track - buff.buff != len)
 *       error("Out of sync - something bad happened!.");
 *
 *   vvvvvvvv
 * !> DANGER <!
 *   ^^^^^^^^
 *
 * Writing functions move the buffer pointer to point to the byte after the last
 * non-NULL byte they've written (generally this will be a NULL).
 *
 *   vvvvvvvv
 * !> DANGER <!
 *   ^^^^^^^^
 *
 * This makes it simpler to code measure/write as a two iteration loop that uses
 * the same code for measuring and writing, except for allocating the buffer.
 *
 * These functions return how many bytes are/will be written, and should also
 * check whether adding those bytes to the 'len' input would cause an 'int'
 * overflow.  Typically the functions will accept an error message and an
 * R-level input index so that they can provide a bit more guidnce as to what
 * happened during and overflow.
 *
 * There is a performance trade-off in using the exact same code to measure the
 * buffer size and to write it.  Many of the measurments are knowable ahead of
 * time, but here we'll generate the substrings to measure them, only to
 * regenerate them again to write them.  The advantage is that it is easier to
 * keep the code in sync between measure and write modes.
 */

/*
 * Allocates a fresh chunk of memory if the existing one is not large enough.
 *
 * We never intend to re-use what's already in memory so we don't realloc.  If
 * allocation is needed the buffer will be either twice as large as it was
 * before, or size `size` if that is greater than twice the size.
 *
 * Total buffer allocation will be size + 1 to allow for an additional NULL
 * terminator.
 */
void FANSI_size_buff(struct FANSI_buff * buff, int size) {
  // assumptions check that  SIZE_T fits INT_MAX + 1
  if(size < 0) error("Internal Error: negative buffer allocations disallowed.");
  size_t buff_max = (size_t)FANSI_lim.lim_int.max + 1;
  size_t size_req = (size_t)size + 1;
  size_t size_alloc = 0;
  if(size_req > buff_max)
    // Difficult to test as everything already checking for overflows
    // nocov start
    error(
      "%s  Requesting %zu",
      "Internal Error: max allowed buffer size is INT_MAX + 1.",
       size_req
    );
    // nocov end

  if(size_req > buff->len) {
    if(!buff->len) {
      // in theory little penalty to ask this minimum
      if(size_req < 128 && FANSI_lim.lim_int.max >= 127)
        size_alloc = 128;
      else
        size_alloc = size_req;
    } else {
      // More generic case
      if(buff->len > buff_max - buff->len) {
        // can't double size
        size_alloc = buff_max;
      } else if (size_req > buff->len + buff->len) {
        // doubling not enough
        size_alloc = size_req;
      } else {
        // double size
        size_alloc = buff->len + buff->len;
      }
    }
    if(size_alloc < size_req)
      // nocov start
      error(
        "Internal Error: buffer size computation error (%zu vs %zu).",
        size_alloc, size_req
      );
      // nocov end

    // Maybe should be re-alloc to free the previous buffer?
    buff->len = size_alloc;
    buff->buff = R_alloc(buff->len, sizeof(char));
  }
  if(!buff->buff)
    error("Internal Error: buffer not allocated.");
  *(buff->buff) = 0;  // Always reset the string, guaranteed one byte.
}
/*
 * Copy/Measure a NULL terminated string into the buffer.
 *
 * @len bytes already accumulated in the buffer (i.e. before the pointer).
 * @i index in overal character vector, needed to report overflow string.
 * @err_msg overflow error message
 */
int FANSI_W_copy(
  char ** buff, const char * tmp, int len, R_xlen_t i,
  const char * err_msg
) {
  size_t tmp_len = strlen(tmp);  // tmp must be NULL terminated
  if(tmp_len > (size_t) FANSI_lim.lim_int.max)
    FANSI_check_append_err(err_msg, i);

  FANSI_check_append(len, tmp_len, err_msg, i);
  if(*buff) {
    strcpy(*buff, tmp); // strcpy copies the terminating NULL
    *buff += tmp_len;
  }
  return tmp_len;
}
/*
 * Like FANSI_w_copy, but uses memcpy and a known length to copy.
 */
int FANSI_W_mcopy(
  char ** buff, const char * tmp, int tmp_len, int len, R_xlen_t i,
  const char * err_msg
) {
  FANSI_check_append(len, tmp_len, err_msg, i);
  if(*buff) {
    memcpy(*buff, tmp, (size_t) tmp_len);
    *buff += tmp_len;
    **buff = 0;  // not necessary, but helps to debug
  }
  return tmp_len;
}
/*
 * Fill an array by repeating a charater
 */

int FANSI_W_fill(
  char ** buff, const char tmp, int times,
  int len, R_xlen_t i, const char * err_msg
) {
  FANSI_check_append(len, times, err_msg, i);
  if(*buff) {
    for(int i = 0; i < times; ++i) *(*buff)++ = tmp;
    **buff = 0;  // not necessary, but helps to debug
  }
  return times;
}

/*
 * End Active Sequences
 *
 * Inspects a state object, and produces the set of escape sequences that will
 * close just the open sequences, to the extent possible.
 *
 * Intended for compatibility with crayon.
 *
 * If buff is NULL, then only the required size of the buffer is returned.
 *
 * Ideally we would store all the styles in e.g. 2 uint64_t, and then maybe each
 * style would have an associated 2 uint64_t of what they turn on and off, and
 * somehow we would have a system to determine what the minimal combination of
 * styles required to turn off all active styles.  This would guarantee we can
 * keep the on-off styles in sync, at the cost of quite a bit of complexity.
 *
 * So instead we hard-code everything and hope we keep it in sync.
 */

int FANSI_W_sgr_close(
  char ** buff, struct FANSI_sgr sgr, int len, int normalize, R_xlen_t i
) {
  int len0 = len;
  const char * err_msg = "Generating closing SGR";

  if(FANSI_sgr_active(sgr)) {
    if(normalize) {
      // We're deliberate in only closing things we know how to close in
      // both the state and in the ouptut string, that way we can check
      // state at the end to make sure we did actually close everything.

      if(sgr.font) {
        sgr.font = 0;
        len += FANSI_W_COPY(buff, "\033[10m");
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
        len += FANSI_W_COPY(buff, "\033[22m");
      }
      if(sgr.style & s_frakital) {
        sgr.style &= ~s_frakital;
        len += FANSI_W_COPY(buff, "\033[23m");
      }
      if(sgr.style & s_underline) {
        sgr.style &= ~s_underline;
        len += FANSI_W_COPY(buff, "\033[24m");
      }
      if(sgr.style & s_blink) {
        sgr.style &= ~s_blink;
        len += FANSI_W_COPY(buff, "\033[25m");
      }
      // 26 is opening prop spacing (50 to close)
      if(sgr.style & s_inverse) {
        sgr.style &= ~s_inverse;
        len += FANSI_W_COPY(buff, "\033[27m");
      }
      if(sgr.style & s_conceal) {
        sgr.style &= ~s_conceal;
        len += FANSI_W_COPY(buff, "\033[28m");
      }
      if(sgr.style & s_strikethrough) {
        sgr.style &= ~s_strikethrough;
        len += FANSI_W_COPY(buff, "\033[29m");
      }
      // Colors
      if(sgr.color >= 0) {
        sgr.color = -1;
        len += FANSI_W_COPY(buff, "\033[39m");
      }
      if(sgr.bg_color >= 0) {
        sgr.bg_color = -1;
        len += FANSI_W_COPY(buff, "\033[49m");
      }
      // Prop spacing
      if(sgr.style & s_propspc) {
        sgr.style &= ~s_propspc;
        len += FANSI_W_COPY(buff, "\033[50m");
      }
      // Border and ideogram
      if(sgr.border & (1U << 1U | 1U << 2U)) {
        sgr.border &= ~(1U << 1U | 1U << 2U);
        len += FANSI_W_COPY(buff, "\033[54m");
      }
      if(sgr.border & (1U << 3U)) {
        sgr.border &= ~(1U << 3U);
        len += FANSI_W_COPY(buff, "\033[55m");
      }
      if(sgr.ideogram > 0U) {
        for(unsigned int k = 0; k < 5; ++k) sgr.ideogram &= ~(1U << k);
        len += FANSI_W_COPY(buff, "\033[65m");
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
      len += FANSI_W_COPY(buff, "\033[0m");
    }
  }
  return len - len0;
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
int FANSI_W_sgr(
  char ** buff, struct FANSI_sgr sgr, int len, int normalize, R_xlen_t i
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

  int len0 = len;
  const char * err_msg = "Writing SGR tokens"; // for FANSI_W_COPY
  // biggest would be "\033[XXm" + NULL, won't fit e.g bright color codes
  // CAREFUL if we modify code to use `tmp` for other purposes.
  char tmp[6] = {0};
  char * buff_anchor = * buff;

  if(FANSI_sgr_active(sgr)) {
    if(!normalize) len += FANSI_W_COPY(buff, "\033[");
    // styles
    char tokval[2] = {0};
    for(unsigned int i = 1; i < 10; i++) {
      if((1U << i) & sgr.style) {
        *tokval = '0' + (char) i;
        len += FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    } }
    // styles outside 0-9

    if(sgr.style & (1 << 10)) {
      // fraktur
      len += FANSI_W_COPY(buff, make_token(tmp, "20", normalize));
    }
    if(sgr.style & (1 << 11)) {
      // double underline
      len += FANSI_W_COPY(buff, make_token(tmp, "21", normalize));
    }
    if(sgr.style & (1 << 12)) {
      // prop spacing
      len += FANSI_W_COPY(buff, make_token(tmp, "26", normalize));
    }
    // colors
    if(sgr.color > -1) {
      char tokval[17] = {0};  // largest: "38;2;255;255;255", 16 chars + NULL
      len += FANSI_W_COPY(
        buff,
        color_token(tokval, sgr.color, sgr.color_extra, 3, normalize)
      );
    }
    if(sgr.bg_color > -1) {
      char tokval[17] = {0};
      len += FANSI_W_COPY(
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
          len += FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    } } }
    // Ideogram
    if(sgr.ideogram) {
      char tokval[3] = {'6', '0'};
      for(int i = 0; i < 5; ++i){
        if((1 << i) & sgr.ideogram) {
          tokval[1] = '0' + i;
          len += FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    } } }
    // font
    if(sgr.font) {
      char tokval[3] = {'1', '0'};
      tokval[1] = '0' + (sgr.font % 10);
      len += FANSI_W_COPY(buff, make_token(tmp, tokval, normalize));
    }
    // Finalize (replace trailing ';' with 'm')
    if(*buff) {
      *((*buff) - 1) = 'm';
      // nocov start
      if(*buff - buff_anchor != len - len0)
        // nocov start
        error(
          "Internal Error: %s (%td vs %d).",
          "buffer length mismatch in writing SGR generation (2)",
          *buff - buff_anchor, len - len0
        );
      // nocov end
    }
  }
  else if(*buff) **buff = 0;  // for debugging, buff always should have 1 byte
  return len - len0;
}
