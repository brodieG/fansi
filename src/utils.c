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
/*
 * Used to set a global limit values for testing purposes.
 *
 * This does not affect FANSI_add_int as that we can test separately, and
 * setting it there prevents us from testing some of the downstream overflow
 * logic.
 *
 * Watch out that we don't set R_LEN_T_MAX to be less than the length of any
 * test vector, as it is implicitly assumed no vector can be longer than
 * R_LEN_T_MAX.
 */

#define LIM_INIT {\
  .lim_int={.name="INT", .min=INT_MIN, .max=INT_MAX},       \
  .lim_R_len_t={.name="R_LEN_T", .min=0, .max=R_LEN_T_MAX}, \
  .lim_size_t={.name="SIZE", .min=0, .max=SIZE_MAX}         \
}

struct FANSI_limits lim_init = LIM_INIT;

SEXP FANSI_set_int_max(SEXP x) {
  if(TYPEOF(x) != INTSXP || XLENGTH(x) != 1)
    error("invalid int_max value");  // nocov
  int x_int = asInteger(x);

  if(x_int < 1)
    error("int_max value must be positive"); // nocov

  int old_int = FANSI_lim.lim_int.max;
  FANSI_lim.lim_int.max = (intmax_t) x_int;
  return ScalarInteger(old_int);
}
SEXP FANSI_reset_limits() {
  FANSI_lim = LIM_INIT;
  return ScalarLogical(1);
}
// nocov start
// used only for debugging
SEXP FANSI_get_int_max() {
  return ScalarInteger(FANSI_lim.lim_int.max);
}
// nocov end
/*
 * Note we are stricter than necessary when y is negative because we want to
 * count hitting INT_MIN as an overflow so that we can use the integer values
 * in R where INT_MIN is NA.
 */

int FANSI_add_int(int x, int y, const char * file, int line) {
  // don't use FANSI_lim.lim.* as that locks up testing other things
  if(
    (y >= 0 && (x > INT_MAX - y)) ||
    (y < 0 && (x <= INT_MIN - y))
  )
    error(
      "Integer overflow in file %s at line %d; %s", file, line,
      "contact maintainer."
    );
  return x + y;
}
SEXP FANSI_add_int_ext(SEXP x, SEXP y) {
  if(
    TYPEOF(x) != INTSXP || XLENGTH(x) != 1 ||
    TYPEOF(y) != INTSXP || XLENGTH(y) != 1
  )
    error("Internal error: arguments must be scalar integers"); // nocov

  return ScalarInteger(FANSI_ADD_INT(asInteger(x), asInteger(y)));
}
/*
 * Compute Location and Size of Next ANSI Sequences
 *
 * See FANSI_parse_esc as well, where there is similar logic, although we keep
 * it separated here for speed since we don't try to interpret the string.
 *
 * Length includes the ESC and [, and start point is the ESC.
 *
 * Validity here means striclty that all the contained escape sequences were
 * valid CSI sequences as per the strict definition.
 *
 * We report the length of invalid sequnces, but you really can't trust them.
 * The true length may actually be different depending on your terminal,
 * (e.g. OSX terminal spits out illegal characters to screen but keeps
 * processing the sequence).
 *
 * @param ctl is a bit flag to line up against VALID.WHAT index values, so
 *   (ctl & (1 << 0)) is newlines, (ctl & (1 << 1)) is C0, etc, though note
 *   this does not act
 */

struct FANSI_csi_pos FANSI_find_esc(const char * x, int ctl) {
  /***************************************************\
  | IMPORTANT: KEEP THIS ALIGNED WITH FANSI_read_esc  |
  | although now this also deals with c0              |
  \***************************************************/
  int valid = 1;
  int found = 0;
  int found_ctl = 0;
  const char * x_track = x;
  const char * x_found_start;
  const char * x_found_end;

  struct FANSI_csi_pos res;

  while(*x_track) {
    const char x_val = *(x_track++);
    // use found & found_this in conjunction so that we can allow multiple
    // adjacent elements to be found in one go

    int found_this = 0;

    // If not normal ASCII or UTF8, examine whether we need to found
    if(!((x_val > 31 && x_val < 127) || x_val < 0 || x_val > 127)) {
      if(!found) {
        // Keep resetting strip start point until we find something we want to
        // mark
        x_found_start = x_found_end = x_track - 1;
      }
      found_this = 0;
      if(x_val == 27) {
        if(*x_track == '[') {
          // This is a CSI sequence, so it has multiple characters that we
          // need to skip.  The final character is processed outside of here
          // since it has the same logic for CSI and non CSI sequences

          // skip [

          ++x_track;

          // Skip all the valid parameters tokens

          while(*x_track >= 0x30 && *x_track <= 0x3F) ++x_track;

          // And all the valid intermediates

          int intermediate = 0;
          while(*x_track >= 0x20 && *x_track <= 0x2F) {
            if(!intermediate) intermediate = 1;
            ++x_track;
          }
          // Check validity

          int valid_tmp = *x_track >= 0x40 && *x_track <= 0x7E;

          // If not valid, consume all subsequent parameter tokens  as that
          // seems to be terminal.osx and iterm behavior (though terminal.osx
          // seems pretty picky about what it considers intermediate or even
          // parameter characters).

          if(!valid_tmp)
            while(*x_track >= 0x20 && *x_track <= 0x3F) ++x_track;

          valid = valid && valid_tmp;

          // CSI SGR only found if ends in m and no intermediate

          int sgr = !intermediate && *x_track == 'm';
          found_ctl |= sgr ? FANSI_CTL_SGR & ctl : FANSI_CTL_CSI & ctl;
          found_this =
            (sgr && (ctl & FANSI_CTL_SGR)) ||  // SGR
            (!sgr && (ctl & FANSI_CTL_CSI));      // CSI
        } else {
          // Includes both the C1 set and "controls strings"
          found_this = ctl & FANSI_CTL_ESC;
          found_ctl |= ctl & FANSI_CTL_ESC;
          valid = valid && (*x_track >= 0x40 && *x_track <= 0x7E);
        }
        // Advance unless next char is ESC, in which case we want to keep
        // looping

        if(*x_track && *x_track != 27) x_track++;
      } else {
        // x01-x1F, x7F, all the C0 codes

        found_ctl |= (x_val == '\n' ? ctl & FANSI_CTL_NL : ctl & FANSI_CTL_C0);
        found_this =
          (x_val == '\n' && (ctl & FANSI_CTL_NL)) ||
          (x_val != '\n' && (ctl & FANSI_CTL_C0));
      }
      if(found_this) {
        x_found_end = x_track;
        if(!found) found = 1;
      }
    }
    if(found && !found_this) break;
  }
  if(found) {
    res = (struct FANSI_csi_pos){
      .start=x_found_start, .len=(x_found_end - x_found_start),
      .valid=valid, .ctl=found_ctl
    };
  } else {
    res = (struct FANSI_csi_pos){
      .start=x, .len=0, .valid=valid, ctl=found_ctl
    };
  }
  return res;
}
/*
 * Allocates a fresh chunk of memory if the existing one is not large enough.
 *
 * We never intend to re-use what's already in memory so we don't realloc.  If
 * allocation is needed the buffer will be either twice as large as it was
 * before, or size `size` if that is greater than twice the size.
 *
 * Does NOT allocate extra byte for NULL!
 */
void FANSI_size_buff(struct FANSI_buff * buff, size_t size) {
  // assumptions check that  SIZE_T fits INT_MAX + 1
  size_t buff_max = (size_t) FANSI_lim.lim_int.max + 1;
  if(size > buff->len) {
    // Special case for intial alloc

    if(!buff->len) {
      if(size < 128 && FANSI_lim.lim_int.max > 128)
        size = 128;  // in theory little penalty to ask this minimum
      else if(size > buff_max) {
        // nocov start
        // too difficult to test, all the code pretty much checks for overflow
        // before requesting memory
        error(
          "Internal Error: requested buff size %zu greater than INT_MAX + 1.",
           size
        );
        // nocov end
      }
      else buff->len = size;
    }
    // More generic case

    if(size > buff->len) {
      size_t tmp_double_size = 0;
      if(buff->len > buff_max - size) {
        tmp_double_size = buff_max;
      } else {
        tmp_double_size = buff->len + buff->len;
      }
      if(size > tmp_double_size) tmp_double_size = size;

      if(tmp_double_size > buff_max)
        // nocov start
        // this can't really happen unless size starts off bigger than
        // INT_MAX + 1
        error(
          "%s  Requesting %zu",
          "Internal Error: max allowed buffer size is INT_MAX + 1.",
           tmp_double_size
        );
        // nocov end
      buff->len = tmp_double_size;
    }
    buff->buff = R_alloc(buff->len, sizeof(char));
  }
}
/*
 * Compute how many digits are in a number
 *
 * Add an extra character for negative integers.
 */

int FANSI_digits_in_int(int x) {
  int num = 1;
  if(x < 0) {
    ++num;
    x = -x;
  }
  while((x = (x / 10))) ++num;
  return num;
}
SEXP FANSI_digits_in_int_ext(SEXP y) {
  if(TYPEOF(y) != INTSXP) error("Internal Error: required int.");

  R_xlen_t ylen = XLENGTH(y);
  SEXP res = PROTECT(allocVector(INTSXP, ylen));

  for(R_xlen_t i = 0; i < ylen; ++i)
    INTEGER(res)[i] = FANSI_digits_in_int(INTEGER(y)[i]);

  UNPROTECT(1);
  return(res);
}
/*
 * Compresses the ctl vector into a single integer by encoding each value of
 * ctl as a bit.
 */

int FANSI_ctl_as_int(SEXP ctl) {
  int ctl_int = 0;
  int flip_bits = 0;
  for(R_xlen_t i = 0; i < XLENGTH(ctl); ++i) {
    // -2 because ctl is 1 indexed (from R), and position 1 means "all", so we
    // need to shift by 1 for the 0 index, and then by one more for the position
    // occupied by "all" that really means flip bits
    int ctl_val = INTEGER(ctl)[i] - 2;
    if(ctl_val > 4)
      error("Internal Error: max ctl value allowed is 4.");
    if(ctl_val < 0) flip_bits = 1;
    else ctl_int |= 1 << ctl_val;
  }
  if(flip_bits) ctl_int ^= FANSI_CTL_ALL;
  return ctl_int;
}
SEXP FANSI_ctl_as_int_ext(SEXP ctl) {
  return ScalarInteger(FANSI_ctl_as_int(ctl));
}
/*
 * Partial match a single string byte by byte
 *
 * @param x a scalar STRSXP
 * @param choices an array of strings to match against
 * @param choice_count how many elements there are in array
 * @param arg_name the name of the argument to use in an error message if
 *   the match fails.
 * @return the position in choices that partial matches x, on a 0-index basis
 *   (ie. 0 == 1st, 1 == 2nd, etc.)
 */
// nocov start
int FANSI_pmatch(
  SEXP x, const char ** choices, int choice_count, const char * arg_name
) {
  error("remove nocov if we start to use this");
  if(TYPEOF(x) != STRSXP || XLENGTH(x) != 1)
    error("Argument `%s` must be a length 1 character vector.", arg_name);

  SEXP x_chrsxp = STRING_ELT(x, 0);
  const char * x_chr = CHAR(x_chrsxp);

  if(!LENGTH(x_chrsxp))
    error("Argument `%s` may not be an empty string.", arg_name);

  int match_count = choice_count;
  int last_match_index = -1;

  for(int i = 0; i < choice_count; ++i) {
    if(!strncmp(x_chr, choices[i], LENGTH(x_chrsxp))) {
      last_match_index = i;
      --match_count;
    }
  }
  if(match_count > 1) {
    error(
      "Argument `%s` matches more than one of the possible choices.",
      arg_name
    );
  } else if(!match_count) {
    error("Argument `%s` does not match any of the valid choices.", arg_name);
  }
  // success

  return last_match_index;
}
// nocov end

// concept borrowed from utf8-lite, but is not great because we're
// still doing the calculation every iteration.  Probably okay though, the
// alternative is just too much of a pain.

void FANSI_interrupt(R_xlen_t i) {if(!(i & 1023)) R_CheckUserInterrupt();}
/*
 * Split an integer vector into two equal size pieces
 */

SEXP FANSI_cleave(SEXP x) {
  if(TYPEOF(x) != INTSXP || XLENGTH(x) % 2)
    error("Internal error, need even length INTSXP.");  // nocov

  R_xlen_t len = XLENGTH(x) / 2;  // R_xlen_t checked to be < SIZE_MAX

  SEXP a, b;
  a = PROTECT(allocVector(INTSXP, len));
  b = PROTECT(allocVector(INTSXP, len));

  // see sort_chr
  size_t size = 0;
  for(int i = 0; i < (int) sizeof(int); ++i) {
    if(size > FANSI_lim.lim_size_t.max - len)
      error("Internal error: vector too long to cleave."); // nocov
    size += len;
  }
  memcpy(INTEGER(a), INTEGER(x), size);
  memcpy(INTEGER(b), INTEGER(x) + len, size);

  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, a);
  SET_VECTOR_ELT(res, 1, b);
  UNPROTECT(3);
  return res;
}
struct datum {int val; R_xlen_t idx;};

static int cmpfun (const void * p, const void * q) {
  struct datum a = *(struct datum *) p;
  struct datum b = *(struct datum *) q;
  return(a.val > b.val ? 1 : (a.val < b.val ? -1 : 0));
}
/*
 * Equivalent to `order`, but less overhead.  May not be faster for longer
 * vectors but since we call it potentially repeatedly via our initial version
 * of strsplit, we want to do this to make somewhat less sub-optimal
 *
 * Does this actually have less overhead now with the radix sort available?
 */
SEXP FANSI_order(SEXP x) {
  if(TYPEOF(x) != INTSXP)
    error("Internal error: this order only supports ints.");  // nocov

  R_xlen_t len = XLENGTH(x);  // R_xlen_t checked to be < SIZE_MAX
  SEXP res;

  if(len) {
    size_t size = 0;
    // See chr_sort
    for(int i = 0; i < (int) sizeof(struct datum); ++i) {
      if(size > FANSI_lim.lim_size_t.max - len)
        error("Internal error: vector too long to order."); // nocov
      size += len;
    }
    struct datum * data = (struct datum *) R_alloc(len, sizeof(struct datum));

    for(R_xlen_t i = 0; i < len; ++i)
      *(data + i) = (struct datum){.val=INTEGER(x)[i], .idx=i + 1};

    qsort(data, (size_t) len, sizeof(struct datum), cmpfun);

    res = PROTECT(allocVector(INTSXP, len));

    for(R_xlen_t i = 0; i < len; ++i) INTEGER(res)[i] = (data + i)->idx;
  } else {
    res = PROTECT(allocVector(INTSXP, 0));
  }
  UNPROTECT(1);
  return res;
}
/*
 * Equivalent to `sort`, but less overhead.  May not be faster for longer
 * vectors but since we call it potentially repeatedly via our initial version
 * of strsplit, we want to do this to make somewhat less sub-optimal
 */
// nocov start
static int cmpfun2 (const void * p, const void * q) {
  int a = *(int *) p;
  int b = *(int *) q;
  return(a > b ? 1 : (a < b ? -1 : 0));
}
SEXP FANSI_sort_int(SEXP x) {
  error("get rid of nocov if we start using");
  if(TYPEOF(x) != INTSXP)
    error("Internal error: this order only supports ints.");  // nocov

  R_xlen_t len = XLENGTH(x);

  SEXP res = PROTECT(duplicate(x));

  qsort(INTEGER(res), (size_t) len, sizeof(int), cmpfun2);

  UNPROTECT(1);
  return res;
}
// nocov end
struct datum2 {SEXP val; R_xlen_t idx;};

static int cmpfun3 (const void * p, const void * q) {
  struct datum2 a = *(struct datum2 *) p;
  struct datum2 b = *(struct datum2 *) q;
  const char * a_chr = CHAR(a.val);
  const char * b_chr = CHAR(b.val);
  return(a_chr > b_chr ? 1 : (a_chr < b_chr ? -1 : 0));
}
/*
 * Sort chars so that equal values are contiguous
 *
 * Beware, the sort is not lexical, instead this is sorted by the memory addess
 * of the character strings backing each CHARSXP.
 *
 * The only purpose of this is to support the unique_chr function.
 */

SEXP FANSI_sort_chr(SEXP x) {
  if(TYPEOF(x) != STRSXP)
    error("Internal error: this sort only supports char vecs.");  // nocov

  R_xlen_t len = XLENGTH(x);  // R_xlen_t checked to be < SIZE_MAX
  SEXP res = x;

  if(len > 2) {
    // Check overflow by adding len as many times as there are bytes in the
    // atomic unit.  We could divide, but there aren't many bytes.

    size_t size = 0;
    for(int i = 0; i < (int) sizeof(struct datum); ++i) {
      if(size > FANSI_lim.lim_size_t.max - len)
        error("Internal error: vector too long to order."); // nocov
      size += len;
    }
    struct datum2 * data = (struct datum2 *) R_alloc(len, sizeof(struct datum2));

    for(R_xlen_t i = 0; i < len; ++i)
      *(data + i) = (struct datum2){.val=STRING_ELT(x, i), .idx=i};

    qsort(data, (size_t) len, sizeof(struct datum2), cmpfun3);

    res = PROTECT(allocVector(STRSXP, len));

    for(R_xlen_t i = 0; i < len; ++i)
      SET_STRING_ELT(res, i, STRING_ELT(x, (data + i)->idx));

    UNPROTECT(1);
  }
  return res;
}
/*
 * So we can use a consistent integer type in printing possibly large indeces.
 *
 * Returns in 1 based indexing, -1 in the unlikely case R_xlen_t == intmax_t.
 */

intmax_t FANSI_ind(R_xlen_t i) {
  intmax_t ind = i >= INTMAX_MAX ? -2 : i; // i == INTMAX_MAX is the issue
  return ind + 1;
}

void FANSI_check_chr_size(char * start, char * end, R_xlen_t i) {
  if(end - start > FANSI_lim.lim_int.max) {
    // Can't get to this point with a string that violates, AFAICT
    // nocov start
    error(
      "Internal Error: %s at index [%jd] (3).",
      "attempting to write string longer than INT_MAX",
      FANSI_ind(i)
    );
    // nocov end
  }
}
/*
 * Check Whether String Would Overflow if Appended To
 *
 * @param cur current length
 * @param extra how many bytes we're looking to append
 */

void FANSI_check_append_err(const char * msg, R_xlen_t i) {
  error(
    "%s will create string longer than INT_MAX at index [%jd]%s",
    msg, FANSI_ind(i), ". Try again with smaller strings."
  );
}
int FANSI_check_append(
  int cur, int extra, const char * msg, R_xlen_t i
) {
  if(cur < 0 || extra < 0)
    error("Internal Error: negative lengths.");  // nocov
  if(cur > FANSI_lim.lim_int.max - extra) FANSI_check_append_err(msg, i);
  return cur + extra;
}
/*
 * Similar to mkCharCE
 *
 * Key differences are that we check for R_len_t overflow.
 *
 * String is assumed to have been checked to be no longer than INT_MAX,
 * excluding the NULL terminator.
 *
 * @param start beginning of string to write
 * @param end of string to write; care should be taken that it is indeed the
 *   same string we're talking about.  This is done so that we can measure the
 *   length of the strings directly as in most "write" scenarios we have a
 *   pointer at the end of the buffer.
 */
SEXP FANSI_mkChar(
  const char * start,
  const char * end,
  cetype_t enc, R_xlen_t i
) {
  if(end < start)
    error("Internal Error: buffer reversed; contact maintainer."); // nocov

  // PTRDIFF_MAX known to be >= INT_MAX (assumptions), and string should not
  // be longer than INT_MAX, so no overflow possible here.
  ptrdiff_t len = end - start;

  if(len > FANSI_lim.lim_R_len_t.max)
    error(
      "%s at index [%jd]."
      "Attempting to create CHARSXP longer than R_LEN_T_MAX",
      FANSI_ind(i)
    );

  // Annoyingly mkCharLenCE accepts int parameter instead of R_len_t, so we need
  // to check that too.

  if(end - start > FANSI_lim.lim_int.max)
    error(
      "%s at index [%jd]."
      "Attempting to create CHARSXP longer than INT_MAX",
      FANSI_ind(i)
    );

  mkCharLenCE(name, len, enc);
}

