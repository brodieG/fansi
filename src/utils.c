/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 or 3 of the License.
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

#define LIM_INIT (struct FANSI_limits) {                       \
  .lim_int={.name="INT", .min=INT_MIN, .max=INT_MAX},          \
  .lim_R_len_t={.name="R_LEN_T", .min=0, .max=R_LEN_T_MAX},    \
  .lim_R_xlen_t={.name="R_XLEN_T", .min=0, .max=R_XLEN_T_MAX}, \
  .lim_size_t={.name="SIZE", .min=0, .max=SIZE_MAX}            \
}
// See also check_limits in assumptions.c
struct FANSI_limits FANSI_lim = LIM_INIT;

SEXP FANSI_set_int_max(SEXP x) {
  if(TYPEOF(x) != INTSXP || XLENGTH(x) != 1)
    error("invalid int_max value");  // nocov
  int x_int = asInteger(x);

  if(x_int < 1)
    error("int_max value must be positive"); // nocov

  int old_int = FANSI_lim.lim_int.max;
  FANSI_lim.lim_int.max = x_int;
  return ScalarInteger(old_int);
}
SEXP FANSI_set_rlent_max(SEXP x) {
  if(TYPEOF(x) != INTSXP || XLENGTH(x) != 1)
    error("invalid R_len_t_max value");  // nocov
  int x_R_len_t = asInteger(x);

  if(x_R_len_t < 1)
    error("R_len_t_max value must be positive"); // nocov

  int old_R_len_t = FANSI_lim.lim_R_len_t.max;
  FANSI_lim.lim_R_len_t.max = (intmax_t) x_R_len_t;
  return ScalarInteger(old_R_len_t);
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
 * Seek a state up until a known control.
 *
 * Beware that the position offsets other than .x will be incorrect.
 */
int FANSI_find_ctl(
  struct FANSI_state *state, R_xlen_t i, const char * arg
) {
  int pos = state->pos.x;
  while(state->string[state->pos.x]) {
    pos = state->pos.x += FANSI_seek_ctl(state->string + state->pos.x);
    FANSI_read_next(state, i, arg);
    // Known control read
    if(state->status & FANSI_CTL_MASK) {
      break;
  } }
  return pos;
}
static int FANSI_maybe_ctl(const char x) {
  // Controls range from 0000 0001 (0x01) to 0001 1111 (0x1F), plus 0x7F;
  // We don't treat C1 controls as specials, apparently
  return x && (!(x & (~0x1F)) || x == 0x7F);
}
/*
 * Searches for start of next possible control character, if there is one,
 * and returns the offset from the current point
 */

int FANSI_seek_ctl(const char * x) {
  const char * x0 = x;
  while(*x && !FANSI_maybe_ctl(*x)) x++;
  if(x - x0 > FANSI_lim.lim_int.max)
    error("Internal error: sought past INT_MAX, should not happen.");  // nocov
  return (x - x0);
}
/*
 * Compresses the ctl vector into a single integer by encoding each value of
 * ctl as a bit.
 */

unsigned int FANSI_ctl_as_int(SEXP ctl) {
  int ctl_int = 0;
  int flip_bits = 0;
  for(R_xlen_t i = 0; i < XLENGTH(ctl); ++i) {
    // -2 because ctl is 1 indexed (from R), and position 1 means "all", so we
    // need to shift by 1 for the 0 index, and then by one more for the position
    // occupied by "all" that really means flip bits
    int ctl_val = INTEGER(ctl)[i] - 2;
    if(ctl_val > 6)
      error("Internal Error: max ctl value allowed is 6.");
    if(ctl_val < 0) flip_bits = 1;
    else ctl_int |= 1U << ctl_val;
  }
  if(flip_bits) ctl_int ^= FANSI_CTL_ALL;
  return ctl_int;
}
SEXP FANSI_ctl_as_int_ext(SEXP ctl) {
  return ScalarInteger(FANSI_ctl_as_int(ctl));
}
// See ctl_as_int for explanation

int FANSI_term_cap_as_int(SEXP term_cap) {
  int term_cap_int = 0;
  int flip_bits = 0;
  for(R_xlen_t i = 0; i < XLENGTH(term_cap); ++i) {
    int term_cap_val = INTEGER(term_cap)[i] - 2;
    if(term_cap_val > 2)
      error("Internal Error: max term_cap value allowed is 2."); // nocov
    if(term_cap_val < 0) flip_bits = 1;
    else term_cap_int |= 1 << term_cap_val;
  }
  if(flip_bits) term_cap_int ^= FANSI_TERM_ALL;
  return term_cap_int;
}

SEXP FANSI_get_warn_all() {
  return ScalarInteger(FANSI_WARN_MASK);
}
SEXP FANSI_get_warn_mangled() {
  return ScalarInteger(FANSI_WARN_MANGLED);
}
SEXP FANSI_get_warn_badbyte() {
  return ScalarInteger(FANSI_WARN_BADBYTE);
}
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
SEXP FANSI_sort_chr_ext(SEXP x) {
  return FANSI_sort_chr(x);
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
static SEXP mkChar_core(
  struct FANSI_buff buff, cetype_t enc, R_xlen_t i, int strict
) {
  FANSI_check_buff(buff, i, strict);

  // PTRDIFF_MAX known to be >= INT_MAX (assumptions), and string should not
  // be longer than INT_MAX, so no overflow possible here.

  if(buff.len > FANSI_lim.lim_R_len_t.max) {
    error(
      "%s at index [%jd].",
      "Attempting to create CHARSXP longer than R_LEN_T_MAX",
      FANSI_ind(i)
    );
  }

  // Annoyingly mkCharLenCE accepts int parameter instead of R_len_t, so we need
  // to check that too.
  if(buff.len > FANSI_lim.lim_int.max)
    error(
      "%s at index [%jd].",
      "Attempting to create CHARSXP longer than INT_MAX",
      FANSI_ind(i)
    );

  return mkCharLenCE(buff.buff0, buff.len, enc);

}
// Original mkChar taking star and end points

SEXP FANSI_mkChar0(
  char * start, char * end, cetype_t enc, R_xlen_t i
) {
  // dummy buff
  struct FANSI_buff buff = {.buff0=start, .buff=end, .len=end - start};
  return mkChar_core(buff, enc, i, 0);
}
SEXP FANSI_mkChar(struct FANSI_buff buff, cetype_t enc, R_xlen_t i) {
  return mkChar_core(buff, enc, i, 1);
}
// return 1 if is TRUE/FALSE
int FANSI_is_tf(SEXP x) {
  return TYPEOF(x) == LGLSXP && XLENGTH(x) == 1 &&
    LOGICAL(x)[0] != NA_LOGICAL;
}
/*
 * Basic validation on common arguments
 *
 * Note FANSI_state_init_full also validates many of the common args
 */
void FANSI_val_args(SEXP x, SEXP norm, SEXP carry) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` must be character.");     // nocov
  if(TYPEOF(carry) != STRSXP || XLENGTH(carry) != 1L)
    error("Argument `carry` must be scalar character.");         // nocov
  if(!FANSI_is_tf(norm))
    error("Argument `norm` must be TRUE or FALSE.");  // nocov
}
// Utilitiy fun
// nocov start

void FANSI_print(const char * x) {
  if(x) {
    size_t len = strlen(x);
    for(size_t i = 0; i < len; ++i)
      if(*(x + i) < 0x20 || *(x + i) > 0x7F)
        Rprintf("\\x%2x", *(x + i));
      else
        Rprintf("%c", *(x + i));
    Rprintf("\n");
  }
}
void FANSI_print_len(const char * x, int len) {
  for(int i = 0; i < len; ++i)
    if(*(x + i) < 0x20 || *(x + i) > 0x7F)
      Rprintf("\\x%2x", *(x + i));
    else
      Rprintf("%c", *(x + i));
  Rprintf("\n");
}
static void print_bits(unsigned int x) {
  unsigned int uintbits = (sizeof(x) * CHAR_BIT);
  for(unsigned int i = uintbits; i > 0; --i) {
    if(i < uintbits && !(i % 8)) Rprintf(" ");
    Rprintf("%d", (x & (1U << (i - 1))) > 0);
  }
}
void FANSI_print_sgr(struct FANSI_sgr s) {
  Rprintf(
    "  color:  %d %d %d;%d;%d bgcolor:  %d %d %d;%d;%d\n",
    s.color.x & FANSI_CLR_MASK,
    s.color.x & ~FANSI_CLR_MASK, 
    s.color.extra[0], s.color.extra[1], s.color.extra[2],
    s.bgcol.x & FANSI_CLR_MASK,
    s.bgcol.x & ~FANSI_CLR_MASK, 
    s.bgcol.extra[0], s.bgcol.extra[1], s.bgcol.extra[2]
  );
  Rprintf("  style:  ");
  print_bits(s.style);
  Rprintf("\n");
}
void FANSI_print_state(struct FANSI_state x) {
  Rprintf("- State -------\n");
  FANSI_print_sgr(x.fmt.sgr);
  Rprintf(
    "  pos: b %d r %d a %d w %d\n",
    x.pos.x, x.pos.r, x.pos.a, x.pos.a
  );
  Rprintf("  status: ");
  print_bits(x.status);
  Rprintf("\n  settng: ");
  print_bits(x.settings);
  Rprintf("\n- End State ---\n");
}

SEXP FANSI_read_all(SEXP x, SEXP warn, SEXP term_cap) {
  R_xlen_t len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(INTSXP, len));
  int * res_i = INTEGER(res);
  const char * arg = "x";
  struct FANSI_state state;
  for(R_xlen_t i = 0; i < len; ++i) {
    if(!i) state = FANSI_state_init(x, warn, term_cap, i);
    else FANSI_state_reinit(&state, x,  i);

    while(state.string[state.pos.x]) FANSI_read_next(&state, i, arg);
    res_i[i] = state.pos.x;
  }
  UNPROTECT(1);
  return res;
}

