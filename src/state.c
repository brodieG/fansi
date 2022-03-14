/*
 * Copyright (C) 2022 Brodie Gaslam
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
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
 */

#include "fansi.h"

/*
 * Create a state structure with everything set to zero
 *
 * We rely on struct initialization to set everything else to zero.
 *
 * FANSI_state_init_full is specifically to handle the allowNA case in nchar,
 * for which we MUST check FANSI_GET_ERR(state.status) after each
 * `FANSI_read_next`.  In all other cases `R_nchar` should be set to not
 * `allowNA`.
 */
struct FANSI_state FANSI_state_init_full(
  SEXP strsxp, SEXP warn, SEXP term_cap, SEXP allowNA, SEXP keepNA,
  SEXP width, SEXP ctl, R_xlen_t i
) {
  // nocov start
  if(TYPEOF(strsxp) != STRSXP) {
    error(
      "Internal error: state_init with bad type for strsxp (%s)",
      type2char(TYPEOF(strsxp))
    );
  }
  if(i < 0 || i > XLENGTH(strsxp))
    error(
      "Internal error: state_init with out of bounds index [%jd] for strsxp.",
      FANSI_ind(i)
    );
  SEXP chrsxp = STRING_ELT(strsxp, i);
  FANSI_check_chrsxp(chrsxp, i);
  const char * string = CHAR(chrsxp);

  // Validation not complete here, many of these should be scalar, rely on R
  // level checks.
  if(TYPEOF(term_cap) != INTSXP)
    error(
      "Internal error: state_init with bad type for term_cap (%s)",
      type2char(TYPEOF(term_cap))
    );
  if(TYPEOF(allowNA) != LGLSXP)
    error(
      "Internal error: state_init with bad type for allowNA (%s)",
      type2char(TYPEOF(allowNA))
    );
  if(TYPEOF(keepNA) != LGLSXP)
    error(
      "Internal error: state_init with bad type for keepNA (%s)",
      type2char(TYPEOF(keepNA))
    );
  if(TYPEOF(width) != INTSXP)
    error(
      "Internal error: state_init with bad type for width (%s)",
      type2char(TYPEOF(width))
    );
  if(TYPEOF(ctl) != INTSXP)
    error(
      "Internal error: state_init with bad type for ctl (%s)",
      type2char(TYPEOF(ctl))
    );
  if(TYPEOF(warn) != INTSXP || XLENGTH(warn) != 1L)
    error(
      "Internal error: state_init with bad (%s) type or length (%jd) for warn.",
      type2char(TYPEOF(warn)), XLENGTH(warn)
    );
  int warn_int = asInteger(warn);
  if((unsigned int) warn_int & ~WARN_MASK)
    error(
      "Internal error: state_init with OOB value for warn (%d)",
      warn_int
    );
  // nocov end

  unsigned int settings = 0;
  unsigned int term_cap_raw = FANSI_term_cap_as_int(term_cap);
  // slightly hacky, the "old term capability" mode is crammed into the raw term
  // cap integer, and needs to be extracted out.
  if(term_cap_raw > TERM_ALL) settings |= SET_TERMOLD;
  settings = FANSI_SET_RNG(
    settings, SET_TERMCAP, TERM_ALL, term_cap_raw & TERM_ALL
  );
  settings = FANSI_SET_RNG(
    settings, SET_WIDTH, COUNT_ALL, asInteger(width)
  );
  settings = FANSI_SET_RNG(
    settings, SET_CTL, CTL_ALL, FANSI_ctl_as_int(ctl)
  );
  settings |= asLogical(allowNA) ? SET_ALLOWNA : 0;
  settings |= asLogical(keepNA) ? SET_KEEPNA : 0;
  settings |= (unsigned int) warn_int;

  // All others struct-inited to zero.
  return (struct FANSI_state) {
    .string = string,
    .settings = settings
  };
}
/*
 * Re-initialize state
 *
 * Reduce overhead from revalidating params that are recycled across vector
 * elements from a single external function call.
 */
void FANSI_state_reinit(
  struct FANSI_state * state, SEXP x, R_xlen_t i
) {
  if(i < 0 || i >= XLENGTH(x))
    // nocov start
    error(
      "Internal error: state_init with out of bounds index [%jd] for strsxp.",
      FANSI_ind(i)
    );
    // nocov end
  SEXP chrsxp = STRING_ELT(x, i);
  FANSI_check_chrsxp(chrsxp, i);
  const char * string = CHAR(chrsxp);
  state->string = string;
  FANSI_reset_state(state);
}
// When we don't care about R_nchar width, but do care about CSI / SGR (which
// means, we only really care about SGR since all CSI does is affect width calc).

struct FANSI_state FANSI_state_init(
  SEXP strsxp, SEXP warn, SEXP term_cap, R_xlen_t i
) {
  int prt = 0;
  SEXP R_false = PROTECT(ScalarLogical(0)); ++prt;
  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  SEXP R_zero = PROTECT(ScalarInteger(0)); ++prt;
  SEXP R_one = PROTECT(ScalarInteger(1)); ++prt;
  struct FANSI_state res = FANSI_state_init_full(
    strsxp, warn, term_cap,
    R_true,  // allowNA for invalid multibyte
    R_false, // keepNA
    R_zero,  // Don't use width by default
    R_one,   // Treat all escapes as special by default (wrong prior to v1.0)
    i
  );
  UNPROTECT(prt);
  return res;
}
// We only care to specify ctl;
// means, we only really care about SGR since all CSI does is affect width calc).

struct FANSI_state FANSI_state_init_ctl(
  SEXP strsxp, SEXP warn, SEXP ctl, R_xlen_t i
) {
  int prt = 0;
  SEXP R_false = PROTECT(ScalarLogical(0)); ++prt;
  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  SEXP R_zero = PROTECT(ScalarInteger(0)); ++prt;
  SEXP R_one = PROTECT(ScalarInteger(1)); ++prt;
  struct FANSI_state res = FANSI_state_init_full(
    strsxp, warn,
    R_one,   // all term_cap
    R_true,  // allowNA for invalid multibyte
    R_false, // keepNA
    R_zero,  // Don't use width by default
    ctl,     // Which sequences are recognized
    i
  );
  UNPROTECT(prt);
  return res;
}

void FANSI_reset_width(struct FANSI_state * state) {
  state->pos.w = 0;
}
/*
 * Reset the position counters
 *
 * Intended so that all the state info is kept for when state persists from one
 * element in a character vector to the next.
 *
 * We are not 100% sure we're resetting everything that needs to be reset.
 *
 * See also FANSI_state_reinit
 */
void FANSI_reset_pos(struct FANSI_state * state) {
  state->pos = (struct FANSI_position){0};
  unsigned int warned = state->status & STAT_WARNED;
  state->status = 0U;
  if(warned) state->status |= STAT_WARNED;
}
/*
 * Reset state without changing index/string
 *
 * This one probably doesn't benefit much of by-ref, but doing so for
 * consistency with others.
 */
void FANSI_reset_state(struct FANSI_state * state) {
  state->fmt = (struct FANSI_format){0};
  state->pos = (struct FANSI_position){0};
  state->status = 0U;
  state->utf8 = 0;
}
/*
 * Generate the tag corresponding to the state and write it out as a NULL
 * terminated string.
 *
 * @return the byte after the last one written, typically set to zero.  For the
 *   start of the string: buff->buff0
 */
char * FANSI_state_as_chr(
  struct FANSI_buff *buff, struct FANSI_state state, int normalize, R_xlen_t i
) {
  FANSI_reset_buff(buff);
  FANSI_W_sgr(buff, state.fmt.sgr, normalize, 1, i);
  FANSI_W_url(buff, state.fmt.url, i);

  FANSI_size_buff(buff);
  FANSI_W_sgr(buff, state.fmt.sgr, normalize, 1, i);
  FANSI_W_url(buff, state.fmt.url, i);
  return buff->buff;
}
/*
 * Determine whether two state structs have same color
 */
static int sgr_comp_color(
  struct FANSI_color target, struct FANSI_color current
) {
  unsigned char tclr = target.x;
  unsigned char cclr = current.x;
  int c256 = tclr & (CLR_256 | CLR_TRU);
  int cTRU = tclr & CLR_TRU;
  return
    tclr != cclr  ||
    // Can't use memcmp because we don't necessarly cleanup extra
    (c256 && target.extra[0] != current.extra[0]) ||
    (cTRU && target.extra[1] != current.extra[1]) ||
    (cTRU && target.extra[2] != current.extra[2]);
}
int FANSI_sgr_comp_color(
  struct FANSI_sgr target, struct FANSI_sgr current
) {
  return
    sgr_comp_color(target.color, current.color) ||
    sgr_comp_color(target.bgcol, current.bgcol);
}
/*
 * Create a new SGR that has all the styles in `old` missing from `new`.
 *
 * This is so that we can then generate the closing SGRs required to transition
 * from one state to the other (used for diff).
 *
 * @param mode 0 to explicitly close/open styles that will be overriden (e.g.
 *   color), and 1 to do so implicitly
 */
struct FANSI_sgr FANSI_sgr_setdiff(
  struct FANSI_sgr old, struct FANSI_sgr new, int mode
) {
  struct FANSI_sgr res = (struct FANSI_sgr){0};
  if(
    (!mode && sgr_comp_color(old.color, new.color)) ||
    (mode && old.color.x && !new.color.x)
  ) {
    res.color.x = old.color.x;
    memcpy(res.color.extra, old.color.extra, sizeof(old.color.extra));
  }
  if(
    (!mode && sgr_comp_color(old.bgcol, new.bgcol)) ||
    (mode && old.bgcol.x && !new.bgcol.x)
  ) {
    res.bgcol.x = old.bgcol.x;
    memcpy(res.bgcol.extra, old.bgcol.extra, sizeof(old.bgcol.extra));
  }
  // We don't bother to shift the fonts here since both are already encoded
  unsigned int font_old, font_new;
  font_old = old.style & FONT_MASK;
  font_new = new.style & FONT_MASK;
  if(
    (!mode && (font_old != font_new)) || (mode && font_old && !font_new)
  )
    res.style = (res.style & ~FONT_MASK) | font_old;

  // All non font styles are just bit flags
  unsigned int style_old, style_new;
  style_old = old.style & ~FONT_MASK;
  style_new = new.style & ~FONT_MASK;
  res.style |= style_old & ~style_new;
  return res;
}
/*
 */
struct FANSI_sgr FANSI_sgr_intersect(
  struct FANSI_sgr old, struct FANSI_sgr new
) {
  struct FANSI_sgr res = {0};
  if(old.color.x == new.color.x) {
    res.color.x = new.color.x;
    memcpy(res.color.extra, new.color.extra, sizeof(new.color.extra));
  }
  if(old.bgcol.x == new.bgcol.x) {
    res.bgcol.x = new.bgcol.x;
    memcpy(res.bgcol.extra, new.bgcol.extra, sizeof(new.bgcol.extra));
  }
  unsigned int font_old, font_new;
  font_old = old.style & FONT_MASK;
  font_new = new.style & FONT_MASK;
  if(font_old == font_new) res.style &= font_new | ~FONT_MASK;

  // All non font styles are just bit flags
  unsigned int style_old, style_new;
  style_old = old.style & ~FONT_MASK;
  style_new = new.style & ~FONT_MASK;
  res.style |= style_old & style_new;
  return res;
}

// Keep synchronized with `sgr_close`
int FANSI_sgr_active(struct FANSI_sgr sgr) {
  return sgr.style || sgr.color.x || sgr.bgcol.x;
}
// Keep synchronized with `url_close`
int FANSI_url_active(struct FANSI_url url) {
  return URL_LEN(url) > 0;
}
// Return 0 if equal, 1 if different
//
// As per spec only the same if both url and id are the same, but iterm2 doesn't
// even seem to respect that (i.e. two urls that meet requirement aren't
// simultaneously highlighted on hover, at least as of 3.4.7beta2.
//
// Note, id must be the same, unless there is no URL in which case id may be
// empty.

int FANSI_url_comp(struct FANSI_url target, struct FANSI_url current) {
  int url_eq = target.url.len == current.url.len &&
    (
      !URL_LEN(target) ||
      !memcmp(URL_STRING(target), URL_STRING(current), URL_LEN(target))
    );
  int id_eq = ID_LEN(target) == ID_LEN(current) &&
    (
      (!URL_LEN(target) && !ID_LEN(target)) ||
      (ID_LEN(target) &&
        !memcmp(ID_STRING(target), ID_STRING(current), ID_LEN(target))
      )
    );

  return !(url_eq && id_eq);
}
/*
 * For closing things for substr, so we don't need to automatically normalize
 * every string if we just close with ESC[0m.
 *
 * Pretty inefficient to do it this way...
 *
 * @param x should be a vector of active states at end of strings.
 */
SEXP FANSI_state_close_ext(SEXP x, SEXP warn, SEXP term_cap, SEXP norm) {

  if(TYPEOF(x) != STRSXP)
    error("Argument `x` should be a character vector.");  // nocov
  if(TYPEOF(norm) != LGLSXP || XLENGTH(norm) != 1)
    error("Argument `normalize` should be TRUE or FALSE.");  // nocov

  const char * arg = "x";
  int prt = 0;
  R_xlen_t len = xlength(x);
  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;

  PROTECT_INDEX ipx;
  // reserve spot if we need to alloc later
  PROTECT_WITH_INDEX(res, &ipx); ++prt;

  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);
  int normalize = asInteger(norm);

  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  SEXP R_one = PROTECT(ScalarInteger(1)); ++prt;
  SEXP R_zero = PROTECT(ScalarInteger(0)); ++prt;
  struct FANSI_state state;

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    if(!i) {
      state = FANSI_state_init_full(
        x, warn, term_cap, R_true, R_true, R_zero, R_one, i
      );
    } else FANSI_state_reinit(&state, x, i);

    SEXP x_chr = STRING_ELT(x, i);
    if(x_chr == NA_STRING || !LENGTH(x_chr)) continue;

    FANSI_read_all(&state, i, arg);
    FANSI_reset_buff(&buff);
    FANSI_W_close(&buff, state.fmt, normalize, i);

    if(buff.len) {
      if(res == x) REPROTECT(res = duplicate(x), ipx);
      FANSI_size_buff(&buff);
      FANSI_W_close(&buff, state.fmt, normalize, i);

      cetype_t chr_type = getCharCE(x_chr);
      SEXP reschr = PROTECT(FANSI_mkChar(buff, chr_type, i));
      SET_STRING_ELT(res, i, reschr);
      UNPROTECT(1);
    }
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}

