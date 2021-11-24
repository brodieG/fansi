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
 * Create a state structure with everything set to zero
 *
 * We rely on struct initialization to set everything else to zero.
 *
 * FANSI_state_init_full is specifically to handle the allowNA case in nchar,
 * for which we MUST check `state.err_code == 9` after each `FANSI_read_next`.
 * In all other cases `R_nchar` should be set to not `allowNA`.
 */
struct FANSI_state FANSI_state_init_full(
  SEXP strsxp, SEXP warn, SEXP term_cap, SEXP allowNA, SEXP keepNA,
  SEXP width, SEXP ctl, R_xlen_t i, const char * arg
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
  if(warn_int < 0 || warn_int > FANSI_WARN_ALL)
    error(
      "Internal error: state_init with OOB value for warn (%d)",
      warn_int
    );
  // nocov end

  // All others struct-inited to zero.
  return (struct FANSI_state) {
    .sgr = (struct FANSI_sgr) {.color = -1, .bg_color = -1},
    .sgr_prev = (struct FANSI_sgr) {.color = -1, .bg_color = -1},
    .string = string,
    .warn = (unsigned int) warn_int,
    .term_cap = FANSI_term_cap_as_int(term_cap),
    .allowNA = asLogical(allowNA),
    .keepNA = asLogical(keepNA),
    .width_mode = asInteger(width),
    .ctl = FANSI_ctl_as_int(ctl),
    .arg = arg
  };
}
/*
 * Re-initialize state
 *
 * Reduce overhead from revalidating params that are recycled across vector
 * elements from a single external function call.
 */
struct FANSI_state FANSI_state_reinit(
  struct FANSI_state state, SEXP x, R_xlen_t i
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

  state.string = string;
  return FANSI_reset_state(state);
}
// When we don't care about R_nchar width, but do care about CSI / SGR (which
// means, we only really care about SGR since all CSI does is affect width calc).

struct FANSI_state FANSI_state_init(
  SEXP strsxp, SEXP warn, SEXP term_cap, R_xlen_t i, const char * arg
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
    i,
    arg
  );
  UNPROTECT(prt);
  return res;
}
// We only care to specify ctl;
// means, we only really care about SGR since all CSI does is affect width calc).

struct FANSI_state FANSI_state_init_ctl(
  SEXP strsxp, SEXP warn, SEXP ctl, R_xlen_t i, const char * arg
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
    i,
    arg
  );
  UNPROTECT(prt);
  return res;
}

struct FANSI_state FANSI_reset_width(struct FANSI_state state) {
  state.pos_width = 0;
  return state;
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
struct FANSI_state FANSI_reset_pos(struct FANSI_state state) {
  state.pos_byte = 0;
  state.pos_ansi = 0;
  state.pos_raw = 0;
  state.pos_width = 0;
  state.last_special = 0;
  state.non_normalized = 0;
  return state;
}
/*
 * Reset state without changing index/string
 */
struct FANSI_state FANSI_reset_state(struct FANSI_state state) {
  struct FANSI_state state_reinit;
  state_reinit = (struct FANSI_state) {
    .string = state.string,
    .warn = state.warn,
    .term_cap = state.term_cap,
    .allowNA = state.allowNA,
    .keepNA = state.keepNA,
    .width_mode = state.width_mode,
    .ctl = state.ctl,
    .arg = state.arg
  };
  state_reinit.sgr = (struct FANSI_sgr) {.color = -1, .bg_color = -1};
  state_reinit.sgr_prev = (struct FANSI_sgr) {.color = -1, .bg_color = -1};
  return state_reinit;
}
/*
 * Compute the state given a character position (raw position)
 *
 * This function is designed to be called iteratively on the same string with
 * monotonically increasing values of `pos`.  This allows us to compute state at
 * multiple positions while re-using the work we did on the earlier positions.
 *
 * The algorithm seeks to read past the requested point and rewind back to the
 * best break point once it's clear we're past the requested point.
 *
 * The same code used for start and stop positions, with the concept that zero
 * width chacters should always be consumed (i.e. excluded from start, included
 * in end), except that in the special case that an escape sequence will cause a
 * break if we're in "terminate" mode at the end of a string.
 *
 * Overshoot relates to the R-level `round` parameter, but it has diffrent
 * meanings for start and end.  For `start`, to overshoot means to exclude the
 * character from the final substring.  For `end`, it means to include it.  This
 * makes sense if you consider a substring '123' from 'oo123oo'.  If we are
 * targeting the start of '123' and overshoot, we end up with '23oo'.  If we are
 * targeting the end and overshoot we end up with 'oo123o'.
 *
 * One oddity is that Unicode "controls" (which AFAICT includes the ESC) are
 * supposed to create break points for graphemes, but both OS X term and iterm2
 * ignore this (Big Sur c.a. 6/2021).  So we don't break at them either.  Tested
 * this with CSI SGR and regular controls like \a.
 *
 * @param pos the minimum number of characters or width units, to include
 *   (excluding SGR and similar).
 * @param is_start is this for the beginning of a substring?  We tried to avoid
 *   needing this parameter but in the end needed it to avoid retaining SGR that
 *   would be immediately closed.
 * @param state the last state that was known not to exceed prior target.
 * @param int type whether to use character (0), width (1), or byte (2) when
 *   computing the position (looks like we don't use 2?)
 * @param overshoot whether a partially started width unit should be kept (only
 *   meaningful in type = 1 (width) mode.  See details.
 */

static struct FANSI_state_pair state_at_pos2(
  int pos, struct FANSI_state state, int type, int is_start,
  int overshoot, int terminate, R_xlen_t i
) {
  switch(type) {
    case FANSI_COUNT_CHARS:
    case FANSI_COUNT_GRAPH:
    case FANSI_COUNT_WIDTH: break;
    default: error("Internal Error: invalid type (%d).", type);  // nocov
  }
  struct FANSI_state state_res, state_restart;
  state_res = state_restart = state;
  int pos_new, pos_restart;
  int pos_ini = pos;
  int os = overshoot;
  pos_new = pos_restart = type ? state.pos_width : state.pos_raw;

  // Read until we are sure we've passed our threshold, and include all trailing
  // zero width items (with one exception).
  while(
    (pos_new <= pos || pos_new == pos_restart) && state.string[state.pos_byte]
  ) {
    pos_restart = pos_new;
    state = FANSI_read_next(state, i, 1);
    pos_new = type ? state.pos_width : state.pos_raw;

    // Last spot that's safe to restart from either as start or stop
    // Needed b/c starts read trailing SGR but stops don't (in terminate mode)
    if(pos_new > pos_restart && pos_new <= pos_ini) state_restart = state;

    // overshoot gives us one chance to extend what we're looking for
    if(pos_restart < pos && pos_new > pos && os) {
      os = 0;
      pos = pos_new;
    }
    // Set an anchor point to rewind to last read item, except if a trailing
    // SGR in terminate mode, as that would be immediately closed.
    if(!(state.last_special && !is_start && terminate)) {
      if(pos_new <= pos) state_res = state;
    }
  }
  // Avoid potential double warning next time we read
  state_restart.warned = state.warned = state.warned;

  return (struct FANSI_state_pair){.cur=state_res, .restart=state_restart};
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
  FANSI_W_sgr(buff, state.sgr, normalize, 1, i);
  if(state.url.url.len) FANSI_W_url(buff, state.url, normalize, i);

  FANSI_size_buff(buff);
  FANSI_W_sgr(buff, state.sgr, normalize, 1, i);
  if(state.url.url.len) FANSI_W_url(buff, state.url, normalize, i);
  return buff->buff;
}

/*
 * Determine whether two state structs have same style
 *
 * This only compares the style pieces (i.e. not the position pieces)
 *
 * Returns 1 if the are different, 0 if they are equal.
 *
 * _basic is used just for the 1-9 SGR codes plus colors.
 */
int FANSI_sgr_comp_color(
  struct FANSI_sgr target, struct FANSI_sgr current
) {
  return !(
    target.color == current.color &&
    target.bg_color == current.bg_color &&
    target.color_extra[0] == current.color_extra[0] &&
    target.bg_color_extra[0] == current.bg_color_extra[0] &&
    target.color_extra[1] == current.color_extra[1] &&
    target.bg_color_extra[1] == current.bg_color_extra[1] &&
    target.color_extra[2] == current.color_extra[2] &&
    target.bg_color_extra[2] == current.bg_color_extra[2] &&
    target.color_extra[3] == current.color_extra[3] &&
    target.bg_color_extra[3] == current.bg_color_extra[3]
  );
}
static int FANSI_sgr_comp_basic(
  struct FANSI_sgr target, struct FANSI_sgr current
) {
  // 1023 is '11 1111 1111' in binary, so this will grab the last ten bits
  // of the styles which are the 1-9 styles
  return FANSI_sgr_comp_color(target, current) ||
    (target.style & 1023) != (current.style & 1023);
}
static int FANSI_sgr_comp(struct FANSI_sgr target, struct FANSI_sgr current) {
  return !(
    !FANSI_sgr_comp_basic(target, current) &&
    target.style == current.style &&
    target.border == current.border &&
    target.font == current.font &&
    target.ideogram == current.ideogram
  );
}
/*
 * Create a new SGR that has all the styles in `old` missing from `new`.
 *
 * This is so that we can then generate the closing SGRs required to transition
 * from one state to the other (used for diff).
 *
 * @param mode 0 to explicitly close/open styles that will be overriden (e.g.
 *   color), and 1 to do so implicityly
 */
struct FANSI_sgr FANSI_sgr_setdiff(
  struct FANSI_sgr old, struct FANSI_sgr new, int mode
) {
  struct FANSI_sgr res = {
    .color=-1, .bg_color=-1, .style=0, .border=0, .ideogram=0, .font=0
  };
  if(
    (!mode && old.color != new.color) ||
    (mode && old.color > -1 && new.color == -1)
  ) {
    res.color = old.color;
    memcpy(res.color_extra, old.color_extra, sizeof(old.color_extra));
  }
  if(
    (!mode && old.bg_color != new.bg_color) ||
    (mode && old.bg_color > -1 && new.bg_color == -1)
  ) {
    res.bg_color = old.bg_color;
    memcpy(res.bg_color_extra, old.bg_color_extra, sizeof(old.bg_color_extra));
  }
  if(
    (!mode && old.font != new.font) ||
    (mode && old.font && !new.font)
  ) {
    res.font = old.font;
  }
  res.style = old.style & (~new.style);
  res.border = old.border & (~new.border);
  res.ideogram = old.ideogram & (~new.ideogram);
  return res;
}
/*
 */
struct FANSI_sgr FANSI_sgr_intersect(
  struct FANSI_sgr old, struct FANSI_sgr new
) {
  struct FANSI_sgr res = {
    .color=-1, .bg_color=-1, .style=0, .border=0, .ideogram=0, .font=0
  };
  if(old.color == new.color) {
    res.color = new.color;
    memcpy(res.color_extra, new.color_extra, sizeof(new.color_extra));
  }
  if(old.bg_color == new.bg_color) {
    res.bg_color = new.bg_color;
    memcpy(res.bg_color_extra, new.bg_color_extra, sizeof(new.bg_color_extra));
  }
  if(old.font == new.font) res.font = new.font;
  res.style = old.style & new.style;
  res.border = old.border & new.border;
  res.ideogram = old.ideogram & new.ideogram;
  return res;
}

// Keep synchronized with `sgr_close`
int FANSI_sgr_active(struct FANSI_sgr sgr) {
  return
    sgr.style || sgr.color >= 0 || sgr.bg_color >= 0 ||
    sgr.font || sgr.border || sgr.ideogram;
}
// Keep synchronized with `url_close`
int FANSI_url_active(struct FANSI_url url) {
  return url.url.len > 0;
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
      !target.url.len ||
      !memcmp(target.url.val, current.url.val, target.url.len)
    );
  int id_eq = target.id.len == current.id.len &&
    (
      (!target.url.len && !target.id.len) ||
      (target.id.len && !memcmp(target.id.val, current.id.val, target.id.len))
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
        x, warn, term_cap, R_true, R_true, R_zero, R_one, i, "x"
      );
    } else {
      state = FANSI_state_reinit(state, x, i);
    }
    SEXP x_chr = STRING_ELT(x, i);
    if(x_chr == NA_STRING || !LENGTH(x_chr)) continue;

    while(*(state.string + state.pos_byte)) {
      state = FANSI_read_next(state, i, 1);
    }
    FANSI_reset_buff(&buff);
    FANSI_W_sgr_close(&buff, state.sgr, normalize, i);
    FANSI_W_url_close(&buff, state.url, i);

    if(buff.len) {
      if(res == x) REPROTECT(res = duplicate(x), ipx);
      FANSI_size_buff(&buff);
      FANSI_W_sgr_close(&buff, state.sgr, normalize, i);
      FANSI_W_url_close(&buff, state.url, i);

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

/*
 * R interface for state_at_position
 *
 * See state_at_pos2.
 *
 * No carry param, that should be R-level.
 */

SEXP FANSI_state_at_pos_ext(
  SEXP x, SEXP pos, SEXP type,
  SEXP overshoot, SEXP is_start, SEXP warn,
  SEXP term_cap, SEXP ctl, SEXP norm,
  SEXP terminate, SEXP ids
) {
  /*******************************************\
  * IMPORTANT: INPUT MUST ALREADY BE IN UTF8! *
  \*******************************************/

  // errors should be handled R side, but just in case
  if(XLENGTH(x) != 1 || STRING_ELT(x, 0) == NA_STRING)
    error("Argument `x` must be scalar character and not be NA.");   // nocov
  if(
      TYPEOF(norm) != LGLSXP || XLENGTH(norm) != 1 ||
      asLogical(norm) == NA_LOGICAL
    )
    error("Argument `normalize` must be TRUE or FALSE.");      // nocov
  if(
      TYPEOF(terminate) != LGLSXP || XLENGTH(terminate) != 1 ||
      asLogical(terminate) == NA_LOGICAL
    )
    error("Argument `terminate` must be TRUE or FALSE.");      // nocov

  if(TYPEOF(pos) != INTSXP)
    error("Argument `pos` must be integer.");          // nocov
  if(TYPEOF(overshoot) != LGLSXP)
    error("Argument `overshoot` must be logical.");          // nocov
  if(XLENGTH(pos) != XLENGTH(overshoot))
    error("Argument `overshoot` must be the same length as `pos`.");  // nocov
  if(XLENGTH(pos) != XLENGTH(is_start))
    error("Argument `ends` must be the same length as `pos`."); // nocov
  if(XLENGTH(pos) != XLENGTH(ids))
    error("Argument `ids` must be the same length as `pos`."); // nocov

  int prt = 0;
  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  R_xlen_t len = XLENGTH(pos);
  int normalize = asInteger(norm);
  int term = asLogical(terminate);

  const int res_cols = 4;  // if change this, need to change rownames init
  if(len > R_XLEN_T_MAX / res_cols) {
    // nocov start
    error("Argument `pos` may be no longer than R_XLEN_T_MAX / %d", res_cols);
    // nocov end
  }

  // Allocate result, will be a res_cols x n matrix.  A bit wasteful to record
  // all the color values given we'll rarely use them, but variable width
  // structures are likely to be much slower.  We could encode most color values
  // into one int but it would be a little annoying to retrieve them

  char * rownames[4] = { // make sure lines up with res_cols
    "pos.byte", "pos.raw", "pos.ansi", "pos.width"
  };
  SEXP res_rn = PROTECT(allocVector(STRSXP, res_cols)); ++prt;
  for(int i = 0; i < res_cols; i++) {
    SET_STRING_ELT(
      res_rn, i,
      FANSI_mkChar0(
        rownames[i], rownames[i] + strlen(rownames[i]),
        CE_NATIVE, (R_xlen_t) 0
    ) );
  }
  // Result will comprise a character vector with all the state tags at the
  // position as well as the various position translations in a matrix with as
  // many *columns* as the character vector has elements

  SEXP res_mx = PROTECT(allocVector(INTSXP, res_cols * len)); ++prt;
  SEXP dim = PROTECT(allocVector(INTSXP, 2)); ++prt;
  SEXP dim_names = PROTECT(allocVector(VECSXP, 2)); ++prt;

  INTEGER(dim)[0] = res_cols;
  INTEGER(dim)[1] = len;
  setAttrib(res_mx, R_DimSymbol, dim);
  SET_VECTOR_ELT(dim_names, 0, res_rn);
  SET_VECTOR_ELT(dim_names, 1, R_NilValue);
  setAttrib(res_mx, R_DimNamesSymbol, dim_names);

  SEXP res_str = PROTECT(allocVector(STRSXP, len)); ++prt;
  char * empty = "";
  SEXP res_chr, res_chr_prev =
    PROTECT(FANSI_mkChar0(empty, empty, CE_NATIVE, (R_xlen_t) 0)); ++prt;

  SEXP allowNA, keepNA;
  allowNA = keepNA = R_true;

  struct FANSI_state state = FANSI_state_init_full(
    x, warn, term_cap, allowNA, keepNA, type, ctl, (R_xlen_t) 0, "x"
  );
  struct FANSI_state state_prev = state;
  struct FANSI_state_pair state_pair = {state, state_prev};

  // Compute state at each `pos` and record result in our results matrix

  int type_int = asInteger(type);
  int pos_prev = -1;
  int * pos_i = INTEGER(pos);
  int * overshoot_i = LOGICAL(overshoot);
  int * start_i = INTEGER(is_start);
  struct FANSI_buff buff;
  int * res_mx_i = INTEGER(res_mx);
  FANSI_INIT_BUFF(&buff);

  // Need to handle possibility of real indices; these should always be
  // representable as R_xlen_t as they come from seq_len.
  union i_or_d_p {int * i; double * d;};
  union i_or_d_p id_i_p;

  if(TYPEOF(ids) == INTSXP) id_i_p.i = INTEGER(ids);
  else if(TYPEOF(ids) == REALSXP) id_i_p.d = REAL(ids);
  else error("Internal Error: bad ID type.");

  for(R_xlen_t i = 0; i < len; i++) {
    FANSI_interrupt(i);
    if(pos_i[i] == NA_INTEGER) {
      error("Internal Error: NAs not allowed"); // nocov
    } else {
      if(pos_i[i] < pos_prev)
        // nocov start
        error("Internal Error: `pos` must be sorted %d %d.", pos_i[i], pos_prev);
        // nocov end

      // index could be int or double (should we just coerce to double, assuming
      // 64 bit IEEE754 double?)
      R_xlen_t id_i;
      id_i = (R_xlen_t)(TYPEOF(ids) == INTSXP ? id_i_p.i[i] : id_i_p.d[i]) - 1;

      state_prev = state_pair.cur;
      state_pair = state_at_pos2(
        pos_i[i], state_pair.restart, type_int, start_i[i], overshoot_i[i],
        term, id_i
      );
      state = state_pair.cur;

      // Record position, these will be set back to 1 index at the R-level
      // by adding 1 to the starts.

      if(state.pos_ansi == FANSI_lim.lim_int.max && start_i[i]) {
        // nocov start
        error(
          "Internal Error: integer overflow for start position at index[%td]",
          FANSI_ind(id_i)
        );
        // nocov end
      }
      res_mx_i[i * res_cols + 0] = state.pos_byte;
      res_mx_i[i * res_cols + 1] = state.pos_raw;
      res_mx_i[i * res_cols + 2] = state.pos_ansi;  // this is what's used
      res_mx_i[i * res_cols + 3] = state.pos_width;

      // Record color tag if state changed
      if(
        FANSI_sgr_comp(state.sgr, state_prev.sgr) ||
        FANSI_url_comp(state.url, state_prev.url)
      ) {
        // this computes length twice..., we know state_char can be at most
        // INT_MAX excluding NULL (and certainly will be much less).
        FANSI_state_as_chr(&buff, state, normalize, i);
        res_chr = PROTECT(FANSI_mkChar(buff, CE_NATIVE, i));
      } else {
        res_chr = PROTECT(res_chr_prev);
      }
      SET_STRING_ELT(res_str, i, res_chr);
      res_chr_prev = res_chr;
      UNPROTECT(1);  // note res_chr is protected by virtue of being in res_str
      pos_prev = pos_i[i];
    }
    state_prev = state;
  }
  FANSI_release_buff(&buff, 1);

  SEXP res_list = PROTECT(allocVector(VECSXP, 2)); ++prt;
  SET_VECTOR_ELT(res_list, 0, res_str);
  SET_VECTOR_ELT(res_list, 1, res_mx);

  UNPROTECT(prt);
  return(res_list);
}
