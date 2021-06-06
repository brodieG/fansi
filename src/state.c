/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 *  This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
 * Create a state structure with everything set to zero
 *
 * We rely on struct initialization to set everything else to zero.
 *
 * FANSI_state_init_full is specifically to handle the allowNA case in nchar,
 * for which we MUST check `state.nchar_err` after each `FANSI_read_next`.  In
 * all other cases `R_nchar` shoudl be set to not `allowNA`.
 */
struct FANSI_state FANSI_state_init_full(
  SEXP strsxp, SEXP warn, SEXP term_cap, SEXP allowNA, SEXP keepNA,
  SEXP width, SEXP ctl, R_xlen_t i
) {
  // nocov start
  if(TYPEOF(strsxp) != STRSXP) {
    error(
      "Internal error: state_init with bad type for strsxp (%s)",
      type2char(TYPEOF(warn))
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

  if(TYPEOF(warn) != LGLSXP)
    error(
      "Internal error: state_init with bad type for warn (%s)",
      type2char(TYPEOF(warn))
    );
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

  // nocov end

  int * term_int = INTEGER(term_cap);
  int warn_int = asInteger(warn);
  int term_cap_int = 0;

  R_xlen_t i_len = XLENGTH(term_cap);
  for(R_xlen_t i = 0; i < i_len; ++i) {
    // we limit term cap to 31 to avoid potential bit shift issues with a signed
    // int.  In practice we shouldn't get anywhere close to this anwyay.
    if(term_int[i] > 31 || term_int[i] < 1)
      // nocov start
      error("Internal Error: bit flag value for term_cap illegal.");
      // nocov end

    term_cap_int |= 1 << (term_int[i] - 1);
  }
  return (struct FANSI_state) {
    .sgr = (struct FANSI_sgr) {.color = -1, .bg_color = -1},
    .sgr_prev = (struct FANSI_sgr) {.color = -1, .bg_color = -1},
    .string = string,
    .warn = warn_int,
    .term_cap = term_cap_int,
    .allowNA = asLogical(allowNA),
    .keepNA = asLogical(keepNA),
    .use_nchar = asInteger(width),  // 0 for chars, 1 for width
    .ctl = FANSI_ctl_as_int(ctl)
  };
}
struct FANSI_state FANSI_state_init(
  SEXP strsxp, SEXP warn, SEXP term_cap, R_xlen_t i
) {
  SEXP R_false = PROTECT(ScalarLogical(0));
  SEXP R_true = PROTECT(ScalarLogical(1));
  SEXP R_zero = PROTECT(ScalarInteger(0));
  struct FANSI_state res = FANSI_state_init_full(
    strsxp, warn, term_cap,
    R_true,  // allowNA for invalid multibyte
    R_false,
    R_zero,  // Don't use width by default
    R_zero,  // Don't treat any escapes as special by default
    i
  );
  UNPROTECT(3);
  return res;
}
struct FANSI_state FANSI_reset_width(struct FANSI_state state) {
  state.pos_width = 0;
  state.pos_width_target = 0;
  return state;
}
// Error message specific to use in adding spaces for tabs.
struct FANSI_state FANSI_inc_width(
  struct FANSI_state state, int inc, R_xlen_t i
) {
  if(state.pos_width_target > FANSI_lim.lim_int.max - inc)
    error(
      "Expanding tabs will cause string to exceed INT_MAX at index [%ju].",
      FANSI_ind(i)
    );

  state.pos_width_target += inc;
  state.pos_width += inc;
  return state;
}
/*
 * Reset the position counters
 *
 * Intended so that all the state info is kept for when state persists from one
 * element in a character vector to the next.
 *
 * We are not 100% sure we're resetting everything that needs to be reset.
 */
struct FANSI_state FANSI_reset_pos(struct FANSI_state state) {
  state.pos_byte = 0;
  state.pos_ansi = 0;
  state.pos_raw = 0;
  state.pos_width = 0;
  state.pos_width_target = 0;
  state.last_char_width = 0;
  state.terminal = 0;
  state.non_normalized = 0;
  return state;
}
/*
 * Compute the state given a character position (raw position)
 *
 * Assumption is that any byte > 127 is UTF8 (i.e. input strings must be all
 * legal ASCII, or must have been translated to UTF8).  Potential for relaxation
 * with latin-1 since those are all single byte single width, but right now
 * that's not implemented.
 *
 * This function is designed to be called iteratively on the same string with
 * monotonically increasing values of `pos`.  This allows us to compute state at
 * multiple positions while re-using the work we did on the earlier positions.
 * To transfer the state info from earlier positions we use a FANSI_state_pair
 * object that contains the state info from the previous computation.
 *
 * @param pos the raw position (i.e. treating parseable ansi tags as zero
 *   length) we want the state for
 * @param state_pair two states (see description)
 * @param int type whether to use character (0), width (1), or byte (2) when
 *   computing the position
 * @param lag in cases where requested breakpoint is not feasible because of
 *   multi width characters, whether to return end of prior (0) or the start of
 *   the next (1)
 * @param end whether the request is made for the ends of the string (i.e. as
 *   part of the `stop` parameter) (1), or not (0) (i.e. as part of the start
 *   parameters).
 */
static struct FANSI_state_pair state_at_position(
  int pos, struct FANSI_state_pair state_pair, int type, int lag, int end,
  R_xlen_t i
) {
  struct FANSI_state state = state_pair.cur;
  int pos_init = type ? state.pos_width : state.pos_raw;
  if(pos < pos_init)
    // nocov start
    error(
      "Internal Error: %s (%d) than `pos` (%d)",
      "Cannot re-use a state for a later position",
      pos_init, pos
    );
    // nocov end
  int cond = 0;

  // Need to reset pos_width_target since it could be distorted by a previous
  // middle of wide char event
  state.pos_width_target = state.pos_width;

  struct FANSI_state state_res, state_prev, state_prev_buff;

  state_prev = state_prev_buff = state_pair.prev;
  state_res = state;

  while(1) {
    state_prev = state_res = state;
    state.err_code = state.last = 0;

    // Handle UTF-8, we need to record the byte size of the sequence as well as
    // the corresponding display width.

    if(state.pos_byte == FANSI_lim.lim_int.max)
      // nocov start
      // ... a bit tricky here, because we read ahead a few bytes in some
      // circumstances, but not all, so the furthest pos_byte should be allowed
      // to get actually varies
      error("Internal Error: counter overflow while reading string.");
      // nocov end

    state = FANSI_read_next(state, i);

    // cond is just how many units we have left until our requested position.
    // we can overshoot and it can be negative

    switch(type) {
      case 0: cond = pos - state.pos_raw; break;
      case 1: cond = pos - state.pos_width; break;
      default:
        // nocov start
        error("Internal Error: Illegal offset type; contact maintainer.");
        // nocov end
    }
    // If zero width advance, we want to update prev state to be the newest
    // state

    if(state.pos_width == state_prev.pos_width) state_prev = state;

    // We still have stuff to process, though keep in mind we can be at end of
    // string with cond > 0 if we ask for position past end

    if(cond >= 0) {
      if(state.string[state.pos_byte]) {
        // some ambiguity as to whether the next `state_prev` will be valid, so
        // we store the current one just in case
        state_prev_buff = state_prev;
        continue;
      }
      // state_res = state_prev;
      state_res = state;
      break;
    }
    /*
     * A key problem here is that what constitues a valid offset depends on the
     * display width of the character.  For example, -1 makes sense if the
     * character is 1 wide, or if we're looking to match that character to end.
     *
     * If instead we are matching the start of a wide character, then we're
     * looking for the overshoot to be the width of the character.
     */

    if(type == 1) { // width mode
      if(!lag) {
        if(end && cond != -1) {
          state_res = state_prev_buff;
        } else if (!end && cond != -state.last_char_width) {
          state_res = state;
        }
      }
      // This is the width we hoped to get originally
      state_res.pos_width_target = pos;
    } else if(cond < -1) {
      // nocov start
      error(
        "%s%s",
        "Internal Error: partial width should only happen in type 'width'; ",
        "contact maintainer."
      );
      // nocov end
    }
    break;
  }
  state_res.warn = state.warn;  // otherwise can get double warning

  // Keep advancing if there are any zero width UTF8 characters, not entirely
  // sure what we're supposed to do with prev_buff here, need to have some test
  // cases to see what happens.  Note we only do this when we end on zero width
  // chars

  if(end) {
    struct FANSI_state state_next, state_next_prev, state_next_prev_prev;
    state_next_prev_prev = state_res;
    state_next_prev = FANSI_read_next(state_next_prev_prev, i);
    state_next = FANSI_read_next(state_next_prev, i);

    while(!state_next.last_char_width) {
      state_next_prev_prev = state_next_prev;
      state_next_prev = state_next;
      state_next = FANSI_read_next(state_next, i);
      if(!state_next.string[state_next.pos_byte]) break;
    }
    state_res = state_next_prev_prev;
  }
  // We return the state just before we overshot the end

  return (struct FANSI_state_pair){.cur=state_res, .prev=state_prev_buff};
}
/*
 * We always include the size of the delimiter; could be a problem that this
 * isn't the actual size, but rather the maximum size (i.e. we always assume
 * three bytes even if the numbers don't get into three digits).
 */
int FANSI_color_size(int color, int * color_extra) {
  int size = 0;
  if(color == 8 && color_extra[0] == 2) {
    size = 3 + 2 +
      FANSI_digits_in_int(color_extra[1]) + 1 +
      FANSI_digits_in_int(color_extra[2]) + 1 +
      FANSI_digits_in_int(color_extra[3]) + 1;
  } else if (color == 8 && color_extra[0] == 5) {
    size = 3 + 2 +
      FANSI_digits_in_int(color_extra[1]) + 1;
  } else if (color == 8) {
    error("Internal Error: unexpected compound color format");   // nocov
  } else if (color >= 0 && color < 10) {
    size = 3;
  } else if (color >= 90 && color <= 97) {
    size = 3;
  } else if (color >= 100 && color <= 107) {
    size = 4;
  } else if (color > 0) {
    error("Internal Error: unexpected color format"); // nocov
  }
  return size;
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
int FANSI_sgr_write(
  char * buff, struct FANSI_sgr sgr, int len, int normalize, R_xlen_t i
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
  const char * err_msg = "Writing SGR tokens"; // for COPY_OR_MEASURE
  // biggest would be "\033[XXm" + NULL, won't fit e.g bright color codes
  // CAREFUL if we modify code to use `tmp` for other purposes.
  char tmp[6] = {0};
  char * buff_track = buff;

  if(FANSI_sgr_active(sgr)) {
    if(!normalize) len += COPY_OR_MEASURE(&buff_track, "\033[");
    // styles
    char tokval[2] = {0};
    for(unsigned int i = 1; i < 10; i++) {
      if((1U << i) & sgr.style) {
        *tokval = '0' + (char) i;
        len += COPY_OR_MEASURE(&buff_track, make_token(tmp, tokval, normalize));
    } }
    // styles outside 0-9

    if(sgr.style & (1 << 10)) {
      // fraktur
      len += COPY_OR_MEASURE(&buff_track, make_token(tmp, "20", normalize));
    }
    if(sgr.style & (1 << 11)) {
      // double underline
      len += COPY_OR_MEASURE(&buff_track, make_token(tmp, "21", normalize));
    }
    if(sgr.style & (1 << 12)) {
      // prop spacing
      len += COPY_OR_MEASURE(&buff_track, make_token(tmp, "26", normalize));
    }
    // colors
    if(sgr.color > -1) {
      char tokval[17] = {0};  // largest: "38;2;255;255;255", 16 chars + NULL
      len += COPY_OR_MEASURE(
        &buff_track,
        color_token(tokval, sgr.color, sgr.color_extra, 3, normalize)
      );
    }
    if(sgr.bg_color > -1) {
      char tokval[17] = {0};
      len += COPY_OR_MEASURE(
        &buff_track,
        color_token(tokval, sgr.bg_color, sgr.bg_color_extra, 4, normalize)
      );
    }
    // Borders
    if(sgr.border) {
      char tokval[3] = {'5', '0'};
      for(int i = 1; i < 4; ++i) {
        if((1 << i) & sgr.border) {
          tokval[1] = '0' + i;
          len +=
            COPY_OR_MEASURE(&buff_track, make_token(tmp, tokval, normalize));
    } } }
    // Ideogram
    if(sgr.ideogram) {
      char tokval[3] = {'6', '0'};
      for(int i = 0; i < 5; ++i){
        if((1 << i) & sgr.ideogram) {
          tokval[1] = '0' + i;
          len +=
            COPY_OR_MEASURE(&buff_track, make_token(tmp, tokval, normalize));
    } } }
    // font
    if(sgr.font) {
      char tokval[3] = {'1', '0'};
      tokval[1] = '0' + (sgr.font % 10);
      len += COPY_OR_MEASURE(&buff_track, make_token(tmp, tokval, normalize));
    }
    // Finalize

    if(buff) {
      buff[len - 1] = 'm';
      // nocov start
      if(buff_track - buff != len - len0)
        // nocov start
        error(
          "Internal Error: %s (%td vs %d).",
          "buffer length mismatch in writing SGR generation (2)",
          buff_track - buff, len - len0
        );
      // nocov end
    }
  }
  return len - len0;
}
/*
 * Generate the ANSI tag corresponding to the state and write it out as a NULL
 * terminated string.
 */
char * FANSI_sgr_as_chr(struct FANSI_sgr sgr, int normalize, R_xlen_t i) {
  // First pass computes total size of tag
  int tag_len = FANSI_sgr_write(NULL, sgr, 0, normalize, i);

  // Now write
  char * tag_tmp = R_alloc((size_t) tag_len + 1, sizeof(char));
  FANSI_sgr_write(tag_tmp, sgr, 0, normalize, i);
  tag_tmp[tag_len] = 0;

  return tag_tmp;
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
int FANSI_sgr_comp_basic(
  struct FANSI_sgr target, struct FANSI_sgr current
) {
  // 1023 is '11 1111 1111' in binary, so this will grab the last ten bits
  // of the styles which are the 1-9 styles
  return FANSI_sgr_comp_color(target, current) ||
    (target.style & 1023) != (current.style & 1023);
}
int FANSI_sgr_comp(struct FANSI_sgr target, struct FANSI_sgr current) {
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
 */
struct FANSI_sgr FANSI_sgr_setdiff(struct FANSI_sgr old, struct FANSI_sgr new) {
  struct FANSI_sgr res = {
    .color=-1, .bg_color=-1, .style=0, .border=0, .ideogram=0, .font=0
  };
  if(old.color > -1 && new.color == - 1) {
    res.color = old.color;
    memcpy(res.color_extra, old.color_extra, 4);
  }
  if(old.bg_color > -1 && new.bg_color == - 1) {
    res.bg_color = old.bg_color;
    memcpy(res.bg_color_extra, old.bg_color_extra, 4);
  }
  if(old.font && !new.font) res.font = old.font;
  res.style = old.style & (~new.style);
  res.border = old.border & (~new.border);
  res.ideogram = old.ideogram & (~new.ideogram);
  return res;
}

// Keep synchronized with `sgr_close`
int FANSI_sgr_active(struct FANSI_sgr sgr) {
  return
    sgr.style || sgr.color >= 0 || sgr.bg_color >= 0 ||
    sgr.font || sgr.border || sgr.ideogram;
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

int FANSI_sgr_close(
  char * buff, struct FANSI_sgr sgr, int len, int normalize, R_xlen_t i
) {
  // char * buff_track = buff;
  int len0 = len;
  const char * err_msg = "Generating closing SGR";

  if(FANSI_sgr_active(sgr)) {
    if(normalize) {
      // We're deliberate in only closing things we know how to close in
      // both the state and in the ouptut string, that way we can check
      // state at the end to make sure we did actually close everything.

      if(sgr.font) {
        sgr.font = 0;
        len += COPY_OR_MEASURE(&buff, "\033[10m");
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
        len += COPY_OR_MEASURE(&buff, "\033[22m");
      }
      if(sgr.style & s_frakital) {
        sgr.style &= ~s_frakital;
        len += COPY_OR_MEASURE(&buff, "\033[23m");
      }
      if(sgr.style & s_underline) {
        sgr.style &= ~s_underline;
        len += COPY_OR_MEASURE(&buff, "\033[24m");
      }
      if(sgr.style & s_blink) {
        sgr.style &= ~s_blink;
        len += COPY_OR_MEASURE(&buff, "\033[25m");
      }
      // 26 is opening prop spacing (50 to close)
      if(sgr.style & s_inverse) {
        sgr.style &= ~s_inverse;
        len += COPY_OR_MEASURE(&buff, "\033[27m");
      }
      if(sgr.style & s_conceal) {
        sgr.style &= ~s_conceal;
        len += COPY_OR_MEASURE(&buff, "\033[28m");
      }
      if(sgr.style & s_strikethrough) {
        sgr.style &= ~s_strikethrough;
        len += COPY_OR_MEASURE(&buff, "\033[29m");
      }
      // Colors
      if(sgr.color >= 0) {
        sgr.color = -1;
        len += COPY_OR_MEASURE(&buff, "\033[39m");
      }
      if(sgr.bg_color >= 0) {
        sgr.bg_color = -1;
        len += COPY_OR_MEASURE(&buff, "\033[49m");
      }
      // Prop spacing
      if(sgr.style & s_propspc) {
        sgr.style &= ~s_propspc;
        len += COPY_OR_MEASURE(&buff, "\033[50m");
      }
      // Border and ideogram
      if(sgr.border & (1U << 1U | 1U << 2U)) {
        sgr.border &= ~(1U << 1U | 1U << 2U);
        len += COPY_OR_MEASURE(&buff, "\033[54m");
      }
      if(sgr.border & (1U << 3U)) {
        sgr.border &= ~(1U << 3U);
        len += COPY_OR_MEASURE(&buff, "\033[55m");
      }
      if(sgr.ideogram > 0U) {
        for(unsigned int k = 0; k < 5; ++k) sgr.ideogram &= ~(1U << k);
        len += COPY_OR_MEASURE(&buff, "\033[65m");
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
      len += COPY_OR_MEASURE(&buff, "\033[0m");
    }
  }
  return len - len0;
}
/*
 * For closing things for substr, so we don't need to automatically normalize
 * every string if we just close with ESC[0m.
 *
 * Pretty inefficient to do it this way...
 *
 * Any warnings this could emit should have been emitted earlier during reading
 * of string the active state came from.
 *
 * @param x should be a vector of active states at end of strings.
 */
SEXP FANSI_sgr_close_ext(SEXP x, SEXP term_cap) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` should be a character vector.");  // nocov
  if(TYPEOF(term_cap) != INTSXP)
    error("Argument `term.cap` should be an integer vector.");  // nocov

  // Compress `ctl` into a single integer using bit flags

  R_xlen_t len = xlength(x);
  SEXP res = x;

  PROTECT_INDEX ipx;
  // reserve spot if we need to alloc later
  PROTECT_WITH_INDEX(res, &ipx);

  struct FANSI_buff buff = {.len = 0};
  int normalize = 1;

  SEXP R_true = PROTECT(ScalarLogical(1));
  SEXP R_false = PROTECT(ScalarLogical(0));
  SEXP R_one = PROTECT(ScalarInteger(1));
  SEXP R_zero = PROTECT(ScalarInteger(0));

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    SEXP x_chr = STRING_ELT(x, i);
    if(x_chr == NA_STRING || !LENGTH(x_chr)) continue;

    struct FANSI_state state = FANSI_state_init_full(
      x, R_false, term_cap, R_true, R_true, R_zero, R_one, i
    );
    while(*(state.string + state.pos_byte)) {
      state = FANSI_read_next(state, i);
    }
    int len = 0;
    len = FANSI_sgr_close(NULL, state.sgr, len, normalize, i);
    if(len) {
      if(res == x) REPROTECT(res = duplicate(x), ipx);
      FANSI_size_buff(&buff, (size_t)len + 1);
      FANSI_sgr_close(buff.buff, state.sgr, len, normalize, i);
      cetype_t chr_type = getCharCE(x_chr);
      SEXP reschr =
        PROTECT(FANSI_mkChar(buff.buff, buff.buff + len, chr_type, i));
      SET_STRING_ELT(res, i, reschr);
      UNPROTECT(1);
    }
  }
  UNPROTECT(5);
  return res;
}

/*
 * R interface for state_at_position
 * @param string we're interested in state of
 * @param pos integer positions along the string, one index, sorted
 */

SEXP FANSI_state_at_pos_ext(
  SEXP text, SEXP pos, SEXP type,
  SEXP lag, SEXP ends,
  SEXP warn, SEXP term_cap, SEXP ctl,
  SEXP norm
) {
  /*******************************************\
  * IMPORTANT: INPUT MUST ALREADY BE IN UTF8! *
  \*******************************************/

  // no errors should make it here, it should be handled R side
  if(TYPEOF(text) != STRSXP && XLENGTH(text) != 1)
    error("Argument `text` must be character(1L).");   // nocov
  if(STRING_ELT(text, 0) == NA_STRING)
    error("Argument `text` may not be NA.");   // nocov
  if(TYPEOF(pos) != INTSXP)
    error("Argument `pos` must be integer.");          // nocov
  if(TYPEOF(lag) != LGLSXP)
    error("Argument `lag` must be logical.");          // nocov
  if(XLENGTH(pos) != XLENGTH(lag))
    error("Argument `lag` must be the same length as `pos`.");  // nocov
  if(XLENGTH(pos) != XLENGTH(ends))
    error("Argument `ends` must be the same length as `pos`."); // nocov
  if(TYPEOF(warn) != LGLSXP)
    error("Argument `warn` must be integer.");         // nocov
  if(TYPEOF(term_cap) != INTSXP)
    error("Argument `term.cap` must be integer.");     // nocov
  if(TYPEOF(ctl) != INTSXP)
    error("Argument `ctl` must be integer.");         // nocov
  if(TYPEOF(norm) != LGLSXP)
    error("Argument `norm` must be logical.");         // nocov

  R_xlen_t len = XLENGTH(pos);
  int normalize = asInteger(norm);

  const int res_cols = 4;  // if change this, need to change rownames init
  if(len > R_XLEN_T_MAX / res_cols) {
    // nocov start
    error("Argument `pos` may be no longer than R_XLEN_T_MAX / %d", res_cols);
    // nocov end
  }
  struct FANSI_state_pair state_pair;

  // Allocate result, will be a res_cols x n matrix.  A bit wasteful to record
  // all the color values given we'll rarely use them, but variable width
  // structures are likely to be much slower.  We could encode most color values
  // into one int but it would be a little annoying to retrieve them

  const char * rownames[4] = { // make sure lines up with res_cols
    "pos.byte", "pos.raw", "pos.ansi", "pos.width"
  };
  SEXP res_rn = PROTECT(allocVector(STRSXP, res_cols));
  for(int i = 0; i < res_cols; i++)
    SET_STRING_ELT(
      res_rn, i,
      FANSI_mkChar(
        rownames[i], rownames[i] + strlen(rownames[i]),
        CE_NATIVE, (R_xlen_t) 0
    ) );

  // Result will comprise a character vector with all the state tags at the
  // position as well as the various position translations in a matrix with as
  // many *columns* as the character vector has elements

  SEXP res_mx = PROTECT(allocVector(REALSXP, res_cols * len));
  SEXP dim = PROTECT(allocVector(INTSXP, 2));
  SEXP dim_names = PROTECT(allocVector(VECSXP, 2));

  INTEGER(dim)[0] = res_cols;
  INTEGER(dim)[1] = len;
  setAttrib(res_mx, R_DimSymbol, dim);
  SET_VECTOR_ELT(dim_names, 0, res_rn);
  SET_VECTOR_ELT(dim_names, 1, R_NilValue);
  setAttrib(res_mx, R_DimNamesSymbol, dim_names);

  SEXP res_str = PROTECT(allocVector(STRSXP, len));
  const char * empty = "";
  SEXP res_chr, res_chr_prev =
    PROTECT(FANSI_mkChar(empty, empty, CE_NATIVE, (R_xlen_t) 0));
  // PROTECT should not be needed here, but rchk complaining
  SEXP R_true = PROTECT(ScalarLogical(1));
  struct FANSI_state state = FANSI_state_init_full(
    text, warn, term_cap, R_true, R_true, type, ctl, (R_xlen_t) 0
  );
  UNPROTECT(1);

  struct FANSI_state state_prev = state;
  state_pair.cur = state;
  state_pair.prev = state_prev;

  // Compute state at each `pos` and record result in our results matrix

  int type_int = asInteger(type);
  int pos_i, pos_prev = -1;

  for(R_xlen_t i = 0; i < len; i++) {
    R_CheckUserInterrupt();
    pos_i = INTEGER(pos)[i];
    if(pos_i == NA_INTEGER) {
      error("Internal Error: NAs not allowed"); // nocov
    } else {
      if(pos_i < pos_prev)
        // nocov start
        error("Internal Error: `pos` must be sorted %d %d.", pos_i, pos_prev);
        // nocov end

      // We need to allow the same position multiple times in case it shows up
      // as starts and ends, etc.

      if(pos_i == pos_prev) state_pair.cur = state_pair.prev;

      state_pair = state_at_position(
        pos_i, state_pair, type_int, INTEGER(lag)[i], INTEGER(ends)[i], i
      );
      state = state_pair.cur;

      // Record position, but set them back to 1 index, need to use double
      // because INTEGER could overflow because of this + 1, although ironically
      // `substr` probably can't subset the INTMAX character due to the 1
      // indexing...

      REAL(res_mx)[i * res_cols + 0] = state.pos_byte + 1;
      REAL(res_mx)[i * res_cols + 1] = state.pos_raw + 1;
      REAL(res_mx)[i * res_cols + 2] = state.pos_ansi + 1;
      REAL(res_mx)[i * res_cols + 3] = state.pos_width_target + 1;

      // Record color tag if state changed

      if(FANSI_sgr_comp(state.sgr, state_prev.sgr)) {
        // this computes length twice..., we know state_char can be at most
        // INT_MAX excluding NULL (and certainly will be much less).
        char * state_chr = FANSI_sgr_as_chr(state.sgr, normalize, i);
        res_chr = PROTECT(
          FANSI_mkChar(
            state_chr, state_chr + strlen(state_chr), CE_NATIVE, i
        ) );
      } else {
        res_chr = PROTECT(res_chr_prev);
      }
      SET_STRING_ELT(res_str, i, res_chr);
      res_chr_prev = res_chr;
      UNPROTECT(1);  // note res_chr is protected by virtue of being in res_str
      pos_prev = pos_i;
    }
    state_prev = state;
  }
  SEXP res_list = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res_list, 0, res_str);
  SET_VECTOR_ELT(res_list, 1, res_mx);

  UNPROTECT(7);
  return(res_list);
}
