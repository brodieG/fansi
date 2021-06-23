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
 * Data related to prefix / initial
 *
 * Make sure to coordinate with ALL functions that generate/modify these (below)
 * if you change the struct definition.
 */
struct FANSI_prefix_dat {
  const char * string;  // string translated to utf8
  int width;            // display width as computed by R_nchar
  int bytes;            // bytes, excluding NULL terminator
  // how many indent/exdent bytes are included in string, width, and bytes
  int indent;
  int has_utf8;         // whether utf8 contains value > 127
  int warn;             // warning issued while stripping
};
/*
 * Generate data related to prefix / initial
 */

static struct FANSI_prefix_dat make_pre(SEXP x) {
  SEXP chrsxp = STRING_ELT(x, 0);
  FANSI_check_chrsxp(chrsxp, 0);
  const char * x_utf8 = CHAR(chrsxp);
  // ideally we would IS_ASCII(x), but that's not available to extensions
  int x_has_utf8 = FANSI_has_utf8(x_utf8);

  // ideally would have an internal interface to strip so we don't need to
  // generate these SEXPs here
  SEXP warn = PROTECT(ScalarInteger(2));
  SEXP ctl = PROTECT(ScalarInteger(1));
  SEXP x_strip = PROTECT(FANSI_strip(x, ctl, warn));
  int x_width = R_nchar(
    asChar(x_strip), Width, TRUE, FALSE, "when computing display width"
  );
  // wish we could get this directly from R_nchar, grr

  int x_bytes = strlen(x_utf8);
  int warn_int = getAttrib(x_strip, FANSI_warn_sym) != R_NilValue;

  if(x_width == NA_INTEGER) {
    x_width = x_bytes;
    warn_int = 9;
  }
  UNPROTECT(3);
  return (struct FANSI_prefix_dat) {
    .string=x_utf8, .width=x_width, .bytes=x_bytes, .has_utf8=x_has_utf8,
    .indent=0, .warn=warn_int
  };
}
/*
 * Combine initial and indent (or prefix and exdent)
 */
static struct FANSI_prefix_dat pad_pre(
  struct FANSI_prefix_dat dat, int spaces
) {
  int pre_len = dat.bytes;
  const char * pre_chr = dat.string;

  int alloc_size = FANSI_ADD_INT(FANSI_ADD_INT(pre_len, spaces), 1);
  char * res_start = "";
  if(alloc_size > 1) {
    // Can't use buff here because we don't write this string out

    char * res = res_start = R_alloc(alloc_size, sizeof(char));
    memcpy(res, pre_chr, pre_len);
    res += pre_len;
    for(int i = 0; i < spaces; ++i) *(res++) = ' ';
    *res = '\0';
  }
  dat.string = (const char *) res_start;
  dat.bytes = FANSI_ADD_INT(dat.bytes, spaces);
  dat.width = FANSI_ADD_INT(dat.width, spaces);
  dat.indent = FANSI_ADD_INT(dat.indent, spaces);

  return dat;
}
/*
 * Adjusts width and sizes to pretend there is no indent.  String itself is not
 * modified so this only works if whatever is using the string is using the byte
 * counter to limit how much of the string it reads
 */

static struct FANSI_prefix_dat drop_pre_indent(struct FANSI_prefix_dat dat) {
  dat.bytes = FANSI_ADD_INT(dat.bytes, -dat.indent);
  dat.width = FANSI_ADD_INT(dat.width, -dat.indent);
  dat.indent = FANSI_ADD_INT(dat.indent, -dat.indent);
  if(dat.indent < 0)
    // nocov start
    error(
      "Internal Error: cannot drop indent when there is none; contact ",
      "maintainer."
    );
    // nocov end
  return dat;
}

/*
 * Write a line
 *
 * @param state_bound the point where the boundary is
 * @param state_start the starting point of the line
 * @param normalize currently only applies to the start and end SGR.
 */

static SEXP writeline(
  struct FANSI_state state_bound, struct FANSI_state state_start,
  struct FANSI_buff * buff,
  struct FANSI_prefix_dat pre_dat,
  int tar_width, const char * pad_chr,
  R_xlen_t i,
  int normalize, int terminate
) {
  // First thing we need to do is check whether we need to pad as that affects
  // how we treat the boundary.

  int line_width = state_bound.pos_width - state_start.pos_width;
  int target_pad = 0;
  if(tar_width < 0) tar_width = 0;
  if(line_width <= tar_width && *pad_chr) {
    target_pad = tar_width - line_width;
  }
  // handle corner case for empty strings that don't get indented by strwrap;
  // we considered testing width instead of size as that would also prevent
  // indent on thing that just have ESCs, but decided against it (arbitrarily)
  //
  // We do not re-terminate the string, instead relying on widths / sizes to
  // make sure only the non-indent bit is copied

  if(state_bound.pos_byte == state_start.pos_byte)
    pre_dat = drop_pre_indent(pre_dat);

  // In case the last state read was at the end of the string, use the prior
  // state.  No point writing a state that will be immediately closed.  But we
  // must write it if we're going to add padding.

  if(state_bound.terminal && !target_pad && FANSI_sgr_active(state_bound.sgr)) {
    state_bound.sgr = state_bound.sgr_prev;
    state_bound.pos_byte = state_bound.pos_byte_sgr_start;
    state_bound.terminal = 0;
  }
  // state_bound.pos_byte 1 past what we need, so this should include room
  // for NULL terminator
  if(
    (state_bound.pos_byte < state_start.pos_byte) ||
    (state_bound.pos_width < state_start.pos_width)
  )
    // nocov start
    error("Internal Error: boundary leading position; contact maintainer.");
    // nocov end

  if(state_bound.pos_byte < state_start.pos_byte)
    error("Internal Error: line ends backwards.");  // nocov
  if(state_bound.pos_byte < state_start.pos_byte)
    error("Internal Error: negative line width.");  // nocov

  // Check if we are in a CSI state b/c if we need extra room for
  // the closing state tag
  int needs_start = FANSI_sgr_active(state_start.sgr);
  int needs_close = terminate && FANSI_sgr_active(state_bound.sgr);

  // Measure/Write loop (see src/write.c)
  char * buff_track = NULL;
  int len = 0;
  const char * err_msg = "Writing line";

  for(int k = 0; k < 2; ++k) {
    if(k) {
      FANSI_size_buff(buff, len);
      buff_track = buff->buff;
      len = 0;  // reset len
    }
    if(needs_start) {
      err_msg = "Adding initial SGR";
      len += FANSI_W_sgr(&buff_track, state_start.sgr, len, normalize, i);
    }
    // Apply indent/exdent prefix/initial
    if(pre_dat.bytes) {
      err_msg = "Adding prefix characters";
      len += FANSI_W_MCOPY(&buff_track, pre_dat.string, pre_dat.bytes);
    }
    // Actual string, remember state_bound.pos_byte is one past what we need
    const char * string = state_start.string + state_start.pos_byte;
    int bytes = state_bound.pos_byte - state_start.pos_byte;
    len += FANSI_W_MCOPY(&buff_track, string, bytes);

    // Add padding if needed
    err_msg = "Adding padding";
    int to_pad = target_pad;
    len += FANSI_W_FILL(&buff_track, *pad_chr, to_pad);

    // And turn off CSI styles if needed
    if(needs_close)
      len += FANSI_W_sgr_close(
        &buff_track, state_bound.sgr, len, normalize, i
      );
  }
  if(buff_track && (buff_track - buff->buff) != len) {
    // nocov start
    error(
      "Internal Error: line buffer size mismatch (%jd vs %d) at index [%jd].",
      buff_track - buff->buff, len, FANSI_ind(i)
    );
    // nocov end
  }
  // Now create the charsxp and append to the list, start by determining
  // what encoding to use.
  cetype_t chr_type = CE_NATIVE;
  if((state_bound.has_utf8 || pre_dat.has_utf8)) chr_type = CE_UTF8;
  return FANSI_mkChar(buff->buff, buff_track, chr_type, i);
}
/*
 * All input strings are expected to be in UTF8 compatible format (i.e. either
 * encoded in UTF8, or contain only bytes in 0-127).  That way we know we can
 * set the encoding to UTF8 if there are any bytes greater than 127, or NATIVE
 * otherwise under the assumption that 0-127 is valid in all encodings.
 *
 * @param buff a pointer to a buffer struct.  We use pointer to a
 *   pointer because it may need to be resized, but we also don't want to
 *   re-allocate the buffer between calls.
 * @param pre_first, pre_next, strings (and associated meta data) to prepend to
 *   each line; pre_first can be based of of `prefix` or off of `initial`
 *   depending whether we're at the very first line of the external input or not
 * @param strict whether to hard wrap at width or not (not is what strwrap does
 *   by default)
 */

static SEXP strwrap(
  SEXP x, int width,
  struct FANSI_prefix_dat pre_first,
  struct FANSI_prefix_dat pre_next,
  int wrap_always,
  struct FANSI_buff * buff,
  const char * pad_chr,
  int strip_spaces,
  SEXP warn, SEXP term_cap,
  int first_only, SEXP ctl,
  R_xlen_t index,
  int normalize,
  int carry,
  struct FANSI_sgr * sgr_carry,
  int terminate
) {
  SEXP R_true = PROTECT(ScalarLogical(1));
  SEXP R_one = PROTECT(ScalarInteger(1));
  struct FANSI_state state = FANSI_state_init_full(
    x, warn, term_cap, R_true, R_true, R_one, ctl, index
  );
  UNPROTECT(2);

  int width_1 = FANSI_ADD_INT(width, -pre_first.width);
  int width_2 = FANSI_ADD_INT(width, -pre_next.width);

  int width_tar = width_1;

  if(width < 1 && wrap_always)
    error("Internal Error: invalid width."); // nocov
  if(wrap_always && (width_1 < 0 || width_2 < 0))
    error("Internal Error: incompatible width/indent/prefix."); // nocov

  // Use LISTSXP so we don't have to do a two pass process to determine how many
  // items we're going to have, unless we're in first only in which case we know
  // we only need one element per and don't actually use these

  SEXP char_list_start, char_list;
  char_list_start = char_list = PROTECT(list1(R_NilValue));

  int prev_boundary = 0;    // tracks if previous char was a boundary
  int has_boundary = 0;     // tracks if at least one boundary in a line
  int para_start = 1;

  // byte we previously wrote from, need to track to detect potential infinite
  // loop when we wrap-always but the wrap width is narrower than a wide
  // character

  int first_line = 1;
  int last_start = 0;

  // Need to keep track of where word boundaries start and end due to
  // possibility for multiple elements between words

  if(carry) state.sgr = *sgr_carry;
  struct FANSI_state state_start, state_bound, state_prev;
  state_start = state_bound = state_prev = state;
  R_xlen_t size = 0;
  SEXP res_sxp;

  while(1) {
    struct FANSI_state state_next;

    // Can no longer advance after we reach end, but we still need to assemble
    // strings so we assign `state` even though technically not correct

    if(!state.string[state.pos_byte]){
      state_next = state;
    } else {
      state_next = FANSI_read_next(state, index);
    }
    state.warn = state_bound.warn = state_next.warn;  // avoid double warning

    // detect word boundaries and paragraph starts; we need to track
    // state_bound for the special case where we are in strip space mode
    // and we happen to hit the width in a two space sequence such as we might
    // get after [.!?].
    //
    // We're ignoring other word boundaries, but strwrap does too.
    //
    // Recall that if `strip_spaces == TRUE` string will have already been
    // processed to remove sequential spaces.

    if(
      state.string[state.pos_byte] == ' ' ||
      state.string[state.pos_byte] == '\t' ||
      state.string[state.pos_byte] == '\n'
    ) {
      if(strip_spaces && !prev_boundary) state_bound = state;
      else if(!strip_spaces) state_bound = state;
      has_boundary = prev_boundary = 1;
    } else {
      prev_boundary = 0;
    }
    // Write the line

    if(
      !state.string[state.pos_byte] ||
      // newlines kept in strtrim mode
      (state.string[state.pos_byte] == '\n' && !first_only) ||
      (
        (
          state.pos_width > width_tar ||
          (
            // If exactly at width we need to keep going if the next char is
            // zero width, otherwise we should write the string
            state.pos_width == width_tar &&
            state_next.pos_width > state.pos_width
        ) ) &&
        (has_boundary || wrap_always)
      )
    ) {
      if(
        !state.string[state.pos_byte] ||
        (wrap_always && !has_boundary) || first_only
      ) {
        if(state.pos_width > width_tar && wrap_always) {
          state = state_prev; // wide char overshoot
        }
        state_bound = state;
      }
      if(!first_line && last_start >= state_start.pos_byte) {
        error(
          "%s%s",
          "Wrap error: trying to wrap to width narrower than ",
          "character width; set `wrap.always=FALSE` to resolve."
        );
      }
      // If not stripping spaces we need to keep the last boundary char; note
      // that boundary is advanced when strip_spaces == FALSE in earlier code.

      if(
        !strip_spaces && has_boundary && (
          state_bound.string[state_bound.pos_byte] == ' ' ||
          state_bound.string[state_bound.pos_byte] == '\t'
        ) &&
        state_bound.pos_byte < state.pos_byte
      ) {
        state_bound = FANSI_read_next(state_bound, index);
      }
      // Write the string

      res_sxp = PROTECT(
        writeline(
          state_bound, state_start, buff,
          para_start ? pre_first : pre_next,
          width_tar, pad_chr, index, normalize, terminate
        )
      );
      first_line = 0;
      last_start = state_start.pos_byte;
      // first_only for `strtrim`

      if(!first_only) {
        SETCDR(char_list, list1(res_sxp));
        char_list = CDR(char_list);
        UNPROTECT(1);
      } else {
        // Need end state if in trim mode and we wish to carry
        if(carry)
          while(state.string[state.pos_byte])
            state = FANSI_read_next(state, index);
        break;
      }
      // overflow should be impossible here since string is at most int long

      ++size;
      if(!state.string[state.pos_byte]) break;

      // Next line will be the beginning of a paragraph

      para_start = (state.string[state.pos_byte] == '\n');
      width_tar = para_start ? width_1 : width_2;

      // Recreate what the state is at the wrap point, including skipping the
      // wrap character if there was one, and any subsequent leading spaces if
      // there are any and we are in strip_space mode.  If there was no boundary
      // then we're hard breaking and we reset position to the next position.

      if(has_boundary && para_start) {
        state_bound = FANSI_read_next(state_bound, index);
      } else if(!has_boundary) {
        state_bound = state;
      }
      if(strip_spaces) {
        while(state_bound.string[state_bound.pos_byte] == ' ') {
          state_bound = FANSI_read_next(state_bound, index);
      } }
      has_boundary = 0;
      state_bound.pos_width = 0;

      state_prev = state;
      state = state_start = state_bound;
    } else {
      state_prev = state;
      state = state_next;
    }
  }
  // Convert to string and return; this is a little inefficient for the
  // `first_only` mode as ideally we would just return a CHARSXP, but for now we
  // are just trying to keep it simple

  SEXP res;

  if(!first_only) {
    res = PROTECT(allocVector(STRSXP, size));
    char_list = char_list_start;
    for(R_xlen_t i = 0; i < size; ++i) {
      char_list = CDR(char_list); // first element is NULL
      if(char_list == R_NilValue)
        error("Internal Error: wrapped element count mismatch");  // nocov
      SET_STRING_ELT(res, i, CAR(char_list));
    }
    if(CDR(char_list) != R_NilValue)
      error("Internal Error: wrapped element count mismatch 2");  // nocov
  } else {
    // recall there is an extra open PROTECT in first_only mode
    res = res_sxp;
  }
  UNPROTECT(2);
  *sgr_carry = state.sgr;
  return res;
}

/*
 * All integer inputs are expected to be positive, which should be enforced by
 * the R interface checks.
 *
 * @param strict whether to force a hard cut in-word when a full word violates
 *   the width limit on its own
 * @param first_only whether we only want the first line of a wrapped element,
 *   this is to support strtrim. If this is true then the return value becomes a
 *   character vector (STRSXP) rather than a VECSXP
 */

SEXP FANSI_strwrap_ext(
  SEXP x, SEXP width,
  SEXP indent, SEXP exdent,
  SEXP prefix, SEXP initial,
  SEXP wrap_always, SEXP pad_end,
  SEXP strip_spaces,
  SEXP tabs_as_spaces, SEXP tab_stops,
  SEXP warn, SEXP term_cap,
  SEXP first_only,
  SEXP ctl, SEXP norm, SEXP carry,
  SEXP terminate
) {
  FANSI_val_args(x, norm, carry);
  // FANSI_state_init does validations too
  if(
    TYPEOF(width) != INTSXP ||
    TYPEOF(indent) != INTSXP || TYPEOF(exdent) != INTSXP ||
    TYPEOF(prefix) != STRSXP || TYPEOF(initial) != STRSXP ||
    TYPEOF(wrap_always) != LGLSXP ||
    TYPEOF(pad_end) != STRSXP ||
    TYPEOF(strip_spaces) != LGLSXP ||
    TYPEOF(tabs_as_spaces) != LGLSXP ||
    TYPEOF(tab_stops) != INTSXP ||
    TYPEOF(first_only) != LGLSXP ||
    TYPEOF(terminate) != LGLSXP
  )
    error("Internal Error: arg type error 1; contact maintainer.");  // nocov

  int normalize = asLogical(norm);
  int prt = 0;

  const char * pad = CHAR(asChar(pad_end));
  if(*pad != 0 && (*pad < 0x20 || *pad > 0x7e))
    error(
      "%s%s",
      "Argument `pad.end` must be an empty string or a single ",
      "printable ASCII character."
    );

  // Prepare the leading strings; could turn out to be wasteful if we don't
  // need them all; there are three possible combinations: 1) first line of the
  // entire input with indent, 2) first line of paragraph with prefix and
  // indent, 3) other lines with prefix and exdent.

  struct FANSI_prefix_dat pre_dat_raw, ini_dat_raw,
    ini_first_dat, pre_first_dat, pre_next_dat;

  int indent_int = asInteger(indent);
  int exdent_int = asInteger(exdent);
  int warn_int = asInteger(warn);
  int first_only_int = asInteger(first_only);

  if(indent_int < 0 || exdent_int < 0)
    error("Internal Error: illegal indent/exdent values.");  // nocov

  pre_dat_raw = make_pre(prefix);

  const char * warn_base =
    "`%s` contains unhandled ctrl or UTF-8 sequences (see `?unhandled_ctl`).";
  if(warn_int && pre_dat_raw.warn) warning(warn_base, "prefix");
  if(prefix != initial) {
    ini_dat_raw = make_pre(initial);
    if(warn_int && ini_dat_raw.warn) warning(warn_base, "initial");
  } else ini_dat_raw = pre_dat_raw;

  ini_first_dat = pad_pre(ini_dat_raw, indent_int);

  if(initial != prefix) {
    pre_first_dat = pad_pre(pre_dat_raw, indent_int);
  } else pre_first_dat = ini_first_dat;

  if(indent_int != exdent_int) {
    pre_next_dat = pad_pre(pre_dat_raw, exdent_int);
  } else pre_next_dat = pre_first_dat;

  // Set up the buffer, this will be created in FANSI_strwrap, but we want a
  // handle for it here so we can re-use.
  // WARNING: must be after pad_pre as pad_pre uses R_alloc.
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  // Strip whitespaces as needed; `strwrap` doesn't seem to do this with prefix
  // and initial, so we don't either
  int strip_spaces_int = asInteger(strip_spaces);
  if(strip_spaces_int)
    x = PROTECT(FANSI_process(x, term_cap, ctl, &buff)); ++prt;

  // and tabs
  if(asInteger(tabs_as_spaces)) {
    x = PROTECT(FANSI_tabs_as_spaces(x, tab_stops, &buff, warn, term_cap, ctl));
    ++prt;
    prefix = PROTECT(
      FANSI_tabs_as_spaces(prefix, tab_stops, &buff, warn, term_cap, ctl)
    ); ++prt;
    initial = PROTECT(
      FANSI_tabs_as_spaces(initial, tab_stops, &buff, warn, term_cap, ctl)
    ); ++prt;
  }

  // Check that widths are feasible, although really only relevant if in strict
  // mode
  int width_int = asInteger(width);
  int wrap_always_int = asInteger(wrap_always);

  if(
    wrap_always_int && (
      ini_first_dat.width >= width_int ||
      pre_first_dat.width >= width_int ||
      pre_next_dat.width >= width_int
    )
  )
    error(
      "%s%s",
      "Width error: sum of `indent` and `initial` width or sum of `exdent` ",
      "and `prefix` width must be less than `width - 1` when in `wrap.always`."
    );

  // Prep for carry
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  struct FANSI_sgr sgr_carry = FANSI_carry_init(carry, warn, term_cap, ctl);

  // Could be a little faster avoiding this allocation if it turns out nothing
  // needs to be wrapped and we're in simplify=TRUE, but that seems like a lot
  // of work for a rare event
  R_xlen_t i, x_len = XLENGTH(x);
  SEXP res;

  if(first_only_int) {
    // this is to support trim mode
    res = PROTECT(allocVector(STRSXP, x_len)); ++prt;
  } else {
    res = PROTECT(allocVector(VECSXP, x_len)); ++prt;
  }

  // Wrap each element
  for(i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP chr = STRING_ELT(x, i);
    if(chr == NA_STRING) continue;

    SEXP str_i = PROTECT(
      strwrap(
        x, width_int,
        i ? pre_first_dat : ini_first_dat,
        pre_next_dat,
        wrap_always_int, &buff,
        CHAR(asChar(pad_end)),
        strip_spaces_int,
        warn, term_cap,
        first_only_int,
        ctl, i, normalize,
        do_carry,
        &sgr_carry,
        asLogical(terminate)
    ) );
    if(first_only_int) {
      SET_STRING_ELT(res, i, str_i);
    } else {
      SET_VECTOR_ELT(res, i, str_i);
    }
    UNPROTECT(1);
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}
