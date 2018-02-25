/*
 * Copyright (C) 2018  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Escape Aware String Functions"
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
  const char * x_utf8 = FANSI_string_as_utf8(asChar(x)).string;
  // ideally we would IS_ASCII(x), but that's not available to extensions
  int x_has_utf8 = FANSI_has_utf8(x_utf8);

  // ideally would have an internal interface to strip so we don't need to
  // generate these SEXPs here
  SEXP warn = PROTECT(ScalarInteger(2));
  SEXP what = PROTECT(ScalarInteger(1));
  SEXP x_strip = PROTECT(FANSI_strip(x, what, warn));
  int x_width = R_nchar(
    asChar(x_strip), Width, FALSE, FALSE, "when computing display width"
  );
  // wish we could get this directly from R_nchar, grr

  int x_bytes = strlen(x_utf8);
  int warn_int = getAttrib(x_strip, FANSI_warn_sym) != R_NilValue;

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

  int alloc_size = FANSI_add_int(FANSI_add_int(pre_len, spaces), 1);
  char * res_start = "";
  if(alloc_size > 1) {
    // Can't use buff here because we don't write this string out
    // Rprintf("Allocating pre size %d\n", alloc_size);

    char * res = res_start = R_alloc(alloc_size, sizeof(char));
    memcpy(res, pre_chr, pre_len);
    res += pre_len;
    for(int i = 0; i < spaces; ++i) *(res++) = ' ';
    *res = '\0';
  }
  dat.string = (const char *) res_start;
  dat.bytes = FANSI_add_int(dat.bytes, spaces);
  dat.width = FANSI_add_int(dat.width, spaces);
  dat.indent = FANSI_add_int(dat.indent, spaces);

  return dat;
}
/*
 * Adjusts width and sizes to pretend there is no indent.  String itself is not
 * modified so this only works if whatever is using the string is using the byte
 * counter to limit how much of the string it reads
 */

static struct FANSI_prefix_dat drop_pre_indent(struct FANSI_prefix_dat dat) {
  dat.bytes = FANSI_add_int(dat.bytes, -dat.indent);
  dat.width = FANSI_add_int(dat.width, -dat.indent);
  dat.indent = FANSI_add_int(dat.indent, -dat.indent);
  if(dat.indent < 0)
    error(
      "Internal Error: cannot drop indent when there is none; contact ",
      "maintainer."
    );
  return dat;
}
/*
 * Write a line
 *
 * @param state_bound the point where the boundary is
 * @param state_start the starting point of the line
 */

SEXP FANSI_writeline(
  struct FANSI_state state_bound, struct FANSI_state state_start,
  struct FANSI_buff * buff,
  struct FANSI_prefix_dat pre_dat,
  int tar_width, const char * pad_chr
) {
  // Rprintf("  Writeline start with buff %p\n", *buff);

  // Check if we are in a CSI state b/c if we are we neeed extra room for
  // the closing state tag

  int needs_close = FANSI_state_has_style(state_bound);
  int needs_start = FANSI_state_has_style(state_start);

  // state_bound.pos_byte 1 past what we need, so this should include room
  // for NULL terminator

  int target_size = state_bound.pos_byte - state_start.pos_byte;
  int target_width = state_bound.pos_width - state_start.pos_width;
  int target_pad = 0;

  if(!target_size) {
    // handle corner case for empty strings that don't get indented by strwrap;
    // we considered testing width instead of size as that would also prevent
    // indent on thing that just have ESCs, but decided against it (arbitrarily)
    //
    // We do not re-terminate the string, instead relying on widths / sizes to
    // make sure only the non-indent bit is copied

    pre_dat = drop_pre_indent(pre_dat);
  }
  // If we are going to pad the end, adjust sizes and widths

  if(target_width <= tar_width && *pad_chr) {
    target_pad = tar_width - target_width;
    target_width = FANSI_add_int(tar_width, target_pad);
    target_size = FANSI_add_int(target_size, target_pad);
  }
  target_size = FANSI_add_int(target_size, pre_dat.bytes);
  int state_start_size = 0;

  if(needs_close) target_size = FANSI_add_int(target_size, 4);
  if(needs_start) {
    state_start_size = FANSI_state_size(state_start);
    target_size = FANSI_add_int(target_size, state_start_size);
  }
  target_size = FANSI_add_int(target_size, 1); // for NULL terminator

  // Rprintf("target size %d\n", target_size);
  // Make sure buffer is large enough
  FANSI_size_buff(buff, target_size);

  char * buff_track = buff->buff;

  // Apply prevous CSI style

  if(needs_start) {
    // Rprintf("  writing start: %d\n", state_start_size);
    FANSI_csi_write(buff_track, state_start, state_start_size);
    buff_track += state_start_size;
  }
  // Apply indent/exdent prefix/initial

  if(pre_dat.bytes) {
    // Rprintf("  writing pre %s of size %d\n", pre, pre_size);
    memcpy(buff_track, pre_dat.string, pre_dat.bytes);
    buff_track += pre_dat.bytes;
  }
  // Actual string, remember state_bound.pos_byte is one past what we need

  memcpy(
    buff_track, state_start.string + state_start.pos_byte,
    state_bound.pos_byte - state_start.pos_byte
  );
  buff_track += state_bound.pos_byte - state_start.pos_byte;

  // Add padding if needed

  while(target_pad--) {
    *(buff_track++) = *pad_chr;
  }
  // And turn off CSI styles if needed

  if(needs_close) {
    // Rprintf("  close\n");
    memcpy(buff_track, "\033[0m", 4);
    buff_track += 4;
  }
  *buff_track = 0;
  // Rprintf("written %d\n", buff_track - (buff->buff) + 1);

  // Now create the charsxp and append to the list, start by determining
  // what encoding to use.  If pos_byte is greater than pos_ansi it means
  // we must have hit a UTF8 encoded character

  cetype_t chr_type = CE_NATIVE;
  if((state_bound.has_utf8 || pre_dat.has_utf8)) chr_type = CE_UTF8;

  /*
  Rprintf(
    "making string: '%s' len '%d' size '%d'\n",
    buff->buff, (int) (buff_track - buff->buff), target_size
  );
  */
  SEXP res_sxp = PROTECT(
    mkCharLenCE(
      buff->buff, (int) (buff_track - buff->buff), chr_type
  ) );
  UNPROTECT(1);
  return res_sxp;
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
  const char * x, int width,
  struct FANSI_prefix_dat pre_first,
  struct FANSI_prefix_dat pre_next,
  int wrap_always,
  struct FANSI_buff * buff,
  const char * pad_chr,
  int strip_spaces,
  SEXP warn, SEXP term_cap,
  int first_only
) {
  struct FANSI_state state = FANSI_state_init(x, warn, term_cap);

  int width_1 = FANSI_add_int(width, -pre_first.width);
  int width_2 = FANSI_add_int(width, -pre_next.width);

  int width_tar = width_1;

  if(width < 1) error("Internal Error: invalid width.");
  if(width_1 < 0 || width_2 < 0)
    error("Internal Error: incompatible width/indent/prefix.");

  // Use LISTSXP so we don't have to do a two pass process to determine how many
  // items we're going to have, unless we're in first only in which case we know
  // we only need one element per and don't actually use these

  SEXP char_list_start, char_list;
  char_list_start = char_list = PROTECT(list1(R_NilValue));

  int prev_newline = 0;     // tracks if last non blank character was newline
  int prev_boundary = 0;    // tracks if previous char was a boundary
  int has_boundary = 0;     // tracks if at least one boundary in a line
  int para_start = 1;

  // Need to keep track of where word boundaries start and end due to
  // possibility for multiple elements between words

  struct FANSI_state state_start, state_bound;
  state_start = state_bound = state;
  R_xlen_t size = 0;
  SEXP res_sxp;

  while(1) {
    const char cur_chr = state.string[state.pos_byte];
    struct FANSI_state state_next;

    // Can no longer advance after we reach end, but we still need to assemble
    // strings so we assign `state` even though technically not correct

    int string_over = !state.string[state.pos_byte];

    if(string_over) state_next = state;
    else state_next = FANSI_read_next(state);

    /*
    Rprintf(
      "byte: %d width: %d chr: '%c'\n", state.pos_byte - state_start.pos_byte,
      state.pos_width,  cur_chr
    );
    */
    // detect word boundaries and paragraph starts; we need to track
    // state_bound for the special case where we are in strip space mode
    // and we happen to hit the width in a two space sequence such as we might
    // get after [.!?].

    if(cur_chr == ' ' || cur_chr == '\t' || cur_chr == '\n') {
      if(strip_spaces && !prev_boundary) state_bound = state;
      else if(!strip_spaces) state_bound = state;
      has_boundary = prev_boundary = 1;
      // Rprintf("Bound @ %d\n", state_bound.pos_byte - state_start.pos_byte);
    } else {
      prev_boundary = 0;
      prev_newline = 0;
    }
    // Write the line

    if(
      string_over ||
      (cur_chr == '\n' && !first_only) ||  // newlines kept in strtrim mode
      (
        state.pos_width >= width_tar &&
        state_next.pos_width > state.pos_width &&  // keep going w/ zero width
        (has_boundary || wrap_always)
      )
    ) {
      if(string_over || (wrap_always && !has_boundary) || first_only) {
        state_bound = state;
      }
      res_sxp = PROTECT(
        FANSI_writeline(
          state_bound, state_start, buff,
          para_start ? pre_first : pre_next,
          width_tar, pad_chr
        )
      );
      // first_only for `strtrim`

      if(!first_only) {
        SETCDR(char_list, list1(res_sxp));
        char_list = CDR(char_list);
        UNPROTECT(1);
      } else break;

      if(cur_chr == '\n') prev_newline = 1;

      // overflow should be impossible here since string is at most int long

      ++size;
      if(string_over) break;

      // Next line will be the beginning of a paragraph

      para_start = (cur_chr == '\n');

      // Recreate what the state is at the wrap point, including skipping the
      // wrap character if there was one, and any subsequent leading spaces if
      // there are any and we are in strip_space mode.

      if(
        has_boundary && state_bound.string[state_bound.pos_byte] == '\n'
      ) {
        state_bound = FANSI_read_next(state_bound);
      }
      if(strip_spaces) {
        while(state_bound.string[state_bound.pos_byte] == ' ') {
          state_bound = FANSI_read_next(state_bound);
      } }
      has_boundary = 0;
      state_bound.pos_width = 0;
      state = state_start = state_bound;
    } else {
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
  SEXP first_only
) {
  if(
    TYPEOF(x) != STRSXP || TYPEOF(width) != INTSXP ||
    TYPEOF(indent) != INTSXP || TYPEOF(exdent) != INTSXP ||
    TYPEOF(prefix) != STRSXP || TYPEOF(initial) != STRSXP ||
    TYPEOF(wrap_always) != LGLSXP ||
    TYPEOF(pad_end) != STRSXP ||
    TYPEOF(warn) != LGLSXP || TYPEOF(term_cap) != INTSXP ||
    TYPEOF(strip_spaces) != LGLSXP ||
    TYPEOF(tabs_as_spaces) != LGLSXP ||
    TYPEOF(tab_stops) != INTSXP ||
    TYPEOF(first_only) != LGLSXP
  ) {
    error("Internal Error: arg type error 1; contact maintainer.");
  }

  const char * pad = CHAR(asChar(pad_end));
  if(*pad != 0 && (*pad < 0x20 || *pad > 0x7e))
    error(
      "%s%s",
      "Argument `pad.end` must be an empty string or a single ",
      "printable ASCII character."
    );

  // Set up the buffer, this will be created in FANSI_strwrap, but we want a
  // handle for it here so we can re-use

  struct FANSI_buff buff = {.len = 0};

  // Strip whitespaces as needed; `strwrap` doesn't seem to do this with prefix
  // and initial, so we don't either

  int strip_spaces_int = asInteger(strip_spaces);

  if(strip_spaces_int) x = PROTECT(FANSI_process(x, &buff));
  else PROTECT(x);

  // and tabs

  if(asInteger(tabs_as_spaces)) {
    x = PROTECT(FANSI_tabs_as_spaces(x, tab_stops, &buff, warn, term_cap));
    prefix = PROTECT(
      FANSI_tabs_as_spaces(prefix, tab_stops, &buff, warn, term_cap)
    );
    initial = PROTECT(
      FANSI_tabs_as_spaces(initial, tab_stops, &buff, warn, term_cap)
    );
  }
  else x = PROTECT(PROTECT(PROTECT(x)));  // PROTECT stack balance

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
    "`%s` contains illegal escape sequences (see `?illegal_esc`).";
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
      "Width error: sum of `indent` and `initial` width or sum of `exdent` and",
      "`prefix` width must be less than `width` when in `wrap.always`."
    );

  // Could be a little faster avoiding this allocation if it turns out nothing
  // needs to be wrapped and we're in simplify=TRUE, but that seems like a lot
  // of work for a rare event

  R_xlen_t i, x_len = XLENGTH(x);
  SEXP res;

  if(first_only_int) {
    // this is to support trim mode
    res = PROTECT(allocVector(STRSXP, x_len));
  } else {
    res = PROTECT(allocVector(VECSXP, x_len));
  }
  // Wrap each element

  for(i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP chr = STRING_ELT(x, i);
    if(chr == NA_STRING) continue;
    SEXP str_i = PROTECT(
      strwrap(
        CHAR(chr), width_int,
        i ? pre_first_dat : ini_first_dat,
        pre_next_dat,
        wrap_always_int, &buff,
        CHAR(asChar(pad_end)),
        strip_spaces_int,
        warn, term_cap,
        first_only_int
    ) );
    if(first_only_int) {
      SET_STRING_ELT(res, i, str_i);
    } else {
      SET_VECTOR_ELT(res, i, str_i);
    }
    UNPROTECT(1);
  }
  UNPROTECT(5);
  return res;
}
