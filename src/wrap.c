#include "fansi.h"
/*
 * Data related to prefix / initial
 */

struct FANSI_prefix_dat {
  const char * string;  // string translated to utf8
  int width;            // display width as computed by R_nchar
  int bytes;            // bytes, excluding NULL terminator
  int has_utf8;         // whether utf8 contains value > 127
};

/*
 * Generate data related to prefix / initial
 */

static struct FANSI_prefix_dat compute_pre(SEXP x, int is_utf8_loc) {

  const char * x_utf8 = FANSI_string_as_utf8(asChar(x), is_utf8_loc).buff;
  int x_has_utf8 = FANSI_has_utf8(x_utf8);

  SEXP x_strip = PROTECT(FANSI_strip(x));
  int x_width = R_nchar(
    asChar(x_strip), Width, FALSE, FALSE, "when computing display width"
  );

  int x_bytes = strlen(x_utf8);

  UNPROTECT(1);
  return (struct FANSI_prefix_dat) {
    .string=x_utf8, .width=x_width, .bytes=x_bytes, .has_utf8=x_has_utf8
  };
}
/*
 * Combine initial and indent (or prefix and exdent)
 */
static const char * make_pre(const char * pre_chr, int pre_len, int spaces) {
  int alloc_size = FANSI_add_int(FANSI_add_int(pre_len, spaces), 1);
  char * res_start = "";
  if(alloc_size > 1) {
    // Rprintf("Allocating pre size %d\n", alloc_size);
    char * res = res_start = R_alloc(alloc_size, sizeof(char));
    for(int i = 0; i < spaces; ++i) *(res++) = ' ';
    memcpy(res, pre_chr, pre_len);
    *(res + pre_len + 1) = '\0';
  }
  return (const char *) res_start;
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
  const char * pre, int pre_size, int pre_has_utf8, int is_utf8_loc
) {
  // Rprintf("  Writeline start with buff %p\n", *buff);

  // Check if we are in a CSI state b/c if we are we neeed extra room for
  // the closing state tag

  int needs_close = FANSI_state_has_style(state_bound);
  int needs_start = FANSI_state_has_style(state_start);

  // state_bound.pos_byte 1 past what we need, so this should include room
  // for NULL terminator

  int target_size = FANSI_add_int(
    state_bound.pos_byte - state_start.pos_byte, pre_size
  );
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

  /*
  */
  if(needs_start) {
    // Rprintf("  writing start: %d\n", state_start_size);
    FANSI_csi_write(buff_track, state_start, state_start_size);
    buff_track += state_start_size;
  }
  // Apply indent/exdent prefix/initial

  if(pre_size) {
    // Rprintf("  writing pre %s of size %d\n", pre, pre_size);
    memcpy(buff_track, pre, pre_size);
    buff_track += pre_size;
  }
  // Actual string, remember state_bound.pos_byte is one past what we need

  /*
  Rprintf(
    "  string start %d nchar %d\n",
    state_start.pos_byte,
    state_bound.pos_byte - state_start.pos_byte
  );
  */
  memcpy(
    buff_track, state_start.string + state_start.pos_byte,
    state_bound.pos_byte - state_start.pos_byte
  );
  buff_track += state_bound.pos_byte - state_start.pos_byte;

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
  if((state_bound.has_utf8 || pre_has_utf8) && !is_utf8_loc) {
    chr_type = CE_UTF8;
  }
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
 * @param strict whether to hard wrap at width or not (not is what strwrap does
 *   by default)
 * @param is_utf8_loc whether the current locale is UTF8
 */

SEXP FANSI_strwrap(
  const char * x, int width, int indent, int exdent,
  struct FANSI_prefix_dat prefix,
  struct FANSI_prefix_dat initial,
  int wrap_always,
  struct FANSI_buff * buff,
  int is_utf8_loc
) {
  // Rprintf("start wrap\n");
  struct FANSI_state state = FANSI_state_init();
  state.string = x;

  int width_1_tmp = FANSI_add_int(indent, initial.width);
  int width_1 = FANSI_add_int(width, -width_1_tmp);
  int width_2_tmp = FANSI_add_int(exdent, prefix.width);
  int width_2 = FANSI_add_int(width, -width_2_tmp);

  int width_tar = width_1;

  const char * para_start_chr = make_pre(initial.string, initial.bytes, indent);
  int para_start_size = FANSI_add_int(initial.bytes, indent);
  const char * para_next_chr = make_pre(prefix.string, prefix.bytes, exdent);
  int para_next_size = FANSI_add_int(prefix.bytes, exdent);

  if(width < 1) error("Internal Error: invalid width.");
  if(width_1 < 0 || width_2 < 0)
    error("Internal Error: incompatible width/indent/prefix.");

  // Use LISTSXP so we don't have to do a two pass process to determine how many
  // items we're going to have

  SEXP char_list_start, char_list;
  char_list_start = char_list = PROTECT(list1(R_NilValue));
  int prev_newline = 0;     // tracks if last non blank character was newline
  int prev_boundary = 0;    // tracks if previous char was a boundary
  int has_boundary = 0;     // tracks if at least one boundary in a line
  int para_start = 1;

  // Need to keep track of where word boundaries start and end due to
  // possibility for multiple elements between words

  struct FANSI_state state_start, state_bound, state_bound_end;
  state_start = state_bound = state_bound_end = state;
  R_xlen_t size = 0;

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
    // detect word boundaries and paragraph starts

    if(cur_chr == ' ' || cur_chr == '\t' || cur_chr == '\n') {
      if(!prev_boundary) state_bound = state_bound_end = state;
      else state_bound_end = state;
      has_boundary = prev_boundary = 1;
      // Rprintf("Bound @ %d\n", state_bound.pos_byte - state_start.pos_byte);
    } else {
      prev_boundary = 0;
      prev_newline = 0;
    }
    // Write a line, need special logic for newlines because any whitespace
    // following newlines is normally just suppressed (actually, is this still
    // necessary given we now process the string?).

    if(
      string_over ||
      (cur_chr == '\n') ||
      (
        state.pos_width >= width_tar &&
        state_next.pos_width > state.pos_width &&  // keep going w/ zero width
        (has_boundary || wrap_always)
      )
    ) {
      SEXP res_sxp;

      if(prev_newline && cur_chr == '\n') {
        res_sxp = PROTECT(R_BlankString);
      } else {
        if(string_over || (wrap_always && !has_boundary)) {
          state_bound = state;
        }
        res_sxp = PROTECT(
          FANSI_writeline(
            state_bound, state_start, buff,
            para_start ? para_start_chr : para_next_chr,
            para_start ? para_start_size : para_next_size,
            para_start ? initial.has_utf8 : prefix.has_utf8,
            is_utf8_loc
          )
        );
      }
      if(cur_chr == '\n') prev_newline = 1;
      SETCDR(char_list, list1(res_sxp));
      char_list = CDR(char_list);
      UNPROTECT(1);

      // overflow should be impossible here since string is at most int long

      ++size;
      if(string_over) break;

      // Next line will be the beginning of a paragraph

      para_start = (cur_chr == '\n');

      // Recreate what the state is at the wrap point, including skipping the
      // wrap character if there was one

      if(has_boundary) state_bound_end = FANSI_read_next(state_bound_end);

      has_boundary = 0;
      state_bound_end.pos_width = 0;
      state = state_start = state_bound = state_bound_end;
    } else {
      state = state_next;
    }
  }
  // Write last bit of string

  /*
  Rprintf(
    "Do we have extra %d %d %s", written_through , state.pos_byte,
    state.string + state_start.pos_byte
  );
  */

  SEXP res = PROTECT(allocVector(STRSXP, size));
  char_list = char_list_start;
  for(R_xlen_t i = 0; i < size; ++i) {
    char_list = CDR(char_list); // first element is NULL
    if(char_list == R_NilValue)
      error("Internal Error: wrapped element count mismatch");  // nocov
    // Rprintf("string: '%s'\n", CHAR(CAR(char_list)));
    SET_STRING_ELT(res, i, CAR(char_list));
  }
  if(CDR(char_list) != R_NilValue)
    error("Internal Error: wrapped element count mismatch 2");  // nocov
  UNPROTECT(2);
  return res;
}

/*
 * All integer inputs are expected to be positive, which should be enforced by
 * the R interface checks.
 *
 * @param strict whether to force a hard cut in-word when a full word violates
 *   the width limit on its own
 */

SEXP FANSI_strwrap_ext(
  SEXP x, SEXP width,
  SEXP indent, SEXP exdent,
  SEXP prefix, SEXP initial,
  SEXP wrap_always, SEXP pad_end,
  SEXP strip_spaces,
  SEXP tabs_as_spaces, SEXP tab_stops
) {
  if(
    TYPEOF(x) != STRSXP || TYPEOF(width) != INTSXP ||
    TYPEOF(indent) != INTSXP || TYPEOF(exdent) != INTSXP ||
    TYPEOF(prefix) != STRSXP || TYPEOF(initial) != STRSXP ||
    TYPEOF(wrap_always) != LGLSXP || TYPEOF(pad_end) != LGLSXP ||
    TYPEOF(strip_spaces) != LGLSXP ||
    TYPEOF(tabs_as_spaces) != LGLSXP || TYPEOF(tab_stops) != INTSXP
  ) {
    error("Type error.");
  }
  R_xlen_t i, x_len = XLENGTH(x);

  int wrap_always_int = asInteger(wrap_always);
  int is_utf8_loc = FANSI_is_utf8_loc();

  struct FANSI_prefix_dat pre_dat = compute_pre(prefix, is_utf8_loc);
  struct FANSI_prefix_dat ini_dat = compute_pre(initial, is_utf8_loc);

  // Check that widths are feasible, although really only relevant if in strict
  // mode

  int width_int = asInteger(width);
  int indent_int = asInteger(indent);
  int exdent_int = asInteger(exdent);

  if(
    wrap_always_int && (
      FANSI_add_int(indent_int, ini_dat.width) >= width_int ||
      FANSI_add_int(exdent_int, pre_dat.width) >= width_int
    )
  )
    error(
      "%s%s",
      "Width error: sum of `indent` and `initial` width or sum of `exdent` and",
      "`prefix` width must be less than `width` when in `wrap.always`."
    );

  SEXP res = PROTECT(allocVector(VECSXP, x_len));

  // Set up the buffer, this will be created in FANSI_strwrap, but we want a
  // handle for it here so we can re-use

  struct FANSI_buff buff = {.len = 0};

  // Strip whitespaces as needed

  x = PROTECT(FANSI_process(x, &buff));

  // and tabs

  if(asInteger(tabs_as_spaces))
    x = PROTECT(FANSI_tabs_as_spaces(x, tab_stops, &buff, is_utf8_loc));
  else x = PROTECT(x);

  for(i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP chr = STRING_ELT(x, i);
    if(chr == NA_STRING) continue;
    SEXP str_i = PROTECT(
      FANSI_strwrap(
        CHAR(chr), width_int, indent_int, exdent_int,
        pre_dat, ini_dat, wrap_always_int, &buff, is_utf8_loc
    ) );
    SET_VECTOR_ELT(res, i, str_i);
    UNPROTECT(1);
  }
  UNPROTECT(3);
  return res;
}
