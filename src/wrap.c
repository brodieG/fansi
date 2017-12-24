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

  const char * x_utf8 = FANSI_string_as_utf8(asChar(x), is_utf8_loc);
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
static char * make_pre(const char * pre_chr, int pre_len, int spaces) {
  int alloc_size = FANSI_add_int(FANSI_add_int(pre_len, spaces), 1);
  Rprintf("Allocating pre size %d\n", alloc_size);
  char * res = R_alloc(alloc_size, sizeof(char));
  char * res_start = res;
  for(int i = 0; i < spaces; ++i) *(res++) = ' ';
  memcpy(res, pre_chr, pre_len);
  *(res + pre_len + 1) = '\0';
  return res_start;
}
/*
 * Write a line
 */

SEXP FANSI_writeline(
  struct FANSI_state state, struct FANSI_state state_bound,
  struct FANSI_state state_start, char ** buff, int * buff_size,
  const char * pre, int pre_size, int pre_has_utf8, int is_utf8_loc
) {
  char * buff_target = * buff;

  // Check if we are in a CSI state b/c if we are we neeed extra room for
  // the closing state tag

  int needs_close = FANSI_state_has_style(state);
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
  if(target_size > *buff_size) {
    // don't do re-alloc because we are going to overwrite everything
    // anyway, double prev buffer unless entry is bigger than that

    int tmp_double_size = FANSI_add_int(*buff_size, *buff_size);
    if(target_size > tmp_double_size) tmp_double_size = target_size;
    *buff_size = tmp_double_size;
    buff_target = R_alloc(*buff_size, sizeof(char));
  }
  const char * buff_target_start = buff_target;

  // Apply prevous CSI style

  if(needs_start) {
    FANSI_csi_write(buff_target, state_start, state_start_size);
    buff_target += state_start_size;
  }
  // Apply indent/exdent prefix/initial

  memcpy(buff_target, pre, pre_size);
  buff_target += pre_size;

  // Actual string, remember state_bound.pos_byte is one past what we need

  memcpy(
    buff_target, state_start.string + state_start.pos_byte,
    state_bound.pos_byte - state_start.pos_byte
  );
  buff_target += state_bound.pos_byte - state_start.pos_byte;

  // And turn off CSI styles if needed

  if(needs_close) {
    memcpy(buff_target + state_bound.pos_byte, "\033[0m", 4);
    buff_target += 4;
  }
  *buff_target = 0;

  // Now create the charsxp and append to the list, start by determining
  // what encoding to use.  If pos_byte is greater than pos_ansi it means
  // we must have hit a UTF8 encoded character

  cetype_t chr_type = CE_NATIVE;
  if((state.has_utf8 || pre_has_utf8) && !is_utf8_loc) {
    chr_type = CE_UTF8;
  }
  SEXP res_sxp = PROTECT(
    mkCharLenCE(
      buff_target_start, (int) (buff_target - buff_target_start), chr_type
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
 * @param buff a pointer to a character buffer.  We use pointer to a
 *   pointer because it may need to be resized, but we also don't want to
 *   re-allocate the buffer between calls.
 * @param buff_size the size of the buffer
 * @param strict whether to hard wrap at width or not (not is what strwrap does
 *   by default)
 * @param is_utf8_loc whether the current locale is UTF8
 */

SEXP FANSI_strwrap(
  const char * x, int width, int indent, int exdent,
  struct FANSI_prefix_dat prefix,
  struct FANSI_prefix_dat initial,
  int strict, char ** buff, int * buff_size, int is_utf8_loc
) {
  Rprintf("Internal wrap\n");
  char * buff_target = * buff;
  Rprintf("initialize state\n");
  struct FANSI_state state = FANSI_state_init();
  state.string = x;

  Rprintf("State initialized\n");

  int width_1_tmp = FANSI_add_int(indent, initial.width);
  int width_1 = FANSI_add_int(width, -width_1_tmp);
  int width_2_tmp = FANSI_add_int(exdent, prefix.width);
  int width_2 = FANSI_add_int(width, -width_2_tmp);

  int width_tar = width_1;

  Rprintf("Making pre\n");
  const char * para_start_chr = make_pre(initial.string, initial.width, indent);
  int para_start_size = FANSI_add_int(initial.width, indent);
  const char * para_next_chr = make_pre(prefix.string, prefix.width, exdent);
  int para_next_size = FANSI_add_int(prefix.width, exdent);
  Rprintf("done with pre\n");

  if(width < 1) error("Internal Error: invalid width.");
  if(width_1 < 0 || width_2 < 0)
    error("Internal Error: incompatible width/indent/prefix.");

  // Start with a buffer twice the width we're targeting; we'll grow it as
  // needed.  The buffer may already exist from a previous iteration

  if(!buff_target) {
    Rprintf("Allocate initial size to %d\n", FANSI_add_int(width, width));
    *buff_size = FANSI_add_int(width, width);
    buff_target = R_alloc(*buff_size, sizeof(char));
    Rprintf("Done buff alloc\n");
  }
  // Use LISTSXP so we don't have to do a two pass process to determine how many
  // items we're going to have

  SEXP char_list_start, char_list;
  char_list_start = char_list = PROTECT(list1(R_NilValue));
  int prev_newline = 0;     // tracks if last non blank character was newline
  int prev_boundary = 0;    // tracks if previous char was a boundary
  int para_start = 1;
  int written_through = -1; // which byte was last copied

  struct FANSI_state state_start = state;
  struct FANSI_state state_bound = state;
  R_xlen_t size = 0;

  Rprintf("Start reading chars with tar width %d\n", width_tar);
  while(state.string[state.pos_byte]) {
    Rprintf(
      "byte: %d width: %d\n", state.pos_byte - state_start.pos_byte,
      state.pos_width
    );
    const char cur_chr = state.string[state.pos_byte];

    // detect word boundaries and paragraph starts

    if(cur_chr == ' ' || cur_chr == '\t' || cur_chr == '\n') {
      if(!prev_boundary) state_bound = state;
      prev_boundary = 1;
      Rprintf("Boundary at %d\n", state_bound.pos_byte - state_start.pos_byte);
    } else {
      prev_boundary = 0;
      prev_newline = 0;
    }
    // Write a line, need special logic for newlines because any whitespace
    // following newlines is normally just suppressed.

    if(
      (cur_chr == '\n' && !prev_newline) || state.pos_width >= width_tar
    ) {
      if(cur_chr == '\n') prev_newline = 1;

      SEXP res_sxp = PROTECT(
        FANSI_writeline(
          state, state_bound, state_start, buff, buff_size,
          para_start ? para_start_chr : para_next_chr,
          para_start ? para_start_size : para_next_size,
          para_start ? initial.has_utf8 : prefix.has_utf8,
          is_utf8_loc
        )
      );
      Rprintf("Writing '%s'\n", CHAR(res_sxp));
      SETCDR(char_list, list1(res_sxp));
      UNPROTECT(1);

      // overflow should be impossible here since string is at most int long
      ++size;

      // Next line will be the beginning of a paragraph
      para_start = (cur_chr == '\n');

      // Recreate what the state is at the wrap point

      written_through = state_bound.pos_byte - 1;
      state_bound.pos_width = 0;
      state = state_start = state_bound;
    }
    // NOTE: Need to handle carriage returns

    state = FANSI_read_next(state);
  }
  // Write last bit of string

  if(written_through < state.pos_byte) {
    SEXP res_sxp = PROTECT(
      FANSI_writeline(
        state, state_bound, state_start, buff, buff_size,
        para_start ? para_start_chr : para_next_chr,
        para_start ? para_start_size : para_next_size,
        para_start ? initial.has_utf8 : prefix.has_utf8,
        is_utf8_loc
      )
    );
    SETCDR(char_list, list1(res_sxp));
    UNPROTECT(1);
  }
  SEXP res = PROTECT(allocVector(STRSXP, size));
  char_list = char_list_start;
  for(R_xlen_t i = 0; i < size; ++i) {
    char_list = CDR(char_list); // first element is NULL
    if(char_list == R_NilValue)
      error("Internal Error: wrapped element count mismatch");  // nocov
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
  SEXP prefix, SEXP initial, SEXP strict
) {

  Rprintf("Start wrap ext\n");
  if(
    TYPEOF(x) != STRSXP || TYPEOF(width) != INTSXP ||
    TYPEOF(indent) != INTSXP || TYPEOF(exdent) != INTSXP ||
    TYPEOF(prefix) != STRSXP || TYPEOF(initial) != STRSXP ||
    TYPEOF(strict) != LGLSXP
  ) {
    error("Type error.");
  }
  R_xlen_t i, x_len = XLENGTH(x);

  int strict_int = asInteger(strict);
  int is_utf8_loc = FANSI_is_utf8_loc();

  Rprintf("compute pre\n");

  struct FANSI_prefix_dat pre_dat = compute_pre(prefix, is_utf8_loc);
  struct FANSI_prefix_dat ini_dat = compute_pre(initial, is_utf8_loc);

  Rprintf("done compute pre\n");

  // Check that widths are feasible, although really only relevant if in strict
  // mode

  int width_int = asInteger(width);
  int indent_int = asInteger(indent);
  int exdent_int = asInteger(exdent);

  if(
    strict_int && (
      FANSI_add_int(indent_int, ini_dat.width) >= width_int ||
      FANSI_add_int(exdent_int, pre_dat.width) >= width_int
    )
  )
    error(
      "%s%s",
      "Width error: sum of `indent` and `initial` width or sum of `exdent` and",
      "`prefix` width must be less than `width` when in strict mode."
    );

  SEXP res = PROTECT(allocVector(VECSXP, x_len));

  // Set up the buffer, this will be created in FANSI_strwrap, but we want a
  // handle for it here so we can re-use

  char * buff_tmp = 0;
  char ** buff = &buff_tmp;
  int buff_size_tmp = 0;
  int * buff_size = &buff_size_tmp;

  Rprintf("Start loop\n");

  for(i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP str_i = PROTECT(
      FANSI_strwrap(
        CHAR(STRING_ELT(x, i)), width_int, indent_int, exdent_int,
        pre_dat, ini_dat, strict_int, buff, buff_size, is_utf8_loc
    ) );
    SET_VECTOR_ELT(res, i, str_i);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res;
}
