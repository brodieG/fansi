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
  SEXP x_strip = PROTECT(FANSI_strip(x));
  const char * x_utf8 = FANSI_string_as_utf8(asChar(x), is_utf8_loc);
  int x_has_utf8 = FANSI_has_utf8(x_utf8);
  int x_width = R_nchar(
    x_strip, Width, FALSE, FALSE, "when computing display width"
  );
  int x_bytes = strlen(x_utf8);

  SEXP x_chrsxp = asChar(prefix);
  const char * x_chr = CHAR(x_chrsxp);
  const char * x_chr_strip =
    FANSI_string_as_utf8(asChar(x_strip), is_utf8_loc);

  const char * x_chr_strip_tmp = x_chr;
  int x_has_utf8 = 0;
  while(*x_chr_strip_tmp) {
    if(*x_chr_strip_tmp > 127) {
      x_has_utf8 = 1;
      break;
    }
    ++x_chr_strip_tmp;
  }
  UNPROTECT(1);
  return (struct FANSI_prefix_dat) {
    .string=x_utf8, .width=x_width, .bytes=x_bytes, .has_utf8=x_has_utf8
  };
}
/*
 * Combine initial and indent (or prefix and exdent)
 */
static char * make_pre(const char * pre_chr, int pre_len, int spaces) {
  char * res = R_alloc(pre_len + spaces + 1, sizeof(char));
  char * res_start = res;
  for(int i = 0; i < spaces; ++i) *(res++) = ' ';
  memcpy(res, pre_chr, pre_len);
  *(res + pre_len + 1) = '\0';
  return res_start;
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
  char * buff_target = * buff;
  struct FANSI_state state = FANSI_state_init();
  state.string = x;

  int width_1 = width - indent - initial.width;
  int width_2 = width - exdent - prefix.width;
  int width_tar = width_1;

  const char * para_start_chr = make_pre(initial.string, initial.width, indent);
  int para_start_size = initial.width + indent;
  const char * para_next_chr = make_pre(prefix.string, prefix.width, exdent);
  int para_next_size = prefix.width + exdent;

  if(width < 1) error("Internal Error: invalid width.");
  if(width_1 < 0 || width_2 < 0)
    error("Internal Error: incompatible width/indent/prefix.");

  // Start with a buffer twice the width we're targeting; we'll grow it as
  // needed.  The buffer may already exist from a previous iteration

  if(!buff_target) {
    *buff_size = FANSI_add_int(width, width);
    buff_target = R_alloc(*buff_size, sizeof(char));
  }
  // Track the last valid breaking point we have.  For now we are just looking
  // for spaces, tabs.

  int last_bound = 0;

  // Use LISTSXP so we don't have to do a two pass process to determine how many
  // items we're going to have

  SEXP char_list_start, char_list;
  char_list_start = char_list = PROTECT(list1(R_NilValue));
  int prev_newline = 0;  // tracks if last non blank character was newline
  int para_start = 1;
  int start_byte = state.pos_byte;

  struct FANSI_state state_start = state;
  struct FANSI_state state_blank = FANSI_state_init();  // reference
  R_xlen_t size = 0;

  while(state.string[state.pos_byte]) {
    while(state.pos_width < width_tar) {
      const char cur_chr = state.string[state.pos_byte];

      // detect word boundaries and paragraph starts

      if(cur_chr == ' ' || cur_chr == '\t' || cur_chr == '\n') {
        last_bound = state.pos_byte;
      } else prev_newline = 0;

      // Write a line, need special logic for newlines because any whitespace
      // following newlines is normally just suppressed.

      if((cur_chr == '\n' && !prev_newline) || state.pos_width > width_tar) {
        // Check if we are in a CSI state b/c if we are we neeed extra room for
        // the closing state tag

        int needs_close = FANSI_state_comp(state, state_blank);
        int needs_start = FANSI_state_comp(state_start, state_blank);
        // last_bound 1 past what we need, so this should include room for NULL
        // terminator
        int target_size = FANSI_add_int(
          last_bound - start_byte, 
          (para_start ? para_start_size : para_next_size)
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

        if(cur_chr == '\n') prev_newline = 1;

        // Apply prevous CSI style

        if(needs_start) {
          FANSI_csi_write(buff_target, state_start, state_start_size);
          buff_target += state_start_size;
        }
        // Apply indent/exdent prefix/initial

        if(para_start && para_start_size) {
          memcpy(buff_target, para_start_chr, para_start_size);
          buff_target += para_start_size;
        } else if(!para_start && para_next_size) {
          memcpy(buff_target, para_next_chr, para_next_size);
          buff_target += para_next_size;
        }
        // Actual string, remember last_bound is one past what we need

        memcpy(buff_target, state_start.string, last_bound - start_byte);
        buff_target += last_bound - start_byte;

        // And turn off CSI styles if needed

        if(needs_close) {
          memcpy(buff_target + last_bound, "\033[0m", 4);
          buff_target += 4;
        }
        *buff_target = 0;

        // Now create the charsxp and append to the list, start by determining
        // what encoding to use.  If pos_byte is greater than pos_ansi it means
        // we must have hit a UTF8 encoded character

        cetype_t chr_type = CE_NATIVE;
        if(
          (state.has_utf8 || initial.has_utf8 || prefix.has_utf8) &&
          !is_utf8_loc
        ) {
          chr_type = CE_UTF8;
        }
        SEXP res_sxp = PROTECT(
          mkCharLenCE(
            buff_target_start, (int) (buff_target - buff_target_start), chr_type
        ) );
        SETCDR(char_list, list1(res_sxp));
        UNPROTECT(1);
        // overflow should be impossible here since string is at most int long
        ++size;

        // Next line will be the beginning of a paragraph
        para_start = (cur_chr == '\n');
      }
      warning("Add logic to deal with carriage returns");

      // NOTE: Need to handle carriage returns

      state = FANSI_read_next(state);
    }
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
  UNPROTECT(1);
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
  SEXP x, SEXP width, SEXP indent, SEXP exdent, SEXP prefix,
  SEXP initial, SEXP strict
) {
  if(
    TYPEOF(x) != STRSXP || TYPEOF(width) != INTSXP ||
    TYPEOF(indent) != INTSXP || TYPEOF(exdent) != INTSXP ||
    TYPEOF(prefix) != STRSXP || TYPEOF(strict) != INTSXP
  ) {
    error("Type error.");
  }
  R_xlen_t i, x_len = XLENGTH(x);

  int strict_int = asInteger(strict);

  /*
   * Need to collect a few bits of info:
   * * length in characters width of each string
   * * length in bytes of each string
   * * the actual string translated to UTF8
   * * whether there are UTF8 chars?
   */
  SEXP initial_strip = PROTECT(FANSI_strip(initial));

  int is_utf8_loc = FANSI_is_utf8_loc();
  struct FANSI_prefix_dat = compute_pre(prefix, is_utf8_loc);


  SEXP initial_chrsxp = asChar(initial);
  const char * initial_chr = CHAR(initial_chrsxp);
  const char * initial_chr_strip =
    FANSI_string_as_utf8(asChar(initial_strip), is_utf8_loc);
  SEXP initial_chrsxp_strip = PROTECT(mkChar(initial_chr_strip));
  int initial_chr_len = R_nchar(
    initial_chrsxp_strip, Width, FALSE, FALSE, "when computing display width"
  );
  const char * initial_chr_strip_tmp = CHAR(asChar(initial_strip));
  int initial_has_utf8 = 0;
  while(*initial_chr_strip_tmp) {
    if(*initial_chr_strip_tmp < 0) {
      initial_has_utf8 = 1;
      break;
    }
    ++initial_chr_strip_tmp;
  }
  UNPROTECT(4);

  // Check that widths are feasible, although really only relevant if in strict
  // mode

  int width_int = asInteger(width);
  int indent_int = asInteger(indent);
  int exdent_int = asInteger(exdent);

  if(
    strict_int && (
      FANSI_add_int(indent_int, initial_chr_len) >= width_int ||
      FANSI_add_int(exdent_int, prefix_chr_len) >= width_int
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

  char ** buff = 0;
  int * buff_size = 0;

  for(i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP str_i = PROTECT(
      FANSI_strwrap(
        CHAR(STRING_ELT(x, i)), width_int, indent_int, exdent_int,
        prefix_chr, prefix_chr_len, prefix_has_utf8,
        initial_chr, initial_chr_len, initial_has_utf8,
        strict_int,
        buff, buff_size, is_utf8_loc
    ) );
    SET_VECTOR_ELT(res, i, str_i);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res;
}
