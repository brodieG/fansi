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
  const char * prefix, int prefix_len, int prefix_has_utf8,
  const char * initial, int initial_len, int initial_has_utf8,
  int strict, char ** buff, int * buff_size, int is_utf8_loc
) {
  char * buff_target = * buff;
  FANSI_state state = FANSI_state_init();
  state.string = x;

  int width_1 = width - indent - initial_len;
  int width_2 = width - exdent - prefix_len;

  if(width < 1) error("Internal Error: invalid width.");
  if(width_tar < 0) error("Internal Error: incompatible width/indent/prefix.");

  // Start with a buffer twice the width we're targeting; we'll grow it as
  // needed.  The buffer may already exist from a previous iteration

  if(!buff_target) {
    *buff_size = FANSI_ADD_INT(width, width);
    buff_target = R_alloc(*buff_size, sizeof(char));
  }
  // Track the last valid breaking point we have.  For now we are just looking
  // for spaces, tabs.

  int last_bound = 0;

  // Need to do two pass to figure out how many elements we'll have?  Or maybe
  // just store the CHARSXPs in a LISTSXP at first?  There is probably no harm
  // in that?  Certainly a lot simpler than having to either record states, etc,
  // or re-run everything twice

  SEXP char_list = PROTECT(list1(R_NilValue));
  int prev_newline = 0;  // tracks if last non blank character was newline
  int para_start = 1;
  int width_tar = width_1;
  int start_byte = state.pos_byte;

  struct FANSI_state state_start = state;
  struct FANSI_state state_blank = FANSI_state_init();  // reference

  while(state.string[state.pos_byte]) {
    while(state.pos_width < width_tar) {
      int make_line = 0;
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
        int target_size = last_bound - start_byte - 1;
        int state_start_size = 0;

        if(needs_close) target_size = FANSI_ADD_INT(target_size, 4);
        if(needs_start) {
          state_start_size = FANSI_state_size(state_start);
          target_size = FANSI_ADD_INT(target_size, state_start_size);
        }
        if(target_size > *buff_size) {
          // don't do re-alloc because we are going to overwrite everything
          // anyway, double prev buffer unless entry is bigger than that

          int tmp_double_size = FANSI_ADD_INT(*buff_size, *buff_size);
          if(last_bound - 1 > tmp_double_size) tmp_double_size = last_bound - 1;
          *buff_size = tmp_double_size;
          buff_target = buff_target_start = R_alloc(*buff_size, sizeof(char));
        }
        if(cur_chr == '\n') prev_newline = 1;

        if(needs_start) {
          FANSI_csi_write(buff_target, state_start, state_start_size);
          buff_target += state_start_size;
        }
        memcpy(buff_target, state_start.string, last_bound - start_byte);
        buff_target += last_bound - start_byte;
        if(needs_end) {
          memcpy(buff_target + last_bound, "\033[0m", 4);
          buff_target += 4;
        }
        *buff_target = 0;

        // Now create the charsxp and append to the list, start by determining
        // what encoding to use.  If pos_byte is greater than pos_ansi it means
        // we must have hit a UTF8 encoded character

        cetype_t chr_type = CE_NATIVE;
        if(
          (state.has_utf8 || initial_has_utf8 || prefix_has_utf8) &&
          !is_utf8_loc
        ) {
          chr_type = CE_UTF8;
        }
        SEXP res_sxp = mkCharLenCe();
      }
      // NOTE: Need to handle carriage returns

      state = FANSI_read_next(state);
      if(state.pos_width > width_tar) {
        // Need to make the line

        para_start = 0;
        width_tar = width_2;
      }


    }
  }
  // Need

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
    typeof(x) != STRSXP || typeof(width) != INTSXP ||
    typeof(indent) != INTSXP || typeof(exdent) != INTSXP ||
    typeof(prefix) != STRSXP || typeof(strict) != INTSXP
  ) {
    error("Type error.");
  }
  R_xlen_t i, x_len = XLENGTH(x);

  int strict_int = asInteger(strict);

  // could be a little faster if we has a version that just did this for the
  // char instead of dealing with strsxps

  SEXP prefix_strip = PROTECT(FANSI_strip(prefix));
  SEXP initial_strip = PROTECT(FANSI_strip(initial));

  int is_utf8_loc = FANSI_is_utf8_loc();
  const char * prefix_chr = CHAR(asChar(prefix));
  const char * prefix_chr_strip =
    FANSI_string_as_utf8(asChar(prefix_strip), is_utf8_loc);
  int prefix_chr_len = R_nchar(
    prefix_chr, Width, FALSE, FALSE, "when computing display width"
  );
  const char * prefix_chr_strip_tmp = prefix_strip;
  int prefix_has_utf8 = 0;
  while(*prefix_chr_strip_tmp) {
    if(*prefix_chr_strip_tmp > 127) {
      prefix_has_utf8 = 1;
      break;
    }
    ++prefix_chr_strip_tmp;
  }

  const char * initial_chr = CHAR(asChar(initial));
  const char * initial_chr_strip =
    FANSI_string_as_utf8(asChar(initial_strip), is_utf8_loc);
  int initial_chr_len = R_nchar(
    initial_chr, Width, FALSE, FALSE, "when computing display width"
  );
  const char * initial_chr_strip_tmp = initial_strip;
  int initial_has_utf8 = 0;
  while(*initial_chr_strip_tmp) {
    if(*initial_chr_strip_tmp < 0) {
      initial_has_utf8 = 1;
      break;
    }
    ++initial_chr_strip_tmp;
  }
  UNPROTECT(2);

  // Check that widths are feasible, although really only relevant if in strict
  // mode

  int width_int = asInteger(width);
  int indent_int = asInteger(indent);
  int exdent_int = asInteger(exdent);

  if(
    strict_int && (
      FANSI_ADD_INT(indent_int, initial_chr_len) >= width_int ||
      FANSI_ADD_INT(exdent_int, prefix_chr_len) >= width_int
    )
  )
    error(
      "%s%s",
      "Width error: sum of `indent` and `initial` width or sum of `exdent` and",
      "`prefix` width must be less than `width` when in strict mode."
    )

  SEXP res = PROTECT(allocVector(VECSXP, x_len));

  // Set up the buffer, this will be created in FANSI_strwrap, but we want a
  // handle for it here so we can re-use

  char ** buff = 0;
  int * buff_size = 0;

  for(i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);

    SEXP pre_chr = !i ? asChar(initial) : asChar(prefix);
    R_len_t pre_len = R_nchar(
      pre_chr, Width, FALSE, FALSE, "when computing display width"
    );
    SEXP str_i = PROTECT(
      FANSI_strwrap(
        CHAR(STRING_ELT(x, i)), width_int, indent_int, exdent_int,
        prefix_chr, prefix_len, prefix_has_utf8,
        initial_chr, initial_len, initial_has_utf8,
        strict_int,
        buff, buff_size
    ) );
    SET_VECTOR_ELT(res, i, str_i);
    UNPROTECT(1);
  }
}
