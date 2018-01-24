/*
 * Determine how many spaces tab width should be
 */
int FANSI_tab_width(struct FANSI_state state, SEXP tab_stops) {
  R_xlen_t stops = XLENGTH(tab_stops);
  if(!stops)
    error("Internal Error: must have at least one tab stop");  // nocov

  int tab_width = 0;
  R_xlen_t stop_idx = 0;

  while(state.pos_width > tab_width) {
    int stop_size = INTEGER(tab_stops)[stop_idx];
    if(!stop_size) error("Internal Error: zero size tab stop.");
    tab_width = FANSI_add_int(tab_width, stop_size);
    if(stop_idx < stops) stop_idx++;
  }
  return tab_width - state.pos_width;
}

SEXP FANSI_tabs_as_spaces(
  SEXP vec, SEXP tab_stops, struct FANSI_buff * buff, int is_utf8_loc
) {
  if(TYPEOF(vec) != STRSXP)
    error("Argument 'vec' should be a character vector");
  R_xlen_t len = XLENGTH(vec);
  R_xlen_t len_stops = XLENGTH(tab_stops);

  const char * res, * source, * source_track;
  int tabs_in_str = 0;
  int max_tab_stop = 0;

  SEXP res_sxp = PROTECT(vec);

  for(R_xlen_t i = 0; i < len; ++i) {
    int tab_count = 0;
    char * res_chr;
    source = CHAR(STRING_ELT(vec, i));
    // One additional issue is that if we f
    while((source_track = strchr(source_track, '\t'))) {
      if(!tabs_in_str) {
        tabs_in_str = 1;
        UNPROTECT(1);
        res_sxp = PROTECT(duplicate(vec));
        for(R_xlen_t j = 0; j < len_stops; ++j) {
          if(INTEGER(tab_stops)[i] > max_tab_stop)
            max_tab_stop = INTEGER(tab_stops)[i];
        }
      }
      ++tab_count;
    }
    if(tab_count) {
      // Need to convert to UTF8 so width calcs work

      FANSI_buff_const buff_utf8 =
        FANSI_string_as_utf8(STRING_ELT(vec, i), is_utf8_loc);

      // Figure out possible size of buffer, allowing max_tab_stop for every
      // tab, which should over-allocate

      int new_buff_size = FANSI_add_int(buff.len, 1);

      for(int k = 0; k < tab_count; ++k) {
        new_buff_size = FANSI_add_int(new_buff_size, max_tab_stop - 1);
      }
      FANSI_size_buff(buff, new_buff_size);

      struct FANSI_state state = FANSI_state_init();
      state.string = buff_utf8.buff;
      char cur_chr;
      const char * buff_track, * buff_start, * string_last;
      buff_track = buff_start = buff->buff;
      int last_byte = state.pos_byte;

      while(1) {
        cur_chr = state.string[state.pos_byte];
        int extra_spaces = 0;

        if(cur_chr == '\t') {
          extra_spaces = FANSI_tab_width(state, tab_stops);
        } else if (cur_chr == '\n') {
          state = FANSI_reset_width(state);
        }
        // Write string

        if(cur_chr == '\t' || !cur_chr) {
          int write_bytes = state.pos_byte - 1 - last_byte)
          memcpy(buff_track, string_last, write_bytes);
          buff_track += write_bytes;
          while(extra_spaces) {
            --extra_spaces;
            *(++buff_track) = ' ';
          }
          last_byte = state.pos_byte;
          if(!cur_chr) *buff_track = 0;
        }
        if(!cur_chr) break;
        state = FANSI_read_next(state);
      }
      // Write the CHARSXP

      cetype_t chr_type = CE_NATIVE;
      if((state.has_utf8 && !is_utf8_loc) chr_type = CE_UTF8;

      SEXP chr_sxp = PROTECT(
        mkCharLenCE(buff_start, (int) (buff_track - buff_sart), chr_type)
      );
      SET_STRING_ELT(vec, i, chr_sxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res_sxp;
}
SEXP FANSI_tabs_as_spaces_ext(SEXP vec, SEXP tab_stops) {
  int is_utf8_loc = FANSI_is_utf8_loc();
  struct FANSI_buff buff = {.len = 0};

  return FANSI_tabs_as_spaces(vec, tab_stops, &buff, is_utf8_loc);
}

