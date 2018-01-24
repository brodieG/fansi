

SEXP FANSI_has_tabs(SEXP vec) {
}


SEXP FANSI_tabs_as_spaces(
  SEXP vec, SEXP tab_stops, struct FANSI_buff * buff, int is_utf8_loc
) {
  if(TYPEOF(vec) != STRSXP)
    error("Argument 'vec' should be a character vector");
  R_xlen_t len = XLENGTH(vec);
  R_xlen_t len_stops = XLENGTH(tab_stops);

  const char * res, * source;
  int tabs_in_str = 0;
  int max_tab_stop = 0;

  SEXP res_sxp = PROTECT(vec);

  for(R_xlen_t i = 0; i < len; ++i) {
    int tab_count = 0;
    char * res_chr;
    source = CHAR(STRING_ELT(vec, i));
    while(strchr(source, '\t')) {
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

      while((cur_chr = state.string[state.pos_byte])) {
        if(cur_chr == '\t') {
        } else {
        }
        state = FANSI_read_next(state);
      }
    }
  }
}
