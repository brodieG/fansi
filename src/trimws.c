#include "fansi.h"

/*
 * Trim leading or trailing whitespaces intermixed with control sequences.
 *
 * @param which 0 = both, 1 = left, 2 = right
 */

SEXP FANSI_trimws(
  SEXP x, SEXP which, SEXP warn, SEXP term_cap, SEXP ctl, SEXP norm
) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` should be a character vector.");     // nocov
  if(TYPEOF(ctl) != INTSXP)
    error("Internal Error: `ctl` should integer.");          // nocov
  if(TYPEOF(which) != INTSXP || XLENGTH(which) != 1)
    error("Internal Error: `which` should scalar integer."); // nocov
  if(TYPEOF(norm) != LGLSXP || XLENGTH(norm) != 1)
    error("Internal Error: `norm` should scalar logical.");  // nocov

  R_xlen_t i, len = xlength(x);
  SEXP res_fin = x;
  int which_i = asInteger(which);
  if(which_i < 0 || which_i > 2)
    error("Internal Error: `which` must be between 0 and 2."); // nocov

  int norm_i = asLogical(norm);

  int prt = 0;
  PROTECT_INDEX ipx;
  // reserve spot if we need to alloc later
  PROTECT_WITH_INDEX(res_fin, &ipx); ++prt;

  struct FANSI_state state, state_lead, state_trail, state_last;
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  for(i = 0; i < len; ++i) {
    if(!i) {
      SEXP allowNA, keepNA, type;
      allowNA = keepNA = PROTECT(ScalarLogical(0)); ++prt;
      type = PROTECT(ScalarInteger(0)); ++prt;
      state = FANSI_state_init_full(
        x, warn, term_cap, allowNA, keepNA, type, ctl, (R_xlen_t) 0, "x"
      );
    } else state = FANSI_state_reinit(state, x, i);
    state_lead = state_trail = state_last = state;

    SEXP x_chr = STRING_ELT(x, i);
    if(x_chr == NA_STRING) continue;
    FANSI_interrupt(i);

    int string_start, string_end;
    string_start = string_end = 0;

    // Two (really three) pass process: find begin and end points of string to
    // keep, compute required size for final string (due to normalize and other
    // factors, can't really know just based on input), and finally write.  The
    // last two are part of the standard two pass measure/write framework used
    // in fansi.
    if(which_i == 0 || which_i == 1) {
      while(1) {
        switch(state.string[state.pos_byte]) {
          case ' ':
          case '\n':
          case '\r':
          case '\t':
            ++state.pos_byte;
            continue;
            break;
          case 0x1b:
            state = FANSI_read_next(state, i, 1);
            break;
          default: goto ENDLEAD;
        }
      }
      ENDLEAD:
      state_lead = state;
      string_start = state_lead.pos_byte;
    }
    // Find first space that has no subsequent non-spaces
    if(which_i == 0 || which_i == 2) {
      while(state.string[state.pos_byte]) {
        switch(state.string[state.pos_byte]) {
          case ' ':
          case '\n':
          case '\r':
          case '\t':
            if(!string_end) {
              string_end = state.pos_byte;
              state_trail = state;
            }
            ++state.pos_byte;
            continue;
          case 0x1b:
            state = FANSI_read_next(state, i, 1);
            break;
          default:
            string_end = 0;
            ++state.pos_byte;
            break;
        }
      }
      state_last = state;
    } else {
      string_end = LENGTH(x_chr);
    }
    // Do we need to write the string?
    if(string_start || string_end != LENGTH(x_chr)) {
      // We need a new vector since we have at least one change
      if(res_fin == x) REPROTECT(res_fin = duplicate(x), ipx);
      const char * err_msg = "Trimming whitespace";

      // Two pass measure/write (see write.c)
      for(int k = 0; k < 2; ++k) {
        if(!k) FANSI_reset_buff(&buff);
        else   FANSI_size_buff(&buff);
        state = FANSI_state_reinit(state, x, i);

        // Any leading SGR
        if(string_start) {
          FANSI_W_sgr(&buff, state_lead.sgr, norm_i, 1, i);
          FANSI_W_url(&buff, state_lead.url, norm_i, i);
        }
        int string_bytes = 0;
        if(string_end) string_bytes = string_end - string_start;
        else string_bytes = LENGTH(x_chr) - string_start;

        // Body of string
        FANSI_W_normalize_or_copy(
          &buff, state_lead, norm_i, state_trail.pos_byte, i, err_msg
        );
        // Trailing state
        if(string_end)
          FANSI_W_bridge(&buff, state_trail, state_last, norm_i, i, err_msg);
      }
      // We assume UTF-8 can only show up in the body of the string
      SEXP chr_sexp = PROTECT(FANSI_mkChar(buff, getCharCE(x_chr), i));
      SET_STRING_ELT(res_fin, i, chr_sexp);
      UNPROTECT(1);
    }
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res_fin;
}
