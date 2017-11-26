



const char * FANSI_strwrap_ext(
  const char * x, int width, int indent, const char * prefix, int prefix_len
) {

}



SEXP FANSI_strwrap_ext(
  SEXP x, SEXP width, SEXP indent, SEXP exdent, SEXP prefix,
  SEXP initial
) {
  if(
    typeof(x) != STRSXP || typeof(width) != INTSXP ||
    typeof(indent) != INTSXP || typeof(exdent) != INTSXP ||
    typeof(prefix) != STRSXP
  ) {
    error("Type error.");
  }
  R_xlen_t i, x_len = XLENGTH(x);

  const char * prefix_chr = FANSI_string_as_utf8(prefix);
  const char * initial_chr = FANSI_string_as_utf8(initial);
  int pre_len;
  SEXP res = PROTECT(allocVector(VECSXP, x_len));




  for(i = 0; i < x_len; ++i) {
    if(!i) {
      pre_len = R_nchar(
        STRING_ELT(initial, 0), Width, FALSE, FALSE,
        "when computing display width"
      );
      pre_chr = CHAR(STRING_ELT(initial, 0));
    } else {
      pre_len = R_nchar(
        STRING_ELT(prefix, 0), Width, FALSE, FALSE,
        "when computing display width"
      );
      pre_chr = CHAR(STRING_ELT(prefix, 0));
    }

  }





}
