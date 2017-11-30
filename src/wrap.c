

/*
 * x is expected to be in UTF-8 format
 */

SEXP FANSI_strwrap(
  const char * x, int width, int indent, const char * prefix, int prefix_len
) {

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

  const char * prefix_chr = CHAR(asChar(prefix));
  const char * prefix_chr_strip = FANSI_string_as_utf8(asChar(prefix_strip));
  int prefix_chr_len = R_nchar(
    prefix_chr, Width, FALSE, FALSE, "when computing display width"
  )
  const char * initial_chr = CHAR(asChar(initial));
  const char * initial_chr_strip = FANSI_string_as_utf8(asChar(initial_strip));
  int initial_chr_len = R_nchar(
    initial_chr, Width, FALSE, FALSE, "when computing display width"
  )
  UNPROTECT(2);

  // Check that widths are feasible, although really only relevant if in strict
  // mode

  int width_int = asInteger(width);
  int indent_int = asInteger(indent);
  int exdent_int = asInteger(exdent);

  if(
    strict_int && (
      safe_add(indent_int, initial_chr_len) >= width_int ||
      safe_add(exdent_int, prefix_chr_len) >= width_int
    )
  )
    error(
      "%s%s",
      "Width error: sum of `indent` and `initial` width or sum of `exdent` and",
      "`prefix` width must be less than `width` when in strict mode."
    )

  SEXP res = PROTECT(allocVector(VECSXP, x_len));

  for(i = 0; i < x_len; ++i) {
    SEXP pre_chr = !i ? asChar(initial) : asChar(prefix);
    R_len_t pre_len = R_nchar(
      pre_chr, Width, FALSE, FALSE, "when computing display width"
    );
    SEXP str_i = PROTECT(
      FANSI_strwrap(
        CHAR(STRING_ELT(x, i)), width_int, !i ? indent_int : exdent_int,
        pre_chr, pre_len
    ) );
    SET_VECTOR_ELT(res, i, str_i);
    UNPROTECT(1);
  }
}
