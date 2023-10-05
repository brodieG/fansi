/*
 * Copyright (C) Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 or 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
 */

#include <float.h>
#include <stdint.h>
#include <Rinternals.h>
#include "fansi.h"

/*
 * Check all the assumptions we're making
 *
 * Intended to be run onload to make sure there isn't some weird system where
 * our baseline assumptions are not met
 *
 * returns TRUE on success, but throws warnings.
 */
static void check_limits(void) {
  if(
    // Signed
    FANSI_lim.lim_int.max < 1 || FANSI_lim.lim_int.min > -1 ||
    FANSI_lim.lim_R_len_t.max < 1 || FANSI_lim.lim_R_len_t.min != 0 ||
    FANSI_lim.lim_R_xlen_t.max < 1 || FANSI_lim.lim_R_xlen_t.min != 0 ||
    // Unsigned
    FANSI_lim.lim_size_t.max < 1U || FANSI_lim.lim_size_t.min != 0U
  )
    error("Invalid custom limit; contact maintainer.");  // nocov
}
// nocov start
// by definition none of the errors should be thrown, so no sense in
// covering this
SEXP FANSI_check_assumptions(void) {
  const char * err_base =
    "Failed system assumption: %s%s; please contact maintainer.";

  // Check custom limits
  check_limits();

  // Otherwise bit twiddling assumptions may not work as expected?
  if(CHAR_BIT != 8)
    warningcall(R_NilValue, err_base, "CHAR_BIT is not 8", "");

  // This is supposedly enforced by R, and we rely on it in several places (e.g.
  // to ensure bitmask large enough for styles)
  if(sizeof(int) != 4)
    warningcall(R_NilValue, err_base, "ints are not 32 bits", "");

  // If this is not TRUE, there could be alignment issues for some of our
  // structs that use size_t elements given that R_alloc only guarantees double
  // alignment.
  if(sizeof(size_t) > sizeof(double))
    warningcall(
      R_NilValue, err_base,
      "size_t larger than double may cause alignment issues."
    );

  // Important for some our boundary condition assumptions, in particular that
  // NA_INTEGER < int x.
  if(FANSI_lim.lim_int.min != NA_INTEGER) {
    warningcall(
      R_NilValue, err_base, "INT_MIN != NA_INTEGER but the code in this ",
      "package assumes that they are equal"
    );
  }
  // This also doesn't check R_LEN_T_MAX, should be possible to remove this
  // assumption as we started to for html with fansi 0.5.0 by doing everything
  // in int and checking on entry and on exit it conforms with R_len_t.
  if(sizeof(R_len_t) != sizeof(int))
    warningcall(R_NilValue, err_base, "R_len_t not same size as int", "");

  // Because we check that strings are no longer than this, but then allocate
  // memory as INT_MAX + 1 with a size_t, so need to make sure that fits
  // Update: now runtime check with fansi 0.5.0, at least for html, might still
  // need to check in normal use.
  if(FANSI_lim.lim_size_t.max - 1 < (uintmax_t)FANSI_lim.lim_int.max)
    warningcall(
      R_NilValue, err_base,
      "SIZE_MAX not sufficiently larger than INT_MAX", ""
    );

  if(FANSI_lim.lim_size_t.max - 1 < (uintmax_t)FANSI_lim.lim_R_len_t.max)
    warningcall(
      R_NilValue, err_base,
      "SIZE_MAX not sufficiently larger than R_LEN_T_MAX", ""
    );

  // We allocate memory in multiples of the length of an input vector.  It's
  // thus helpful to be sure that no R_xlen_t value can overflow SIZE_MAX.
  if((uintmax_t)FANSI_lim.lim_R_xlen_t.max > FANSI_lim.lim_size_t.max)
    warningcall(
      R_NilValue, err_base,
      "R_XLEN_TMAX larger than SIZE_MAX", ""
    );

  if((uintmax_t)FANSI_lim.lim_int.max > FANSI_lim.lim_size_t.max)
    warningcall(
      R_NilValue, err_base,
      "INT_MAX larger than SIZE_MAX", ""
    );

  if(FANSI_lim.lim_int.max > FANSI_lim.lim_R_xlen_t.max)
    warningcall(
      R_NilValue, err_base,
      "INT_MAX larger than R_XLEN_T_MAX", ""
    );

  // We ensure strings don't exceed INT_MAX as we create them, but we do
  // measure string length by doing end - start on pointers. intmax_t to
  // suppress compiler warning
  if((intmax_t)PTRDIFF_MAX < (intmax_t)FANSI_lim.lim_int.max)
    warningcall(
      R_NilValue, err_base,
      "PTRDIFF_MAX smaller than INT_MAX", ""
    );

  return ScalarLogical(1);
}
// nocov end
