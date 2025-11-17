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

#ifndef _FANSI_EXT_H
#define _FANSI_EXT_H
#include <Rinternals.h>

SEXP FANSI_substr(
  SEXP x,
  SEXP start, SEXP stop,
  SEXP value,
  SEXP type, SEXP rnd,
  SEXP warn, SEXP term_cap,
  SEXP ctl, SEXP norm,
  SEXP carry, SEXP terminate
);
SEXP FANSI_has(SEXP x, SEXP ctl, SEXP warn);
SEXP FANSI_strip(SEXP x, SEXP ctl, SEXP warn);
SEXP FANSI_strwrap_ext(
  SEXP x, SEXP width,
  SEXP indent, SEXP exdent,
  SEXP prefix, SEXP initial,
  SEXP wrap_always, SEXP pad_end,
  SEXP strip_spaces,
  SEXP tabs_as_spaces, SEXP tab_stops,
  SEXP warn, SEXP term_cap,
  SEXP first_only,
  SEXP ctl, SEXP norm, SEXP carry,
  SEXP terminate
);
SEXP FANSI_process_ext(SEXP input, SEXP term_cap, SEXP ctl);
SEXP FANSI_tabs_as_spaces_ext(
  SEXP vec, SEXP tab_stops, SEXP warn, SEXP term_cap, SEXP ctl
);
SEXP FANSI_color_to_html_ext(SEXP x);
SEXP FANSI_esc_to_html(
  SEXP x, SEXP warn, SEXP term_cap, SEXP color_classes, SEXP carry,
  SEXP warn_unesc
);
SEXP FANSI_unhandled_esc(SEXP x, SEXP term_cap);

SEXP FANSI_nchar(
  SEXP x, SEXP type, SEXP keepNA, SEXP allowNA,
  SEXP warn, SEXP term_cap, SEXP ctl, SEXP z
);
SEXP FANSI_trimws(
  SEXP x, SEXP which, SEXP warn, SEXP term_cap, SEXP ctl, SEXP norm
);

// utility / testing

SEXP FANSI_check_assumptions(void);

SEXP FANSI_add_int_ext(SEXP x, SEXP y);

SEXP FANSI_set_int_max(SEXP x);
SEXP FANSI_set_rlent_max(SEXP x);
SEXP FANSI_get_int_max(void);
SEXP FANSI_get_warn_all(void);
SEXP FANSI_get_warn_mangled(void);
SEXP FANSI_get_warn_utf8(void);
SEXP FANSI_get_warn_error(void);
SEXP FANSI_esc_html(SEXP x, SEXP what);

SEXP FANSI_normalize_state_ext(
  SEXP x, SEXP warn, SEXP term_cap, SEXP carry
);
SEXP FANSI_normalize_state_list_ext(
  SEXP x, SEXP warn, SEXP term_cap, SEXP carry
);

SEXP FANSI_size_buff_ext(SEXP x);
SEXP FANSI_size_buff_prot_test(void);

SEXP FANSI_check_enc_ext(SEXP x, SEXP i);
SEXP FANSI_ctl_as_int_ext(SEXP ctl);

SEXP FANSI_state_close_ext(SEXP x, SEXP warn, SEXP term_cap, SEXP norm);
SEXP FANSI_state_at_end_ext(
  SEXP x, SEXP warn, SEXP term_cap, SEXP ctl, SEXP norm, SEXP carry,
  SEXP arg, SEXP allowNA
);
SEXP FANSI_bridge_state_ext(SEXP end, SEXP restart, SEXP term_cap, SEXP norm);
SEXP FANSI_buff_test_reset(void);
SEXP FANSI_buff_test_copy_overflow(void);
SEXP FANSI_buff_test_mcopy_overflow(void);
SEXP FANSI_buff_test_fill_overflow(void);

SEXP FANSI_reset_limits(void);

SEXP FANSI_read_all_ext(SEXP x, SEXP warn, SEXP term_cap);

SEXP FANSI_unicode_version(void);

#endif  /* _FANSI_EXT_H */
