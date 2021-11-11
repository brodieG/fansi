/*
 * Copyright (C) 2021  Brodie Gaslam
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
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
 */

#include "fansi-ext.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static const
R_CallMethodDef callMethods[] = {
  {"has_csi", (DL_FUNC) &FANSI_has, 3},
  {"strip_csi", (DL_FUNC) &FANSI_strip, 3},
  {"strwrap_csi", (DL_FUNC) &FANSI_strwrap_ext, 18},
  {"substr", (DL_FUNC) &FANSI_substr, 11},
  {"state_at_pos_ext", (DL_FUNC) &FANSI_state_at_pos_ext, 11},
  {"process", (DL_FUNC) &FANSI_process_ext, 3},
  {"check_assumptions", (DL_FUNC) &FANSI_check_assumptions, 0},
  {"tabs_as_spaces", (DL_FUNC) &FANSI_tabs_as_spaces_ext, 5},
  {"color_to_html", (DL_FUNC) &FANSI_color_to_html_ext, 1},
  {"esc_to_html", (DL_FUNC) &FANSI_esc_to_html, 5},
  {"unhandled_esc", (DL_FUNC) &FANSI_unhandled_esc, 2},
  {"unique_chr", (DL_FUNC) &FANSI_unique_chr, 1},
  {"nchar_esc", (DL_FUNC) &FANSI_nchar, 8},
  {"add_int", (DL_FUNC) &FANSI_add_int_ext, 2},
  {"cleave", (DL_FUNC) &FANSI_cleave, 1},
  {"order", (DL_FUNC) &FANSI_order, 1},
  {"sort_chr", (DL_FUNC) &FANSI_sort_chr, 1},
  {"set_int_max", (DL_FUNC) &FANSI_set_int_max, 1},
  {"get_int_max", (DL_FUNC) &FANSI_get_int_max, 0},
  {"set_rlent_max", (DL_FUNC) &FANSI_set_rlent_max, 1},
  {"get_warn_all", (DL_FUNC) &FANSI_get_warn_all, 0},
  {"check_enc", (DL_FUNC) &FANSI_check_enc_ext, 2},
  {"ctl_as_int", (DL_FUNC) &FANSI_ctl_as_int_ext, 1},
  {"esc_html", (DL_FUNC) &FANSI_esc_html, 2},
  {"reset_limits", (DL_FUNC) &FANSI_reset_limits, 0},
  {"normalize_state", (DL_FUNC) &FANSI_normalize_state_ext, 4},
  {"normalize_state_list", (DL_FUNC) &FANSI_normalize_state_list_ext, 4},
  {"close_state", (DL_FUNC) &FANSI_state_close_ext, 4},
  {"size_buff", (DL_FUNC) &FANSI_size_buff_ext, 1},
  {"size_buff_prot_test", (DL_FUNC) &FANSI_size_buff_prot_test, 0},
  {"buff_test_reset", (DL_FUNC) &FANSI_buff_test_reset, 0},
  {"buff_test_copy_overflow", (DL_FUNC) &FANSI_buff_test_copy_overflow, 0},
  {"buff_test_mcopy_overflow", (DL_FUNC) &FANSI_buff_test_mcopy_overflow, 0},
  {"buff_test_fill_overflow", (DL_FUNC) &FANSI_buff_test_fill_overflow, 0},
  {"state_at_end", (DL_FUNC) &FANSI_state_at_end_ext, 8},
  {"bridge_state", (DL_FUNC) &FANSI_bridge_state_ext, 4},
  {NULL, NULL, 0}
};

void attribute_visible R_init_fansi(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, FALSE);
}

