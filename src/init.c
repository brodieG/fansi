
#include "fansi.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"has_csi", (DL_FUNC) &FANSI_has, 1},
  {"strip_csi", (DL_FUNC) &FANSI_strip, 1},
  {"strwrap_csi", (DL_FUNC) &FANSI_strwrap_ext, 11},
  {"state_at_pos_ext", (DL_FUNC) &FANSI_state_at_pos_ext, 7},
  {"process", (DL_FUNC) &FANSI_process_ext, 1},
  {"check_assumptions", (DL_FUNC) &FANSI_check_assumptions, 0},
  {"digits_in_int", (DL_FUNC) &FANSI_digits_in_int_ext, 1},

  {NULL, NULL, 0}
};

void R_init_fansi(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, FALSE);
}

