
#include "fansi.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"has_csi", (DL_FUNC) &FANSI_has, 1},
  {"strip_csi", (DL_FUNC) &FANSI_strip, 1},
  {"strwrap_csi", (DL_FUNC) &FANSI_strwrap_ext, 8},
  {"state_at_pos_ext", (DL_FUNC) &FANSI_state_at_pos_ext, 5},

  {"check_assumptions", (DL_FUNC) &FANSI_check_assumptions, 0},

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

