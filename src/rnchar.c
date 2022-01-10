/*
 * Copyright (C) 2022 Brodie Gaslam
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

#include "fansi.h"

/*
 * We need a stub version of R_nchar for R < 3.2.2 since it is not exposed in
 * those.
 */

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 2)
#else
// nocov start
int R_nchar(SEXP string, nchar_type type_,
            Rboolean allowNA, Rboolean keepNA, const char* msg_name) {
  return 1;
};
// nocov end
#endif

