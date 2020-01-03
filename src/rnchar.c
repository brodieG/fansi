/*
 * Copyright (C) 2020  Brodie Gaslam
 *
 *  This file is part of "fansi - ANSI Control Sequence Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
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

