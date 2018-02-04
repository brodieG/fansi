// /*
//  * Copyright (C) 2018  Brodie Gaslam
//  *
//  * This file is part of "fansi - ANSI Escape Aware String Functions"
//  *
//  * This program is free software: you can redistribute it and/or modify
//  * it under the terms of the GNU General Public License as published by
//  * the Free Software Foundation, either version 2 of the License, or
//  * (at your option) any later version.
//  *
//  * This program is distributed in the hope that it will be useful,
//  * but WITHOUT ANY WARRANTY; without even the implied warranty of
//  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  * GNU General Public License for more details.
//  *
//  * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
//  */
//
// #include "fansi.h"
// /*
//  * General logic here is:
//  *
//  * * Sort everything by the strings so that we can process each string
//  *   separately
//  * * Also sort by start and stop
//  * * for each distinct string, compute location and state of every ESC sequence
//  *   up to the furthest point we request
//  *
//  * We also need a mechanism for getting all the tags from a string
//  *
//  * @param x_scalar whether x was recycled from scalar, in which case we do not
//  *   need to sort
//  */
//
// SEXP FANSI_substr_ext(
//   SEXP x, SEXP start, SEXP stop, SEXP type='chars',
//   SEXP round, SEXP tabs_as_spaces, SEXP tab_stops,
//   SEXP x_scalar
// ) {
//
//
// }
