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

/*
 * Code adapted from src/main/util.c in the R sources.  Original copyright
 * follows
 */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2022  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* This code is actually not completely compliant, but we're just trying to
 * match R behavior rather than the correct UTF8 decoding.  Among other things
 * note that this allows 5-6 byte encodings which are no longer valid.
 */

/* Number of additional bytes */

static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

static int utf8clen(const char * c, int * mb_err) {
  /* This allows through 8-bit chars 10xxxxxx, which are invalid */
  int res = 0;
  if ((*c & 0xc0) != 0xc0) res = 1;
  else res = 1 + utf8_table4[*c & 0x3f];

  // Make sure string doesn't end before UTF8 char supposedly does
  for(int i = 1; i < res; ++i) {
    if(!*(c + i)) {
      *mb_err = 1;
      res = i;
      break;
  } }
  return res;
}

