/*
Copyright (C) 2021 Brodie Gaslam

This file is part of "fansi - ANSI Control Sequence Aware String Functions"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 or 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

// Set or Get range of bits with offset `offset` and range `mask`.
//
// We could in theory have a table with all this info, but then would it be
// slower to look up than providing all the constants in line?  The advantage of
// the table is we can auto-fill it an be less likely to make mistakes in
// specifying the constants, and then we have fewer shifts.
//
// See fansi-cnst.h, FANSI_SET_*, FANSI_STAT_*.
//
// Needed by state.c, read.c,
//
// @param offset how many bits to offset into the int (offset + log2(mask) must
//    be < sizeof(unsigned int)).
// @param mask a number correspoding to in theory adjacent set of ones starting
//    at bit zero.

unsigned int FANSI_get_range(
  unsigned int x, unsigned int offset, unsigned int mask
) {
  return (x >> offset) & mask;
}
unsigned int FANSI_set_range(
  unsigned int x, unsigned int offset, unsigned int mask, unsigned int val
) {
  return (x & ~(mask << offset)) | (val << offset)
}
unsigned int FANSI_get_one(unsigned int x, unsigned int offset) {
  return x & offset;
}
unsigned int FANSI_set_one(
  unsigned int x, unsigned int offset, unsigned int mask, unsigned int val
) {
  return x | offset;
}
unsigned int get_err(x) {
  return get_rng(x, FANSI_STAT_ERR, FANSI_STAT_ERR_ALL);
}
// Only sets error if greater than already encoded one
unsigned int set_err(x, err) {
  unsigned int err0 = get_rng(x, FANSI_STAT_ERR, FANSI_STAT_ERR_ALL);
  if(err > err0) return set_range(x, FANSI_STAT_ERR, FANSI_STAT_ERR_ALL, err);
  else return x;
}


