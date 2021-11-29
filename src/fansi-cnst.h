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

#ifndef _FANSI_CNST_H
#define _FANSI_CNST_H

// - Settings ------------------------------------------------------------------

// Setting are packed into a (32 bit) unsigned int, the _SET_ values are the
// offset into the integer.  For those items that cover a multi-bit range, the
// pattern is to shift by the offset, and then mask by the _ALL version of the
// corresponding constant.

// Offset for starting bytes for various settings
#define FANSI_SET_TERMCAP   0
#define FANSI_SET_WARN      3
#define FANSI_SET_WIDTH    12
#define FANSI_SET_CTL      16

// Offsets, but these are single bit
#define FANSI_SET_ALLOWNA  21
#define FANSI_SET_KEEPNA   22
#define FANSI_SET_ESCONE   23  // consume only one ESC at a time

// First shift by FANSI_SET_CTL
#define FANSI_CTL_NL        1
#define FANSI_CTL_C0        2
#define FANSI_CTL_SGR       4
#define FANSI_CTL_CSI       8
#define FANSI_CTL_ESC      16
#define FANSI_CTL_URL      32
#define FANSI_CTL_OSC      64
#define FANSI_CTL_ALL     127

// First shift by FANSI_SET_TERMCAP
#define FANSI_TERM_BRIGHT    1
#define FANSI_TERM_256       2
#define FANSI_TERM_TRUECOLOR 4
#define FANSI_TERM_ALL       7

// First shift by FANSI_SET_WARN
#define FANSI_WARN_CSIBAD  336 // ... 0001 0101 0000
#define FANSI_WARN_ALL     511 // ... 0001 1111 1111

// First shift by FANSI_SET_WIDTH
#define FANSI_COUNT_CHARS    0
#define FANSI_COUNT_WIDTH    1
#define FANSI_COUNT_GRAPH    2
#define FANSI_COUNT_BYTES    3
#define FANSI_COUNT_ALL      3

// - Status --------------------------------------------------------------------

// Same concept as settings
//
// Type of failure, set to zero if no error, encode in 4 bits
//
// * 1: well formed csi sgr, but contains uninterpretable sub-strings, if a
//      CSI sequence is not fully parsed yet (i.e. last char not read) it is
//      assumed to be SGR until we read the final code.
// * 2: well formed csi sgr, but contains uninterpretable characters [:<=>]
// * 3: well formed csi sgr, but contains color codes that exceed terminal
//     capabilities
// * 4: well formed csi, but not an SGR
// * 5: malformed csi
// * 6: other escape sequence
// * 7: malformed escape (e.g. string ending in ESC).
// * 8: c0 escapes
// * 9: malformed UTF8
#define FANSI_STAT_ERR       0
#define FANSI_STAT_ERR_ALL  15

// Single bit status
#define FANSI_STAT_ZWJ       4
#define FANSI_STAT_RI        5
#define FANSI_STAT_SPECIAL   6   // Is SGR or CSI
#define FANSI_STAT_CTL       7   // Was a recognized control
#define FANSI_STAT_AGAIN     8   // Need to read on more char (was .read_one_more)
#define FANSI_STAT_CSI       9   // CSI was complete (was .last)
#define FANSI_STAT_SGR      10   // CSI is an SGR
#define FANSI_STAT_WARNED   11   // Warning already issued

// - Misc ----------------------------------------------------------------------

// IMPORTANT: masks here are *not* shifted by offset.

// Color modes
#define FANSI_CLR_MASK    192    // 1100 0000
#define FANSI_CLR_OFF       0
#define FANSI_CLR_16       64    // 0100 0000
#define FANSI_CLR_256     128    // 1000 0000
#define FANSI_CLR_TRU     192    // 1100 0000
#define FANSI_CLR_MASK_16  63    // 0011 1111

// For start/stop rounding
#define FANSI_RND_START     1
#define FANSI_RND_STOP      2
#define FANSI_RND_BOTH      3
#define FANSI_RND_NEITHER   4

// style encodings, used against sgr.style
#define FANSI_STL_BOLD      0
#define FANSI_STL_BLUR      1   // or faint
#define FANSI_STL_ITALIC    2
#define FANSI_STL_UNDER     3
#define FANSI_STL_BLINK1    4   // slow blink
#define FANSI_STL_BLINK2    5   // fast blink
#define FANSI_STL_INVERT    6
#define FANSI_STL_CONCEAL   7
#define FANSI_STL_CROSSOUT  8
#define FANSI_STL_FRAKTUR   9
#define FANSI_STL_UNDER2   10   // double underline
#define FANSI_STL_PROPSPC  11   // prop spacing

#define FANSI_STL_MASK   4095
#define FANSI_STL_MASK1   511   // Basic styles (i.e. 1-9 codes, sum(2^(0:8))

#define FANSI_BRD_FRAMED   12
#define FANSI_BRD_ENCIRC   13
#define FANSI_BRD_OVERLN   14
#define FANSI_BRD_MASK  28672   // sum(2^(12:14))

#define FANSI_IDG_UNDERL   15 // ideogram underline or right side line
#define FANSI_IDG_UNDERL2  16 // ideogram dbl underline or dbl line on right
#define FANSI_IDG_OVERL    17 // ideogram overline or left side line
#define FANSI_IDG_OVERL2   18 // ideogram dbl overline or dbl line on left
#define FANSI_IDG_STRESS   19 // ideogram stress marking
#define FANSI_IDG_MASK 1015808 // sum(2^(15:19))

// font bit start, bits including and above this are the font
// this is in sgr.style
// Alternative fonts, 10-19, (encoded as is for simplicity, so use 5 bytes)
#define FANSI_FONT_START  27 // most significant 5 byte encode font in 10-19
#define FANSI_FONT_MASK 4160749568 // sum(2^(27:31))

#endif  /* _FANSI_CNST_H */
