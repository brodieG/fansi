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

// - General Notes -------------------------------------------------------------

// Most of the constants defined here are used to encode state parameters in 32
// bit unsigned int variables.  Many state parameters are encoded as a single
// bit in either the ->setting or ->status members of the state objects.  Others
// are stored as small unsigned integer values occupying some set of adjacent
// bits in the same member objects.  For these multi-bit elements, we
// (sometimes) define _MASK and _ALL constants.  The _MASK constants relate to
// the actual bits being set in the target integers.
//
// Generally: _MASK >> _START == _ALL.
//
// Use the FANSI_GET_RNG and FANSI_SET_RNG macros to get/set the decimal values
// embedded in the larger unsigned int objects.
//
// Ideally, this would all be script generated to avoid risks of manual errors,
// but it isn't today.

// - Settings ------------------------------------------------------------------

// Offset for starting bytes for various settings
#define FANSI_SET_CTL       0
#define FANSI_SET_TERMCAP   7
#define FANSI_SET_WARN     10
#define FANSI_SET_WIDTH    20

// bits 0-6: recognized controls (also used in .status)
#define FANSI_CTL_NL           1
#define FANSI_CTL_C0           2
#define FANSI_CTL_SGR          4
#define FANSI_CTL_CSI          8
#define FANSI_CTL_ESC         16
#define FANSI_CTL_URL         32
#define FANSI_CTL_OSC         64

#define FANSI_CTL_ALL        127
#define FANSI_CTL_MASK       127

// Bits 7-9: term caps
#define FANSI_TERM_BRIGHT    128
#define FANSI_TERM_256       256
#define FANSI_TERM_TRUECOLOR 512

#define FANSI_TERM_ALL         7
#define FANSI_TERM_MASK      896

// Bits 10-19: warning level (see ERR_*)
#define FANSI_WARN_MASK    2096128 // 0111 1111 1111 << FANSI_SET_WARN
#define FANSI_WARN_ALL        2047 // 0111 1111 1111
// Warnings for situations jeopardizing width computation and similar
#define FANSI_WARN_MANGLED  163840 // 0000 1010 0000 << FANSI_SET_WARN
// UTF8 non-ASCII
#define FANSI_WARN_BADBYTE 1572864 // 0110 0000 0000 << FANSI_SET_WARN

// bits 20-21: Width mode, this is an integer, not bit flags, so
// First shift by FANSI_SET_WIDTH
#define FANSI_COUNT_CHARS    0
#define FANSI_COUNT_WIDTH    1
#define FANSI_COUNT_GRAPH    2
#define FANSI_COUNT_BYTES    3

#define FANSI_COUNT_ALL      3

// bits 22-24: other settings
#define FANSI_SET_ALLOWNA  4194304
#define FANSI_SET_KEEPNA   8388608
#define FANSI_SET_ESCONE  16777216  // consume only one ESC at a time

// - Status --------------------------------------------------------------------

#define FANSI_STAT_ERR_START  7

// bits 0-6: identical to .settings (controls found).  It's not clear that we
// actually want this to be a bit field, it might be better to have it be an
// integer representing only the last state, but that's not what we have ATM.


// bits 7-10: integer error code (not bit flags), see read.c/err_msgs[] and
// `?unhandled_ctl` for details.

#define FANSI_STAT_ERR_ALL    15
#define FANSI_STAT_ERR_MASK 1920

// These are all integer values that must be shifted by _ERR_START for encoding
// into ->status.  Subtract 1 for ->settings byte positions (relative to the
// settings starting byte for errors).
#define ERR_UNKNOWN_SUB         1
#define ERR_BAD_SUB             2
#define ERR_EXCEED_CAP          3
#define ERR_NOT_SPECIAL         4
#define ERR_NOT_SPECIAL_BAD_SUB 5
#define ERR_BAD_CSI_OSC         6
#define ERR_ESC_OTHER           7
#define ERR_ESC_OTHER_BAD       8
#define ERR_C0                  9
#define ERR_BAD_UTF8           10
#define ERR_NON_ASCII          11

// bits 11-14: additional status flags
#define FANSI_STAT_ZWJ       2048
#define FANSI_STAT_RI        4096
#define FANSI_STAT_AGAIN     8192 // Need to read on more char
#define FANSI_STAT_WARNED   16384 // Warning already issued
#define FANSI_STAT_SPECIAL  32768 // Valid SGR or URL (no critical errors)

// - sgr.style -----------------------------------------------------------------

// style encodings, used against sgr.style
#define FANSI_STL_BOLD         1
#define FANSI_STL_BLUR         2   // or faint
#define FANSI_STL_ITALIC       4
#define FANSI_STL_UNDER        8
#define FANSI_STL_BLINK1      16   // slow blink
#define FANSI_STL_BLINK2      32   // fast blink
#define FANSI_STL_INVERT      64
#define FANSI_STL_CONCEAL    128
#define FANSI_STL_CROSSOUT   256
#define FANSI_STL_FRAKTUR    512
#define FANSI_STL_UNDER2    1024   // double underline
#define FANSI_STL_PROPSPC   2048   // prop spacing

#define FANSI_STL_MASK      4095
#define FANSI_STL_MASK1      511   // Basic styles (i.e. 1-9 codes, sum(2^(0:8))
#define FANSI_STL_MASK2      447   // Basic styles for HTML, excludes inverse

#define FANSI_BRD_FRAMED    4096
#define FANSI_BRD_ENCIRC    8192
#define FANSI_BRD_OVERLN   16384

#define FANSI_BRD_MASK     28672   // sum(2^(12:14))

#define FANSI_IDG_UNDERL   32768 // ideogram underline or right side line
#define FANSI_IDG_UNDERL2  65536 // ideogram dbl underline or dbl line on right
#define FANSI_IDG_OVERL   131072 // ideogram overline or left side line
#define FANSI_IDG_OVERL2  262144 // ideogram dbl overline or dbl line on left
#define FANSI_IDG_STRESS  524288 // ideogram stress marking
#define FANSI_IDG_MASK   1015808 // sum(2^(15:19))

// Alternative fonts, 10-19, (encoded as is for simplicity, so use 5 bytes)

#define FANSI_FONT_START  27
#define FANSI_FONT_MASK 4160749568 // sum(2^(27:31))
#define FANSI_FONT_ALL    31

// - Misc ----------------------------------------------------------------------

#define FANSI_CLR_BUFF_SIZE  17   // big enough for e.g. 38;2;255;255;255

// Color modes
#define FANSI_CLR_MASK    240    // 1111 0000
#define FANSI_CLR_OFF       0
#define FANSI_CLR_8        16
#define FANSI_CLR_BRIGHT   32
#define FANSI_CLR_256      64
#define FANSI_CLR_TRU     128

// For start/stop rounding
#define FANSI_RND_START     1
#define FANSI_RND_STOP      2
#define FANSI_RND_BOTH      3
#define FANSI_RND_NEITHER   4

#endif  /* _FANSI_CNST_H */
