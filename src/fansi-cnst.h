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
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
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
// Use the FANSI_GET_RNG and SET_RNG macros to get/set the decimal values
// embedded in the larger unsigned int objects.
//
// Ideally, this would all be script generated to avoid risks of manual errors,
// but it isn't today.

// - Settings ------------------------------------------------------------------

// Offset for starting bytes for various settings
#define SET_CTL       0
#define SET_TERMCAP   7
#define SET_WARN     10
#define SET_WIDTH    21

// bits 0-6: recognized controls (also used in .status)
#define CTL_NL           1
#define CTL_C0           2
#define CTL_SGR          4
#define CTL_CSI          8
#define CTL_ESC         16
#define CTL_URL         32
#define CTL_OSC         64

#define CTL_ALL        127
#define CTL_MASK       127
#define CTL_ESC_CTL    124   // Controls starting with ESC

// Bits 7-9: term caps
#define TERM_BRIGHT    128
#define TERM_256       256
#define TERM_TRUECOLOR 512

#define TERM_ALL         7
#define TERM_MASK      896

// Bits 10-20: warning level (see ERR_*)
#define WARN_MASK    2096128 // 0111 1111 1111 << SET_WARN
#define WARN_ALL        2047 // 0111 1111 1111
// Warnings for situations jeopardizing width computation and similar
#define WARN_MANGLED  163840 // 0000 1010 0000 << SET_WARN
#define WARN_UTF8     524288
// These should not be suppressed by warn=FALSE (but an be turned off by
// functions that don't care about them).
#define WARN_ERROR   1572864 // 0110 0000 0000 << SET_WARN

// bits 21-22: Width mode, this is an integer, not bit flags, so
// First shift by SET_WIDTH
#define COUNT_CHARS    0
#define COUNT_WIDTH    1
#define COUNT_GRAPH    2
#define COUNT_BYTES    3

#define COUNT_ALL      3

// bits 23-26: other settings
#define SET_ALLOWNA  8388608
#define SET_KEEPNA  16777216
#define SET_ESCONE  33554432  // consume only one ESC at a time
#define SET_TERMOLD 67108864  // Use < v1.0 terminal mode

// - Status --------------------------------------------------------------------

#define STAT_ERR_START  7

// bits 0-6: identical to .settings (controls found).  It's not clear that we
// actually want this to be a bit field, it might be better to have it be an
// integer representing only the last state, but that's not what we have ATM.

// bits 7-10: integer error code (not bit flags), see read.c/err_msgs[] and
// `?unhandled_ctl` for details.

#define STAT_ERR_ALL    15
#define STAT_ERR_MASK 1920

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

// bits 11-16: additional status flags, some of these are no longer strictly
// necessary with the switch to read_until (from read_next).
#define STAT_ZWJ       2048
#define STAT_RI        4096
#define STAT_AGAIN     8192 // Read past requested width
#define STAT_WARNED   16384 // Warning already issued
#define STAT_SPECIAL  32768 // Valid SGR or URL (no critical errors)
#define STAT_OVERSHOT 65536 // Read past requested width
#define STAT_DONE    131072 // Hit end with special

#define STAT_PERSIST  22528 // _ZWJ | _RI | _WARNED

// - sgr.style -----------------------------------------------------------------

// style encodings, used against sgr.style
// bits 0-11
#define STL_BOLD         1
#define STL_BLUR         2   // or faint
#define STL_ITALIC       4
#define STL_UNDER        8
#define STL_BLINK1      16   // slow blink
#define STL_BLINK2      32   // fast blink
#define STL_INVERT      64
#define STL_CONCEAL    128
#define STL_CROSSOUT   256
#define STL_FRAKTUR    512
#define STL_UNDER2    1024   // double underline
#define STL_PROPSPC   2048   // prop spacing

#define STL_MASK      4095
#define STL_MASK1      511   // Basic styles (i.e. 1-9 codes, sum(2^(0:8))
#define STL_MASK2      447   // Basic styles for HTML, excludes inverse

// bits 12-14
#define BRD_FRAMED    4096
#define BRD_ENCIRC    8192
#define BRD_OVERLN   16384

#define BRD_MASK     28672   // sum(2^(12:14))

// bits 15-19
#define IDG_UNDERL   32768 // ideogram underline or right side line
#define IDG_UNDERL2  65536 // ideogram dbl underline or dbl line on right
#define IDG_OVERL   131072 // ideogram overline or left side line
#define IDG_OVERL2  262144 // ideogram dbl overline or dbl line on left
#define IDG_STRESS  524288 // ideogram stress marking
#define IDG_MASK   1015808 // sum(2^(15:19))

// Alternative fonts, 10-19, (encoded as is for simplicity, so use 5 bytes)

#define FONT_START  20
#define FONT_MASK 4160749568 // sum(2^(20:24))
#define FONT_ALL    31

// - Misc ----------------------------------------------------------------------

#define CLR_BUFF_SIZE 20   // big enough for e.g. ESC[38;2;255;255;255;NULL

// Color modes
#define CLR_MASK    240    // 1111 0000
#define CLR_OFF       0
#define CLR_8        16
#define CLR_BRIGHT   32
#define CLR_256      64
#define CLR_TRU     128

// For start/stop rounding
#define RND_START     1
#define RND_STOP      2
#define RND_BOTH      3
#define RND_NEITHER   4

#endif  /* _FANSI_CNST_H */
