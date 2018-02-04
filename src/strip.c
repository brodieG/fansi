/*
 * Copyright (C) 2018  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Escape Aware String Functions"
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
 * Strips ANSI tags from input
 *
 * Assumes input is NULL terminated.
 *
 * Freaks out if ANSI escape sequence contains stuff outside of 1-127.  Observed
 * behavior is that that stuff gets spat out to screen but ANSI tag continues to
 * be processed...  Probably undefined behavior?
 *
 * Really should have tried to harmonize this and strip white?
 */

SEXP FANSI_strip(SEXP input) {
  if(TYPEOF(input) != STRSXP) error("`input` should be STRSXP");

  R_xlen_t i, len = xlength(input);
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(input, &ipx);  // reserve spot if we need to alloc later
  SEXP res_fin = input;

  int any_ansi = 0;
  R_len_t mem_req = 0;         // how much memory we need for each ansi
  int invalid_ansi = 0;

  struct FANSI_csi_pos csi;

  // Compute longest char element, we'll assume this is the required size of our
  // string buffer.  This is potentially wastful if there is one very large
  // CSI-less string and all the CSI strings are short.  The alternative would
  // be to keep reallocating to a larger size, maybe by 2x, but then that
  // requires the growing buffer, etc.  For now go simple

  for(i = 0; i < len; ++i) {
    FANSI_interrupt();
    R_len_t chr_len = LENGTH(STRING_ELT(input, i));
    if(chr_len > mem_req) mem_req = chr_len;
  }
  // Now strip

  for(i = 0; i < len; ++i) {
    FANSI_interrupt();
    SEXP input_chr = STRING_ELT(input, i);
    if(input_chr == NA_STRING) continue;

    int has_ansi = 0;
    const char * chr = CHAR(input_chr);
    const char * chr_track = chr;
    char * chr_buff;
    char * res_track, * res_start;

    // note that csi.start is the NULL pointer if an escape is not found

    while((csi = FANSI_find_esc(chr_track)).start) {
      if(csi.start - chr >= INT_MAX - csi.len)
        // nocov start
        error(
          "%s%s",
          "Internal Error: string longer than INT_MAX encountered, should ",
          "not be possible."
        );
        // nocov end
      if(csi.valid) {
        // As soon as we encounter ansi, allocate vector to track what has ansi
        if(!any_ansi) {
          any_ansi = 1;

          // We need to allocate a result vector since we'll be stripping ANSI
          // CSI, and also the buffer we'll use to re-write the CSI less strings

          REPROTECT(res_fin = duplicate(input), ipx);

          // Note the is guaranteed to be an over-allocation

          if(mem_req == R_LEN_T_MAX)
            // nocov start
            error(
              "%s%s",
              "Internal error, string should be shorter than R_LEN_T_MAX, ",
              "contact maintainer."
            );
            // nocov end

          chr_buff = (char *) R_alloc(mem_req + 1, sizeof(char));
        }
        if(!has_ansi) {
          has_ansi = 1;
          res_start = res_track = chr_buff;
        }
        // Is memcpy going to cause problems again by reading past end of
        // block?  Didn't in first valgrind check.

        if(csi.len) {
          memcpy(res_track, chr_track, csi.start - chr_track);
          res_track += csi.start - chr_track;
        }
      } else if(!invalid_ansi) {
        invalid_ansi = 1;
        warning(
          "Invalid ESC sequence at index %.0f, byte %d.",
          (double) i + 1, csi.start - chr + 1
        );
      }
      chr_track = csi.start + csi.len;
    }
    // Update string

    if(has_ansi) {
      // Copy final chunk if it exists because above we only memcpy when we
      // encounter the tag

      if(*chr_track) {
        const char * chr_end = chr + LENGTH(input_chr);
        if(!chr_end) {
          // nocov start
          error(
            "%s%s",
            "Internal Error: failed to find str end, ",
            "contact maintainer."
          );
          // nocov end
        } else if(chr_end > chr_track) {
          memcpy(res_track, chr_track, chr_end - chr_track);
          res_track += chr_end - chr_track;
      } }
      *res_track = '\0';
      SEXP chr_sexp = PROTECT(
        mkCharLenCE(
          res_start, res_track - res_start, getCharCE(input_chr)
      ) );
      SET_STRING_ELT(res_fin, i, chr_sexp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res_fin;
}
/*
 * Strips Extra ASCII Spaces
 *
 * Won't do anything about weird UTF8 spaces, etc.  Originally used to have the
 * option to handle ANSI ESC sequences and other escapes, but we ditched that in
 * favor of simplicity.  All those characters / sequences are treated as normal
 * characters, except for the newline.
 *
 * Allows two spaces after periods, question marks, and exclamation marks.  This
 * is to line up with strwrap behavior.
 */

SEXP FANSI_process(SEXP input, struct FANSI_buff *buff) {
  if(TYPEOF(input) != STRSXP) error("Input is not a character vector.");

  SEXP res = PROTECT(input);  // dummy PROTECT
  int strip_any = 0;          // Have any elements in the STRSXP been stripped

  R_xlen_t len = XLENGTH(res);
  for(R_xlen_t i = 0; i < len; ++i) {
    const char * string = CHAR(STRING_ELT(res, i));
    const char * string_start = string;
    char * buff_track;

    R_len_t len_j = LENGTH(STRING_ELT(res, i));
    int strip_this, to_strip, to_strip_nl, punct_prev, punct_prev_prev,
        space_prev, space_start, para_start, newlines, newlines_start;

    strip_this = to_strip = to_strip_nl = punct_prev = punct_prev_prev =
      space_prev = space_start = newlines = newlines_start = 0;

    para_start = 1;

    R_len_t j_last = 0;

    // All spaces [ \t\n] are converted to spaces.  First space is kept, unless
    // right after [.?!][)\\"']{0,1}, in which case one more space can be kept.
    //
    // One exception is that sequences of spaces that resolve to more than one
    // newline are kept as a pair of newlines.
    //
    // We purposefully allow ourselves to read up to the NULL terminator.

    for(R_len_t j = 0; j <= len_j; ++j) {

      int newline = string[j] == '\n';
      int tab = string[j] == '\t';

      // TBD: PROBABLY GOING TO GIVE UP ON ESC SEQ PARSING:
      // Handle ESC sequences; pretend they don't exist; some question whether
      // we should only do the ANSI csi sequences to better align with what
      // stwrap does, or also strip the C0 sequences.  Probably strip the C0
      // sequences and recognize that we'll get different results if those are
      // present.

      if(newline) {
        if(!newlines) {
          newlines_start = j;
          to_strip_nl = to_strip;  // how many chrs need stripping by first nl
        }
        ++newlines;
      }
      int space = ((string[j] == ' ') || tab || newline);
      int line_end = !string[j];

      // Need to keep track if we're in a sequence that starts with a space in
      // case a line ends, as normally we keep one or two spaces, but if we hit
      // the end of the line we don't want to keep them.

      if(space && !para_start) {
        if(!space_prev) space_start = 1;
        else if(space && space_prev && punct_prev_prev) space_start = 2;
      }
      /*
      Rprintf(
        "pr_st: %d %d strip: %d to_strip: %d j: %d spc: %d %d %d w: %d strip_this: %d chr: %c\n",
        para_start, newlines, strip, to_strip,
        j, space, space_prev, space_start,
        (!strip && to_strip) || (!string[j] && strip_this),
        strip_this,
        (string[j] ? string[j] : '~')
      );
      */
      // transcribe string if:
      if(
        // we've hit something that we don't need to strip, and we have accrued
        // characters to strip
        (!space && to_strip)
        ||
        // string end and we've already stripped previously or ending in spaces
        (line_end && (strip_this || space_start))
      ) {
        // need to copy entire STRSXP since we haven't done that yet
        if(!strip_any) {
          UNPROTECT(1);  // input is still protected
          res = PROTECT(duplicate(input));
          strip_any = 1;
        }
        // Make sure buffer is big enough
        if(!strip_this) {
          FANSI_size_buff(buff, len_j + 1);
          buff_track = buff->buff;
          strip_this = 1;
        }
        // newlines normally act as spaces, but if there are two or more in a
        // sequence of tabs/spaces then they behave like a paragraph break
        // so we will replace that sequence with two newlines;

        char spc_chr = ' ';
        int copy_to = j;

        if(newlines > 1) {
          copy_to = newlines_start;
          space_start = 2;
          to_strip = to_strip_nl;
          spc_chr = '\n';
        }
        // Copy the portion up to the point we know should be copied, will add
        // back spaces and/or newlines as needed

        int copy_bytes =
          copy_to -      // current position
          j_last -       // less last time we copied
          to_strip;      // less extra stuff to strip

        /*
        Rprintf(
          "Copy bytes %d j: %d j_last: %d to_str: %d spc_str: %d, buff_t: %d\n",
          copy_bytes, j, j_last, to_strip,
          space_start,
          buff_track - buff->buff
        );
        */
        if(copy_bytes) {
          memcpy(buff_track, string_start, copy_bytes);
          buff_track += copy_bytes;
        }
        // Overwrite the trailing bytes with spaces or newlines as needed
        // because we could have tabs in there; note that we can have
        // `copy_bytes` == 0 and still want to do this (e.g. leading '\n\n')

        if(!line_end) {
          if(space_start) *(buff_track++) = spc_chr;
          if(space_start > 1) *(buff_track++) = spc_chr;
        }
        string_start = string + j;
        j_last = j;
        to_strip = space_start = newlines = 0;
      } else if(space) to_strip++;

      para_start = newlines > 1;
      space_prev = space;
      punct_prev_prev = punct_prev;

      // To match what `strwrap` does, we treat as punctuation [.?!], and also
      // treat them as punctuation if they are followed by closing quotes or
      // parens.

      punct_prev =
        (string[j] == '.' || string[j] == '!' || string[j] == '?') ||
        (
          punct_prev &&
          (string[j] == '"' || string[j] == '\'' || string[j] == ')')
        );
    }
    if(strip_this) {
      /*
      Rprintf(
        "About to write n: %d %p %p\n",
        buff->buff - buff_start, buff->buff,
        buff_start
      );
      */
      *(buff_track) = 0;

      SEXP chrsxp = PROTECT(
        mkCharLenCE(
          buff->buff, buff_track - buff->buff, getCharCE(STRING_ELT(input, i))
      ) );
      SET_STRING_ELT(res, i, chrsxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res;
}

SEXP FANSI_process_ext(SEXP input) {
  struct FANSI_buff buff = {.len=0};
  return FANSI_process(input, &buff);
}
