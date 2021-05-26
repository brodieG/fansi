/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
 * Since we do not use FANSI_read_next, we don't care about conversions to
 * UTF8.
 *
 * @param warn normally TRUE or FALSE, but internally we allow it to be an
 *   integer so that we can use a special mode where if == 2 then we return the
 *   fact that there was a warning as an attached attributed, as opposed to
 *   actually throwing the warning
 */

SEXP FANSI_strip(SEXP x, SEXP ctl, SEXP warn) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` should be a character vector.");  // nocov
  if(TYPEOF(ctl) != INTSXP)
    error("Internal Error: `ctl` should integer.");      // nocov
  if(
    (TYPEOF(warn) != LGLSXP && TYPEOF(warn) != INTSXP) || XLENGTH(warn) != 1 ||
    INTEGER(warn)[0] == NA_INTEGER
  )
    error("Internal Error: `warn` should be TRUE or FALSE");  // nocov

  int warn_int = asInteger(warn);
  if(warn_int < 0 || warn_int > 2)
    error("Argument `warn` must be between 0 and 2 if an integer.");  // nocov

  // Compress `ctl` into a single integer using bit flags

  int ctl_int = FANSI_ctl_as_int(ctl);
  R_xlen_t i, len = xlength(x);
  SEXP res_fin = x;

  PROTECT_INDEX ipx;
  // reserve spot if we need to alloc later
  PROTECT_WITH_INDEX(res_fin, &ipx);

  int any_ansi = 0;
  R_len_t mem_req = 0;          // how much memory we need for each ansi

  struct FANSI_csi_pos csi;

  // Compute longest char element, we'll assume this is the required size of our
  // string buffer.  This is potentially wastful if there is one very large
  // CSI-less string and all the CSI strings are short.  The alternative would
  // be to keep reallocating to a larger size, maybe by 2x, but then that
  // requires the growing buffer, etc.  For now go simple

  for(i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    SEXP x_chr = STRING_ELT(x, i);
    FANSI_check_chrsxp(x_chr, i);
    R_len_t chr_len = LENGTH(STRING_ELT(x, i));
    if(chr_len > mem_req) mem_req = chr_len;
  }
  // Now strip
  int invalid_ansi = 0;
  R_xlen_t invalid_idx = 0;
  char * chr_buff;

  for(i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    SEXP x_chr = STRING_ELT(x, i);
    if(x_chr == NA_STRING) continue;
    FANSI_check_chrsxp(x_chr, i);

    int has_ansi = 0;
    const char * chr = CHAR(x_chr);
    const char * chr_track = chr;
    char * res_track = NULL, * res_start = NULL;

    // We re-use the allocated buffer for every string in the character
    // vector, which is why we re-assign to chr_buff here.  chr_buff will be
    // allocated in the loop below the first time it is needed, but we need to
    // re-assign re_start / res_track .

    res_start = res_track = chr_buff;

    while(1) {
      csi = FANSI_find_esc(chr_track, ctl_int);
      // Currently we can't know for sure if a ESC seq that isn't a CSI is only
      // two long so we should warn if we hit one, or otherwise and invalid seq
      if(
        !invalid_ansi && (
          !csi.valid ||
          ((csi.ctl & FANSI_CTL_ESC) & ctl_int)
        )
      ) {
        invalid_ansi = 1;
        invalid_idx = i;
      }
      if(csi.len) {
        has_ansi = 1;
        if(csi.start - chr > FANSI_lim.lim_int.max - csi.len)
          // nocov start
          error(
            "%s%s",
            "Internal Error: string longer than INT_MAX encountered, should ",
            "not be possible."
          );
          // nocov end

        // As soon as we encounter ansi in any of the character vector elements,
        // allocate vector to track what has ansi

        if(!any_ansi) {
          any_ansi = 1;

          // We need to allocate a result vector since we'll be stripping ANSI
          // CSI, and also the buffer we'll use to re-write the CSI less strings

          REPROTECT(res_fin = duplicate(x), ipx);

          // Note the is guaranteed to be an over-allocation, as it's the
          // longest string in the vector.  It should be guaranteed to be no
          // longer than R_LEN_T_MAX.

          // The character buffer is large enough for the largest element in the
          // vector, and is re-used for every element in the vector.

          chr_buff = (char *) R_alloc(((size_t) mem_req) + 1, sizeof(char));
          res_start = res_track = chr_buff;
        }
        // Is memcpy going to cause problems again by reading past end of
        // block?  Didn't in first valgrind check.

        memcpy(res_track, chr_track, csi.start - chr_track);
        res_track += csi.start - chr_track;
        chr_track = csi.start + csi.len;
      } else break;
    }
    // Update string

    if(has_ansi) {
      // Copy final chunk if it exists because above we only memcpy when we
      // encounter the tag

      if(*chr_track) {
        const char * chr_end = chr + LENGTH(x_chr);
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

      FANSI_check_chr_size(res_start, res_track, i);
      SEXP chr_sexp = PROTECT(
        FANSI_mkChar(
          res_start, res_track - res_start, getCharCE(x_chr), i
      ) );
      SET_STRING_ELT(res_fin, i, chr_sexp);
      UNPROTECT(1);
    }
  }
  if(invalid_ansi) {
    switch(warn_int) {
      case 1: {
        warning(
          "Encountered %s index [%jd], %s%s",
          "invalid or possibly incorreclty handled ESC sequence at ",
          FANSI_ind(invalid_idx),
          "see `?unhandled_ctl`; you can use `warn=FALSE` to turn ",
          "off these warnings."
        );
        break;
      }
      case 2: {
        SEXP attrib_val = PROTECT(ScalarLogical(1));
        setAttrib(res_fin, FANSI_warn_sym, attrib_val);
        UNPROTECT(1);
        break;
  } } }
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

  PROTECT_INDEX ipx;
  SEXP res = input;
  PROTECT_WITH_INDEX(res, &ipx);  // reserve spot if we need to alloc later

  int strip_any = 0;          // Have any elements in the STRSXP been stripped

  R_xlen_t len = XLENGTH(res);
  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    SEXP chrsxp = STRING_ELT(res, i);
    FANSI_check_chrsxp(chrsxp, i);
    const char * string = CHAR(chrsxp);
    const char * string_start = string;
    char * buff_track;

    R_len_t len_j = LENGTH(chrsxp);
    int strip_this, to_strip, to_strip_nl, punct_prev, punct_prev_prev,
        space_prev, space_start, para_start, newlines, newlines_start,
        has_tab_or_nl, leading_spaces;

    strip_this = to_strip = to_strip_nl = punct_prev = punct_prev_prev =
      space_prev = space_start = newlines = newlines_start = has_tab_or_nl = 0;

    para_start = leading_spaces = 1;

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

      has_tab_or_nl += newline + tab;

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
        "pr_st: %d %d to_strip: %d j: %d spc: %d %d %d strip_this: %d chr: %c\n",
        para_start, newlines, to_strip, j,
        space, space_prev, space_start,
        strip_this,
        (string[j] ? string[j] : '~')
      );
      */
      // transcribe string if:
      if(
        // we've hit something that we don't need to strip, and we have accrued
        // characters to strip (more than one space, or more than two spaces if
        // preceeded by punct, or leading spaces
        (
          !space && (
            (
              (to_strip && leading_spaces) ||
              (to_strip > 1 && (!punct_prev)) ||
              (to_strip > 2)
            ) ||
            has_tab_or_nl
        ) )
        ||
        // string end and we've already stripped previously or ending in spaces
        (line_end && (strip_this || space_start))
      ) {
        // need to copy entire STRSXP since we haven't done that yet
        if(!strip_any) {
          REPROTECT(res = duplicate(input), ipx);
          strip_any = 1;
        }
        // Make sure buffer is big enough
        if(!strip_this) {
          FANSI_size_buff(buff, (size_t) len_j + 1);
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
        to_strip = space_start = newlines = has_tab_or_nl = leading_spaces = 0;
      } else if(space) {
        to_strip++;
      } else {
        to_strip = space_start = newlines = has_tab_or_nl = leading_spaces = 0;
      }
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
      *(buff_track) = 0;
      FANSI_check_chr_size(buff->buff, buff_track, i);
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
