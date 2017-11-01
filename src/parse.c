/*
Copyright (C) 2017  Brodie Gaslam

This file is part of "fansi - ANSI CSI-aware String Functions"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/
#include "fansi.h"

// note we use long int b/c these numbers are going back to R

inline int safe_add(int a, int b) {
  if(a > INT_MAX - b) error("int overflow");
  return a + b;
}
/*
 * Create a state structure with everything set to zero
 *
 * We rely on struct initialization to set everything else to zero.
 */
struct FANSI_state FANSI_state_init() {
  return (struct FANSI_state) {
    .color = -1, .bg_color = -1, .pos_ansi=0, .pos_raw=0, .pos_byte=0,
    .pos_width = 0
  };
}
/*
 * Reset all the display attributes, but not the position ones
 */
struct FANSI_state FANSI_reset_state(struct FANSI_state state) {
  state.style = 0;
  state.color = -1;
  for(int i = 0; i < 4; i++) state.color_extra[i] = 0;
  state.bg_color = -1;
  for(int i = 0; i < 4; i++) state.bg_color_extra[i] = 0;

  return  state;
};

// Can a byte be interpreted as ASCII number?

int FANSI_is_num(const char * string) {
  return *string >= 48 && *string <= 57;
}
// Convert a char value to number by subtracting the zero char; only intended
// for use with string values in [0-9]

unsigned int FANSI_as_num(const char * string) {
  if(*string < 0 || *string > 127)
    error("Currently only ASCII-128 characters are supported");
  if(!FANSI_is_num(string))
    error("Internal Error: attempt to convert non-numeric char to int.");

  return (unsigned int) (*string - '0');
}
// Valid end to a CSI SGR numeric token?

int FANSI_is_tok_end(const char * string) {
  return *string == ';' || *string == 'm';
}

// Store the result of reading a token

struct FANSI_tok_res {
  unsigned int val;         // The actual value of the token
  int len;                  // How many character in the token
  // Whether it was a legal token, 0=no, 1=no, but it only contained numbers so
  // it's okay to keep parsing other ones, 2=yes (0-999)
  int success;
  int last;                 // Whether it ended in 'm' (vs. ';')
};
/*
 * Attempts to read CSI SGR tokens
 *
 * See struct FANSI_tok_res for return value details
 */

struct FANSI_tok_res FANSI_parse_token(const char * string) {
  unsigned int mult, val;
  int len, len_prev;
  int success, last;
  int limit = 5;
  success = len = val = last = 0;
  mult = 1;

  while(FANSI_is_num(string) && (--limit)) {
    ++string;
    len_prev = len;
    ++len;
    if(len < len_prev)
      error(
        "Internal Error: overflow trying to parse ANSI esc seq token."
      );
  }
  // Only succed if number isn't too long and terminates in ';' or 'm'

  if(FANSI_is_tok_end(string)) {
    last = (*string == 'm');
    if(len > 3) {
      success = 1;
    } else {
      success = 2;

      // Read the string backwards (assume 0 if no string) and turn it into a
      // number

      int len2 = len;
      while(len2--) {
        val += FANSI_as_num(--string) * mult;
        mult *= 10;
  } } }
  return (struct FANSI_tok_res) {
    .val=val, .len=len, .success=success, .last=last
  };
}
/*
 * Call with state once we've advanced the string just past a [34]8;2
 *
 * Will return the state after reading through the next three tags which in
 * theory correspond to to the r,g,b values.
 *
 * @param mode is whether we are doing foregrounds (3) or backgrounds (4)
 * @param colors is whether we are doing palette (5), or rgb truecolor (2)
 */
struct FANSI_state FANSI_parse_colors(struct FANSI_state state, int mode) {
  if(mode != 3 && mode != 4)
    error("Internal Error: parsing color with invalid mode.");

  struct FANSI_tok_res res;
  int rgb[4] = {0};
  int col = 8;
  int valid_col = 1;
  int i_max;

  // First, figure out if we are in true color or palette mode

  res = FANSI_parse_token(&state.string[state.pos_byte]);
  state.pos_byte = safe_add(state.pos_byte, safe_add(res.len, 1));
  state.last = res.last;
  if(res.success && ((res.val != 2 && res.val != 5) || res.last)) {
    // weird case, we don't want to advance the position here because `res.val`
    // needs to be interpreted as potentially a non-color style and the prior
    // 38 or 48 just gets tossed

    state.pos_byte -= (res.len + 1);
  } else if(!res.success) {
    state.fail = 1;
  } else if(res.success == 2) {
    int colors = res.val;
    if(colors == 2) {
      i_max = 3;
    } else if (colors == 5) {
      i_max = 1;
    } else error("Internal Error: 1301341"); // nocov

    rgb[0] = colors;

    // Parse through the subsequent tokens

    for(int i = 0; i < i_max; ++i) {
      res = FANSI_parse_token(&state.string[state.pos_byte]);
      state.pos_byte = safe_add(state.pos_byte, safe_add(res.len, 1));
      state.last = res.last;
      if(res.success) {
        int early_end = res.last && i < (i_max - 1);
        if(res.success == 2 && res.val < 256 && !early_end) {
          rgb[i + 1] = res.val;
        } else {
          // Not a valid color; doesn't break parsing so that we end up with the
          // cursor at the right place

          valid_col = 0;
        }
      } else {
        state.fail = 1;
        break;
      }
    }
    // Failure handling happens in the main loop, we just need to ensure the
    // byte position is correct

    if(!state.fail) {
      if(!valid_col) {
        for(int i = 0; i < 4; i++) rgb[i] = 0;
        col = -1;
      }
      if(mode == 3) {
        state.color = col;
        for(int i = 0; i < 4; i++) state.color_extra[i] = rgb[i];
      } else if (mode == 4) {
        state.bg_color = col;
        for(int i = 0; i < 4; i++) state.bg_color_extra[i] = rgb[i];
      }
    }
  } else if(res.success == 1) {
    // do nothing here
  } else error("Internal Error: 23234kdshf");

  return state;
}
/*
 * Compute the state given a character position (raw position)
 *
 * Assumption is that any byte > 127 is UTF8 (i.e. input strings must be all
 * legal ASCII, or must have been translated to UTF8).  Potential for relaxation
 * with latin-1 since those are all single byte single width, but right now
 * that's not implemented.
 *
 * @param pos the raw position (i.e. treating parseable ansi tags as zero
 *   length) we want the state for
 * @param string the string we want to compute the state for.  NOTE: this is
 *   assumed to NULL terminated, which should be a safe assumption since the
 *   source of these strings are going to be CHARSXPs.
 * @param struct the state to start from, it should be either something produced
 *   by `state_init`, or the result of running this function on the same string,
 *   but for a position earlier in the string.  The latter use case avoids us
 *   having to reparse the string if we've already retrieved the state at an
 *   earlier position.
 * @param int type whether to use character (0), width (1), or byte (2) when
 *   computing the position
 * @param lag in cases where requested breakpoint is not feasible because of
 *   multi width characters, whether to return end of prior (0) or the start of
 *   the next (1)
 * @param end whether the request is made for the ends of the string (i.e. as
 *   part of the `stop` parameter) (1), or not (0) (i.e. as part of the start
 *   parameters).
 */
struct FANSI_state FANSI_state_at_position(
    int pos, const char * string, struct FANSI_state state, int type,
    int lag, int end
) {
  // Sanity checks, first one is a little strict since we could have an
  // identical copy of the string, but that should not happen in intended use
  // case since we'll be uniqueing prior; ultimtely we should just make the
  // string part of the state and get rid of the string arg

  if(state.string && state.string != string)
    error("Cannot re-use a state with a different string.");
  if(pos < state.pos_raw)
    error(
      "Cannot re-use a state for a later position (%0f) than `pos` (%0f).",
      (double) state.pos_raw, (double) pos
    );

  state.string = string;
  int pos_byte_prev = 0;
  int cond = 0;

  // Need to reset pos_width_target since it could be distorted by a previous
  // middle of wide char event

  state.pos_width_target = state.pos_width;

  // frame shift ourselves (i.e. all position shifts are encoded exclusively in
  // that variable).  This is a little less efficient that just moving the
  // pointer along but probably not worth optimizing.
  //
  // Loosely related, since we don't make any distinction between byte and ansi
  // position for now we ignore `pos_ansi` until the very end

  struct FANSI_state state_prev = state;

  while(1) {
    if(!string[state.pos_byte]) break;
    switch(type) {
      case 0: cond = pos - state.pos_raw; break;
      case 1: {
        // For wide display characters, it matters whether we are looking for
        // the end of a character or the beginning.  E.g. in a single two-wide
        // character that spans positions 0-1, when we request position 0 and
        // expect the beginning of the string, then we are start at the
        // boundary, whereas if we request 0 at the end of the string, then we
        // are in the middle of the two-wide character.
        cond = pos - state.pos_width + (state.pos_width > 1 && end ? 1 : 0);
        break;
      }
      case 2: cond = pos - state.pos_byte; break;
      default:
        // nocov start
        error("Internal Error: Illegal offset type; contact maintainer.");
        // nocov end
    }
    Rprintf(
      "cond %d lag %d pos %d width %d byte %d type %d end %d\n",
      cond, lag, pos, state.pos_width, state.pos_byte, type, end
    );
    if(cond < 0) {
      // Should only happen with 'type' width, which means we overshot the
      // breakpoint due to a wide character

      if(type != 1)
        // nocov start
        error(
          "%s%s",
          "Internal Error: partial width should only happen in type 'width'; ",
          "contact maintainer."
        );
        // nocov end

      if(!lag) state = state_prev;
      state.pos_width_target = pos;
    }
    // Reset internal controls

    state_prev = state;
    state.fail = state.last = 0;
    pos_byte_prev = state.pos_byte;

    // Handle UTF-8, we need to record the byte size of the sequence as well as
    // the corresponding display width.

    // Urgh, don't like the fact that we have two branches with `state.pos_raw <
    // pos`...  Annoying because we don't apply that condition to the ESC[
    // sequence since we can start with that as a zero width element...

    if(string[state.pos_byte] < 0 && cond > 0) {
      // NOTE: need to look ahead for zero width characters?

      int byte_size = FANSI_utf8clen(string[state.pos_byte]);

      // In order to compute char display width, we need to create a charsxp
      // with the sequence in question.  Hopefully not too much overhead since
      // at least we benefit from the global string hash table
      //
      // Note that we should probably not bother with computing this if display
      // mode is not width as it's probably expensive.

      SEXP str_chr =
        PROTECT(mkCharLenCE(string + state.pos_byte, byte_size, CE_UTF8));
      int disp_size = R_nchar(
        str_chr, Width, FALSE, FALSE, "when computing display width"
      );
      UNPROTECT(1);

      // Need to check overflow?  Really only for pos_width?  Maybe that's not
      // even true because you need at least two bytes to encode a double wide
      // character, and there is nothing wider than 2?

      state.pos_byte += byte_size;
      ++state.pos_ansi;
      ++state.pos_raw;
      /*
      Rprintf(
        "old width: %d extra: %d new width: %d\n",
        state.pos_width, disp_size, state.pos_width + disp_size
      );
      */
      state.pos_width += disp_size;
      state.pos_width_target += disp_size;
    } else if(
      string[state.pos_byte] == 27 && string[state.pos_byte + 1] == '['
    ) {
      // make a copy of the struct so we don't modify state if it turns out this
      // is an invalid SGR

      state.pos_byte = safe_add(state.pos_byte, 2);
      struct FANSI_state state_tmp = state;
      struct FANSI_tok_res tok_res = {.success = 0};

      // Loop through the SGR; each token we process successfully modifies state
      // and advances to the next token

      do {
        tok_res = FANSI_parse_token(&string[state.pos_byte]);
        state.pos_byte = safe_add(state.pos_byte, safe_add(tok_res.len, 1));
        state.last = tok_res.last;

        if(!tok_res.success) {
          state.fail = 1;
        } else if(tok_res.success == 2) {
          // We have a reasonable CSI value, now we need to check whether it
          // actually corresponds to anything that should modify state

          if(!tok_res.val) {
            state = FANSI_reset_state(state);
          } else if (tok_res.val < 10) {
            // This is a style, so update the bit mask by enabing the style
            state.style |= 1U << tok_res.val;
          } else if (
            tok_res.val == 20 || tok_res.val == 21 || tok_res.val == 26
          ) {
            // these are corner case tags that aren't actually closing tags or
            // could be interpreted as non-closing tags; we need to figure out
            // what to do with this (just return invalid? but it could change
            // display).

            warning("Encountered non handled tag");
          } else if (tok_res.val == 22) {
            // Turn off bold or faint
            state.style &= ~(1U << 1U);
            state.style &= ~(1U << 2U);
          } else if (tok_res.val == 25) {
            // Turn off blinking
            state.style &= ~(1U << 5U);
            state.style &= ~(1U << 6U);
          } else if (tok_res.val >= 20 && tok_res.val < 30) {
            // All other styles are 1:1
            state.style &= ~(1U << (tok_res.val - 20));
          } else if (tok_res.val >= 30 && tok_res.val < 50) {
            // Colors; much shared logic between color and bg_color, so
            // combining that here

            int foreground = tok_res.val < 40; // true then color, else bg color
            int col_code = tok_res.val - (foreground ? 30 : 40);

            if(col_code == 9) col_code = -1;
            if(foreground) state.color = col_code;
            else state.bg_color = col_code;

            // Handle the special color codes, need to parse some subsequent
            // tokens

            if(col_code == 8) {
              state = FANSI_parse_colors(state, foreground ? 3 : 4);
            }
          }
        } else if (tok_res.success == 1) {
          // "valid" token, but meaningless so just skip and move on to next
        } else error("Internal Error: 350au834.");
        // Note that state.last may be different to tok_res.last when we parse
        // colors of the 38;5;... or 38;2;... variety.

        if(tok_res.last || state.last || state.fail) break;
      } while(1);

      // Invalid escape sequences count as normal characters, and at this point
      // the only way to have a valid escape seq is if it ends in 'm'

      int byte_offset = state.pos_byte - pos_byte_prev;

      if(state.fail) {
        state.pos_raw += byte_offset;
        state_tmp.pos_raw = state.pos_raw;
        state_tmp.pos_byte = state.pos_byte;
        state = state_tmp;
      }
      state.pos_ansi += byte_offset;
    } else if (cond > 0) {
      // Advance one character

      if(state.pos_byte == INT_MAX)
        error("Internal Error: counter overflow while reading string.");

      ++state.pos_byte;
      ++state.pos_ansi;
      ++state.pos_raw;
      ++state.pos_width;
      ++state.pos_width_target;
    } else {
      // We allowed entering loop so long as state.pos_raw <= pos, but we
      // actually don't want to increment counter if at pos; we only entered so
      // that we could potentially parse a zero length sequence that starts at
      // pos

      if(end && type == 1){
        // If in end mode and width mode, we need to back off the ansi counter
        // since we didn't actually advance a character, only width
        --state.pos_ansi;
      }
      break;
    }
  }
  // Special handling in width mode b/c not guaranteed to get same offset we
  // requested

  if(type == 1) {
    if(state.pos_width_target != pos) state.pos_width_target = pos;
  }
  return state;
}
/*
 * We always include the size of the delimiter
 */
unsigned int FANSI_color_size(int color, int * color_extra) {
  unsigned int size = 0;
  if(color == 8 && color_extra[0] == 2) {
    size = 3 + 2 + 4 * 3;
  } else if (color == 8 && color_extra[0] == 5) {
    size = 3 + 2 + 4 * 3;
  } else if (color == 8) {
    error("Internal Error: unexpected compound color format");
  } else if (color >= 0 && color < 10) {
    size = 3;
  } else if (color >= 0) {
    error("Internal Error: unexpected compound color format 2");
  }
  return size;
}
/*
 * Compute length in characters for a number
 */
unsigned int FANSI_num_chr_len(unsigned int num) {
  // + 1.00001 to account for 0
  unsigned int log_len = (unsigned int) ceil(log10(num + 1.00001));
  return log_len;
}
/*
 * Write extra color info to string
 *
 * Modifies string by reference, returns next position in string.  This assumes
 * that the 3 or 4 has been written already and that we're not in a -1 color
 * state that shouldn't have color.
 *
 * String should be a pointer to the location we want to start writing, so
 * should already be offset.  The return value is the offset from the original
 * position
 */
unsigned int FANSI_color_write(
  char * string, int color, int * color_extra, int mode
) {
  if(mode != 3 && mode != 4)
    error("Internal Error: color mode must be 3 or 4");

  unsigned int str_off = 0;
  if(color > 0) {
    string[str_off++] = mode == 3 ? '3' : '4';

    if(color != 8) {
      string[str_off++] = '0' + color;
      string[str_off++] = ';';
    } else {
      string[str_off++] = '8';
      string[str_off++] = ';';

      int write_chrs = -1;
      if(color_extra[0] == 2) {
        write_chrs = sprintf(
          string, "2;%d;%d;%d;", color_extra[1], color_extra[2], color_extra[3]
        );
      } else if (color_extra[0] == 5) {
        write_chrs = sprintf(string, "5;%d;", color_extra[1]);
      } else error("Internal Error: unexpected color code.");

      if(write_chrs < 0) error("Internal Error: failed writing color code.");
      str_off += write_chrs;
    }
  }
  return str_off;
}
/*
 * Generate the ANSI tag corresponding to the state
 */
const char * FANSI_state_as_chr(struct FANSI_state state) {
  // First pass computes total size of tag; we need to account for the separtor
  // as well

  unsigned int tag_len = 0;
  for(unsigned int i = 1; i < 10; i++) tag_len += ((1U << i) & state.style) * 2;

  tag_len += FANSI_color_size(state.color, state.color_extra);
  tag_len += FANSI_color_size(state.bg_color, state.bg_color_extra);

  // Now allocate and generate tag

  const char * tag_res = "";
  if(tag_len) {
    tag_len += 3;  // for CSI, and ending NULL
    char * tag_tmp = R_alloc(tag_len + 1 + 2, sizeof(char));
    unsigned int str_pos = 0;
    tag_tmp[str_pos++] = 27;    // ESC
    tag_tmp[str_pos++] = '[';

    // styles

    for(unsigned int i = 1; i < 10; i++) {
      if((1U << i) & state.style) {
        tag_tmp[str_pos++] = '0' + i;
        tag_tmp[str_pos++] = ';';
      }
    }
    // colors

    str_pos += FANSI_color_write(
      &(tag_tmp[str_pos]), state.color, state.color_extra, 3
    );
    str_pos += FANSI_color_write(
      &(tag_tmp[str_pos]), state.bg_color, state.bg_color_extra, 4
    );
    // Finalize (note, in some cases we slightly overrallocate)

    if(str_pos + 1 > tag_len)
      error(
        "Internal Error: tag mem allocation mismatch (%u, %u)", str_pos, tag_len
      );
    tag_tmp[str_pos - 1] = 'm';
    tag_tmp[str_pos] = '\0';
    tag_res = (const char *) tag_tmp;
  }
  return tag_res;
}
/*
 * Determine whether two state structs have same style
 *
 * This only compares the style pieces
 *
 * Returns 1 if the are different, 0 if they are equal
 */
int FANSI_state_comp(struct FANSI_state target, struct FANSI_state current) {
  return !(
    target.style == current.style &&
    target.color == current.color &&
    target.bg_color == current.bg_color &&
    target.color_extra[0] == current.color_extra[0] &&
    target.bg_color_extra[0] == current.bg_color_extra[0] &&
    target.color_extra[1] == current.color_extra[1] &&
    target.bg_color_extra[1] == current.bg_color_extra[1] &&
    target.color_extra[2] == current.color_extra[2] &&
    target.bg_color_extra[2] == current.bg_color_extra[2] &&
    target.color_extra[3] == current.color_extra[3] &&
    target.bg_color_extra[3] == current.bg_color_extra[3]
  );
}
/*
 * R interface for FANSI_state_at_position
 *
 * @param string we're interested in state of
 * @param pos integer positions along the string, one index, sorted
 */

SEXP FANSI_state_at_pos_ext(
  SEXP text, SEXP pos, SEXP type, SEXP lag, SEXP ends
) {
  if(TYPEOF(text) != STRSXP && XLENGTH(text) != 1)
    error("Argument `text` must be character(1L)");
  if(TYPEOF(pos) != INTSXP)
    error("Argument `pos` must be integer");
  if(TYPEOF(lag) != LGLSXP)
    error("Argument `lag` must be logical");
  if(XLENGTH(pos) != XLENGTH(lag))
    error("Argument `lag` must be the same length as `pos`");
  if(XLENGTH(pos) != XLENGTH(ends))
    error("Argument `ends` must be the same length as `pos`");

  const int res_cols = 4;
  R_xlen_t len = XLENGTH(pos);

  if(len > R_XLEN_T_MAX / res_cols) {
    error("Argument `pos` may be no longer than R_XLEN_T_MAX / %d", res_cols);
  }
  SEXP text_chr = asChar(text);
  const char * string = CHAR(text_chr);
  struct FANSI_state state = FANSI_state_init();
  struct FANSI_state state_prev = FANSI_state_init();

  // Allocate result, will be a res_cols x n matrix.  A bit wasteful to record
  // all the color values given we'll rarely use them, but variable width
  // structures are likely to be much slower.  We could encode most color values
  // into one int but it would be a little annoying to retrieve them

  const char * rownames[res_cols] = {
    "pos.byte", "pos.raw", "pos.ansi", "pos.width"
  };
  SEXP res_rn = PROTECT(allocVector(STRSXP, res_cols));
  for(int i = 0; i < res_cols; i++)
    SET_STRING_ELT(res_rn, i, mkChar(rownames[i]));

  // Result will comprise a character vector with all the state tags at the
  // position as well as the various position translations in a matrix with as
  // many *columns* as the character vector has elements

  SEXP res_mx = PROTECT(allocVector(INTSXP, res_cols * len));
  SEXP dim = PROTECT(allocVector(INTSXP, 2));
  SEXP dim_names = PROTECT(allocVector(VECSXP, 2));
  INTEGER(dim)[0] = res_cols;
  INTEGER(dim)[1] = len;
  setAttrib(res_mx, R_DimSymbol, dim);
  SET_VECTOR_ELT(dim_names, 0, res_rn);
  SET_VECTOR_ELT(dim_names, 1, R_NilValue);
  setAttrib(res_mx, R_DimNamesSymbol, dim_names);

  SEXP res_str = PROTECT(allocVector(STRSXP, len));
  SEXP res_chr, res_chr_prev = PROTECT(mkChar(""));

  int pos_prev = -1;
  int utf8_loc = FANSI_is_utf8_loc();
  cetype_t enc_type = getCharCE(text_chr);
  int translate = !(
    (utf8_loc && enc_type == CE_NATIVE) || enc_type == CE_BYTES ||
    enc_type == CE_UTF8
  );
  if(translate) string = translateCharUTF8(text_chr);

  // Compute state at each `pos` and record result in our results matrix

  int type_int = asInteger(type);

  for(R_xlen_t i = 0; i < len; i++) {
    R_CheckUserInterrupt();
    int pos_i = INTEGER(pos)[i];
    if(text_chr == NA_STRING || pos_i == NA_INTEGER) {
      for(R_xlen_t j = 0; j < res_cols; j++)
        INTEGER(res_mx)[i * res_cols + j] = NA_INTEGER;
    } else {
      if(!(pos_i > pos_prev))
        error("Internal Error: `pos` must be sorted %d %d.", pos_i, pos_prev);
      else pos_prev = pos_i;

      state = FANSI_state_at_position(
        pos_i, string, state, type_int, INTEGER(lag)[i], INTEGER(ends)[i]
      );

      // Record position, but set them back to 1 index

      INTEGER(res_mx)[i * res_cols + 0] = safe_add(state.pos_byte, 1);
      INTEGER(res_mx)[i * res_cols + 1] = safe_add(state.pos_raw, 1);
      INTEGER(res_mx)[i * res_cols + 2] = safe_add(state.pos_ansi, 1);
      INTEGER(res_mx)[i * res_cols + 3] = safe_add(state.pos_width_target, 1);

      // Record color tag if state changed

      if(FANSI_state_comp(state, state_prev)) {
        res_chr = PROTECT(mkChar(FANSI_state_as_chr(state)));
      } else {
        res_chr = PROTECT(res_chr_prev);
      }
      SET_STRING_ELT(res_str, i, res_chr);
      res_chr_prev = res_chr;
      UNPROTECT(1);  // note res_chr is protected by virtue of being in res_str
    }
    state_prev = state;
  }
  SEXP res_list = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res_list, 0, res_str);
  SET_VECTOR_ELT(res_list, 1, res_mx);

  UNPROTECT(7);
  return(res_list);
}
