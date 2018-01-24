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

/*
 * Create a state structure with everything set to zero
 *
 * We rely on struct initialization to set everything else to zero.
 */
struct FANSI_state FANSI_state_init() {
  return (struct FANSI_state) {.color = -1, .bg_color = -1};
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
}
struct FANSI_state FANSI_reset_width(struct FANSI_state state) {
  state.pos_width = 0;
  state.pos_width_target = 0;
  return state;
}
// Can a byte be interpreted as ASCII number?

int FANSI_is_num(const char * string) {
  return *string >= 48 && *string <= 57;
}
// Convert a char value to number by subtracting the zero char; only intended
// for use with string values in [0-9]

int FANSI_as_num(const char * string) {
  if(!FANSI_is_num(string))
    error(
      "Internal Error: attempt to convert non-numeric char (%d) to int.",
      (int) *string
    );
  return (int) (*string - '0');
}
// Store the result of reading a parameter substring token

struct FANSI_tok_res {
  unsigned int val;         // The actual value of the token
  int len;                  // How many character in the token
  int err_code;             // see struct FANSI_state
  int last;                 // Whether it is the last parameter substring
};
/*
 * Attempts to read CSI SGR tokens
 *
 * See struct FANSI_tok_res for return value details
 *
 * Note this makes no attempt to interpret the CSI other than indicate there are
 * odd characters in it.
 */

struct FANSI_tok_res FANSI_parse_token(const char * string) {
  unsigned int mult, val;
  int len, len_intermediate, last, non_standard, private, err_code,
    leading_zeros, not_zero;
  len = len_intermediate = val = last = non_standard = private =
    err_code = leading_zeros = not_zero = 0;
  mult = 1;

  // `private` a bit redundant since we don't actually use it differentially to
  // non-normal

  private = *string >= 0x3C && * string <= 0x3F;

  // cycle through valid parameter bytes

  while(*string >= 0x30 && *string <= 0x3F && *string != ';') {
    int is_zero = *string == '0';
    if(!is_zero && !not_zero) not_zero = 1;
    if(is_zero && !not_zero) ++leading_zeros;
    non_standard |= *string > 0x39;
    string++;
    len = FANSI_add_int(len, 1);
  }
  // check for for intermediate bytes, we allow 'infinite' here even though in
  // practice more than one is likely a bad outcome

  while(*string >= 0x20 && *string <= 0x2F) {
    ++string;
    len_intermediate = FANSI_add_int(len_intermediate, 1);
  }
  // check for final byte

  if((*string == ';' || *string == 'm') && !len_intermediate) {
    // valid end of parameter substring

    if(non_standard) err_code = 1;
    last = (*string == 'm');
  } else if(*string >= 0x40 && *string <= 0x7E && len_intermediate == 1) {
    // valid final byte
    last = 1;
    err_code = 3;
  } else {
    // invalid end
    err_code = 4;
  }
  // Final interpretations; note that anything over 255 cannot be part of a
  // valid SGR sequence

  if(last && (*string != 'm') && err_code < 3) {
    // Not actually an SGR sequence
    err_code = 3;
  } else if(!err_code && (len - leading_zeros) > 3) {
    err_code = 2;
  }
  // Rprintf("    len: %d leading_zeros: %d\n", len, leading_zeros);
  if(!err_code) {
    int len2 = len - leading_zeros;
    while(len2--) {
      val += (FANSI_as_num(--string) * mult);
      mult *= 10;
  } }
  if(val > 255) err_code = 2;

  // If the string didn't end, then we consume one extra character for the
  // ending

  if(*string) ++len;

  return (struct FANSI_tok_res) {
    .val=val, .len=len + len_intermediate,
    .err_code=err_code, .last=last
  };
}
/*
 * Call with state once we've advanced the string just past a [34]8;
 *
 * Will return the state after reading through the next two or four
 * subparameters (depending on whether the first subparameter is 2 or 5) which
 * in theory correspond to to the r,g,b values.
 *
 * @param mode is whether we are doing foregrounds (3) or backgrounds (4)
 * @param colors is whether we are doing palette (5), or rgb truecolor (2)
 */
static struct FANSI_state FANSI_parse_colors(
  struct FANSI_state state, int mode
) {
  if(mode != 3 && mode != 4)
    error("Internal Error: parsing color with invalid mode.");

  struct FANSI_tok_res res;
  int rgb[4] = {0};
  int col = 8;
  int valid_col = 1;
  int i_max;

  // First, figure out if we are in true color or palette mode

  res = FANSI_parse_token(&state.string[state.pos_byte]);
  state.pos_byte = FANSI_add_int(state.pos_byte, res.len);
  state.last = res.last;
  state.err_code = res.err_code;

  if(!state.err_code && ((res.val != 2 && res.val != 5) || state.last)) {
    // weird case, we don't want to advance the position here because `res.val`
    // needs to be interpreted as potentially a non-color style and the prior
    // 38 or 48 just gets tossed (at least this happens on OSX terminal and
    // iTerm)

    state.pos_byte -= (res.len);
    state.err_code = 2;
  } else if(!state.err_code) {
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
      state.pos_byte = FANSI_add_int(state.pos_byte, res.len);
      state.last = res.last;
      state.err_code = res.err_code;

      if(!state.err_code) {
        int early_end = res.last && i < (i_max - 1);
        if(res.val < 256 && !early_end) {
          rgb[i + 1] = res.val;
        } else {
          // Not a valid color; doesn't break parsing so that we end up with the
          // cursor at the right place

          valid_col = 0;
          break;
    } } }
    // Failure handling happens in the main loop, we just need to ensure the
    // byte position and state is correct

    if(!state.err_code) {
      if(!valid_col) {
        for(int i = 0; i < 4; i++) rgb[i] = 0;
        col = -1;
        state.err_code = 2;  // invalid substring
      }
      if(mode == 3) {
        state.color = col;
        for(int i = 0; i < 4; i++) state.color_extra[i] = rgb[i];
      } else if (mode == 4) {
        state.bg_color = col;
        for(int i = 0; i < 4; i++) state.bg_color_extra[i] = rgb[i];
      }
    }
  }
  return state;
}
/*
 * Parses ESC sequences
 *
 * In particular, special treatment for ANSI CSI SGR sequences.
 *
 * @section ANSI CSI:
 *
 * Valid chars:
 *
 * * Paramter Bytes 03/00 - 03/15 (0x30 to 0x3F): [0-9:;<=>?]
 *     * First bit seq should be in 03/00 - 03/11: [0-9:;]
 *     * Otherwise a private spec
 * * If normal spec, then parameter substrings:
 *     * One or more numbers in 0-9
 *     * ':' to separate numbers into decimals
 *     * ';' terminates the substring
 *     * 03/12-03/15 allowed if not in first bytes of the parameter string, but
 *       reserved for future use
 *     * Leading zeros can be omitted
 *     * Empty substrings allowed, and trailing delimiter can/should be omitted
 *  * Intermediate bytes 02/00-02/15 (0x20-0x2F): [ !"#$%&'()*+,\-./]
 *  * Final bytes 04/00-07/14 (0x40-0x7E): [@A-Z\[\\\]\%_`a-z{|}~]
 *
 * @section Possible Outcomes:
 *
 * See `FANSI_state.err_code`
 *
 * From some experimentation it doesn't seem like the intermediate bytes are
 * completely supported by the OSX terminal...  A single itermediate works okay,
 * more and it becomes a crapshoot, '!' in particular seems to cause problems.
 *
 * iTerm seems to read only the last 8 bits of any number specified, at least
 * in the [34]8;2;... subparameter strings.  In other words, '255' is equivalent
 * to '511' or some such.  Terminal doesn't recognize the rgb style
 * sub-parameters, and in fact just seems to start interpreting subparameters
 * right after the 38/48, so '38;2m' is read as '2m'.
 *
 * Both iTerm and Terminal seem to interpret the next subparameter after a 38/48
 * if is not a 2 or a 5.
 *
 * @section Illegal Bytes:
 *
 * Both iTerm and terminal seem to completely ignore bytes above 0x7E or below
 * 0x20 within CSI sequences.  They seem to just display to screen / be
 * interpreted as the C0 ESC sequences they are and then the parsing of the CSI
 * string continues uninterrupted.
 *
 * @input state must be set with .pos_byte pointing to the ESC that begins the
 *   CSI sequence
 * @return a state updated with the SGR sequence info and with pos_byte and
 *   other position info moved to the first char after the sequence.  See
 *   details for failure modes.
 */
struct FANSI_state FANSI_parse_esc(struct FANSI_state state) {
  /***************************************************\
  | IMPORTANT: KEEP THIS ALIGNED WITH FANSI_find_esc  |
  \***************************************************/
  if(state.string[state.pos_byte] != 27)
    // nocov start
    error(
      "Internal error: %s (decimal char %d).",
      "parsing ESC sequence that doesn't start with ESC",
      (int) state.string[state.pos_byte]
    );
    // nocov end

  int pos_byte_prev = state.pos_byte;
  state.pos_byte = FANSI_add_int(state.pos_byte, 1);  // advance ESC

  if(!state.string[state.pos_byte]) {
    // String ends in ESC
    state.err_code = 5;
  } else if(state.string[state.pos_byte] != '[') {
    // Other ESC sequence; note there are technically multi character sequences
    // but we ignore them here.  There is also the possibility that we mess up a
    // utf-8 sequence if it starts right after the ESC, but oh well...
    state.pos_byte = FANSI_add_int(state.pos_byte, 1);
    state.err_code = 5;
  } else {
    // CSI sequence

    state.pos_byte = FANSI_add_int(state.pos_byte, 1);  // consume '['
    struct FANSI_tok_res tok_res = {.err_code = 0};

    // Loop through the SGR; each token we process successfully modifies state
    // and advances to the next token

    do {
      tok_res = FANSI_parse_token(&state.string[state.pos_byte]);
      state.pos_byte =
        FANSI_add_int(state.pos_byte, tok_res.len);
      state.last = tok_res.last;
      state.err_code = tok_res.err_code;

      // Note we use `state.err_code` instead of `tok_res.err_code` as
      // FANSI_parse_colors internally calls FANSI_parse_token

      if(!state.err_code) {
        // We have a reasonable CSI value, now we need to check whether it
        // actually corresponds to anything that should modify state

        if(!tok_res.val) {
          state = FANSI_reset_state(state);
        } else if (tok_res.val < 10) {
          // 1-9 are the standard styles (bold/italic)
          // We use a bit mask on to track these
          state.style |= 1U << tok_res.val;
        } else if (tok_res.val < 20) {
          // These are alternative fonts
          state.font = tok_res.val;
        } else if (tok_res.val == 20) {
          // Fraktur
          state.style |= (1U << 10U);
        } else if (tok_res.val == 21) {
          // Double underline
          state.style |= (1U << 11U);
        } else if (tok_res.val == 22) {
          // Turn off bold or faint
          state.style &= ~(1U << 1U);
          state.style &= ~(1U << 2U);
        } else if (tok_res.val == 23) {
          // Turn off italics, fraktur
          state.style &= ~(1U << 3U);
          state.style &= ~(1U << 10U);
        } else if (tok_res.val == 24) {
          // Turn off underline, double underline
          state.style &= ~(1U << 4U);
          state.style &= ~(1U << 11U);
        } else if (tok_res.val == 25) {
          // Turn off blinking
          state.style &= ~(1U << 5U);
          state.style &= ~(1U << 6U);
        } else if (tok_res.val == 26) {
          // reserved for proportional spacing as specified in CCITT
          // Recommendation T.61; implicitly we are assuming this is a single
          // substring parameter, unlike say 38;2;..., but really we have no
          // idea what this is.
          state.style |= (1U << 12U);
        } else if (tok_res.val >= 20 && tok_res.val < 30) {
          // Turn off the other styles that map exactly from 1-9 to 21-29
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
        } else if(tok_res.val == 50) {
          // Turn off 26
          state.style &= ~(1U << 12U);
        } else if(tok_res.val > 50 & tok_res.val < 60) {
          // borders

          if(tok_res.val < 54) {
            state.border |= (1U << (unsigned int)(tok_res.val - 50));
          } else if (tok_res.val == 54) {
            state.border &= ~(1U << 1);
            state.border &= ~(1U << 2);
          } else if (tok_res.val == 55) {
            state.border &= ~(1U << 3);
          } else {
            state.err_code = 2;  // unknown token
          }
        } else {
          state.err_code = 2;  // unknown token
        }
      }
      // `tok_res` value can't be used because code above, including
      // FANSI_parse_colors can change the corresponding value in the `state`
      // struct, so better to deal with that directly

      if(state.last || state.err_code) break;
    } while(1);
  }
  int byte_offset = state.pos_byte - pos_byte_prev;

  if(state.err_code) {
    state.last_char_width = 0;
    warning(
      "Encountered invalid escape sequence with err code %d.", state.err_code
    );
    state.err_code = 0;
  } else {
    state.last_char_width = 1;
  }
  state.pos_ansi += byte_offset;
  return state;
}
/*
 * Read UTF8 character
 */
static struct FANSI_state FANSI_read_utf8(struct FANSI_state state) {
  int byte_size = FANSI_utf8clen(state.string[state.pos_byte]);

  // In order to compute char display width, we need to create a charsxp
  // with the sequence in question.  Hopefully not too much overhead since
  // at least we benefit from the global string hash table
  //
  // Note that we should probably not bother with computing this if display
  // mode is not width as it's probably expensive.

  SEXP str_chr =
    PROTECT(mkCharLenCE(state.string + state.pos_byte, byte_size, CE_UTF8));
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
  state.last_char_width = disp_size;
  state.pos_width += disp_size;
  state.pos_width_target += disp_size;
  state.has_utf8 = 1;
  return state;
}
/*
 * Read a Character Off when we know it is an ascii char, this is so we have a
 * consistent way of advancing state.
 */
struct FANSI_state FANSI_read_ascii(struct FANSI_state state) {
  ++state.pos_byte;
  ++state.pos_ansi;
  ++state.pos_raw;
  ++state.pos_width;
  ++state.pos_width_target;
  state.last_char_width = 1;
  return state;
}
/*
 * C0 ESC sequences treated as zero width
 */
struct FANSI_state FANSI_read_c0(struct FANSI_state state) {
  state = FANSI_read_ascii(state);
  --state.pos_width;
  --state.pos_width_target;
  return state;
}
/*
 * Read a Character Off and Update State
 *
 * This can probably use some pretty serious optimiation...
 */
struct FANSI_state FANSI_read_next(struct FANSI_state state) {
  const char chr_val = state.string[state.pos_byte];
  if(chr_val >= 0x20 && chr_val != 0x7f) {
    // Character is in the 1-126 range
    if(chr_val != 0x1b) {
      // Normal ASCII character
      state = FANSI_read_ascii(state);
    } else if (chr_val == 0x1b) {
      state = FANSI_parse_esc(state);
    }
  } else if(chr_val < 0) {
    state = FANSI_read_utf8(state);
  } else if(chr_val) {
    // These are the C0 ESC sequences
    state = FANSI_read_c0(state);
  }
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
 * This function is designed to be called iteratively on the same string with
 * monotonically increasing values of `pos`.  This allows us to compute state at
 * multiple positions while re-using the work we did on the earlier positions.
 * To transfer the state info from earlier positions we use a FANSI_state_pair
 * object that contains the state info from the previous compuation.
 *
 * @param pos the raw position (i.e. treating parseable ansi tags as zero
 *   length) we want the state for
 * @param state_pair two states (see description)
 * @param int type whether to use character (0), width (1), or byte (2) when
 *   computing the position
 * @param lag in cases where requested breakpoint is not feasible because of
 *   multi width characters, whether to return end of prior (0) or the start of
 *   the next (1)
 * @param end whether the request is made for the ends of the string (i.e. as
 *   part of the `stop` parameter) (1), or not (0) (i.e. as part of the start
 *   parameters).
 */
struct FANSI_state_pair FANSI_state_at_position(
    int pos, struct FANSI_state_pair state_pair, int type, int lag, int end
) {
  struct FANSI_state state = state_pair.cur;
  if(pos < state.pos_raw)
    // nocov start
    error(
      "Cannot re-use a state for a later position (%0f) than `pos` (%0f).",
      (double) state.pos_raw, (double) pos
    );
    // nocov end
  int cond = 0;

  // Need to reset pos_width_target since it could be distorted by a previous
  // middle of wide char event

  state.pos_width_target = state.pos_width;

  struct FANSI_state state_res, state_prev, state_prev_buff;

  state_prev = state_prev_buff = state_pair.prev;
  state_res = state;

  while(1) {
    state_prev = state_res = state;
    state.err_code = state.last = 0;

    // Handle UTF-8, we need to record the byte size of the sequence as well as
    // the corresponding display width.

    if(state.pos_byte == INT_MAX - 1)
      // nocov start
      // ... a bit tricky here, because we read ahead a few bytes in some
      // circumstances, but not all, so the furthest pos_byte should be allowed
      // to get actually varies
      error("Internal Error: counter overflow while reading string.");
      // nocov end

    state = FANSI_read_next(state);
    if(!state.string[state.pos_byte]) break;

    switch(type) {
      case 0: cond = pos - state.pos_raw; break;
      case 1: cond = pos - state.pos_width; break;
      case 2: cond = pos - state.pos_byte; break;
      default:
        // nocov start
        error("Internal Error: Illegal offset type; contact maintainer.");
        // nocov end
    }
    /*
    Rprintf(
      "cnd %2d x %2d lag %d end %d w (%2d %2d) ansi (%2d %2d) bt (%2d %2d)\n",
      cond, pos, lag, end,
      state.pos_width, state_prev.pos_width,
      state.pos_ansi, state_prev.pos_ansi,
      state.pos_byte, state_prev.pos_byte
    );
    */
    // We still have stuff to process

    if(cond > 0) continue;
    if(cond == 0) {
      // some ambiguity as to whether the next `state_prev` will be valid, soe
      // we store the current one just in case
      state_prev_buff = state_prev;
      continue;
    }
    /*
     * A key problem here is that what constitues a valid offset depends on the
     * display width of the character.  For example, -1 makes sense if the
     * character is 1 wide, or if we're looking to match that character to end.
     *
     * If instead we are matching the start of a wide character, then we're
     * looking for the overshoot to be the width of the character.
     */

    if(type == 1) { // width mode
      if(!lag) {
        if(end && cond != -1) {
          state_res = state_prev_buff;
        } else if (!end && cond != -state.last_char_width) {
          state_res = state;
      } }
      state_res.pos_width_target = pos;
    } else if(cond < -1) {
      // nocov start
      error(
        "%s%s",
        "Internal Error: partial width should only happen in type 'width'; ",
        "contact maintainer."
      );
      // nocov end
    }
    break;
  }
  // Keep advancing if there are any zero width UTF8 characters, not entirely
  // sure what we're supposed to do with prev_buff here, need to have some test
  // cases to see what happens.  Note we only do this when we end on zero width
  // chars

  if(end) {
    struct FANSI_state state_next, state_next_prev, state_next_prev_prev;
    state_next_prev_prev = state_res;
    state_next_prev = FANSI_read_next(state_next_prev_prev);
    state_next = FANSI_read_next(state_next_prev);

    /*
    Rprintf(
      "npp %d %d %d np %d %d %d n %d %d %d end %d\n",

      state_next_prev_prev.pos_byte,
      state_next_prev_prev.pos_width,
      state_next_prev_prev.pos_ansi,

      state_next_prev.pos_byte,
      state_next_prev.pos_width,
      state_next_prev.pos_ansi,

      state_next.pos_byte,
      state_next.pos_width,
      state_next.pos_ansi,

      state_next.string[state_next.pos_byte] == 0
    );
    */

    while(!state_next.last_char_width) {
      /*
      Rprintf(
        "next: width %d ansi %d last %d\n", state_next.pos_width,
        state_next.pos_ansi, state_next.last_char_width
      );
      */
      state_next_prev_prev = state_next_prev;
      state_next_prev = state_next;
      state_next = FANSI_read_next(state_next);
      if(!state_next.string[state_next.pos_byte]) break;
    }
    state_res = state_next_prev_prev;
  }
  // We return the state just before we overshot the end

  /*
  Rprintf(
    "   return pos %2d width %d ansi %2d byte %2d\n",
    pos, state_res.pos_width, state_res.pos_ansi, state_res.pos_byte
  );
  */
  return (struct FANSI_state_pair){.cur=state_res, .prev=state_prev_buff};
}
/*
 * We always include the size of the delimiter; could be a problem that this
 * isn't the actual size, but rather the maximum size (i.e. we always assume
 * three bytes even if the numbers don't get into three digits).
 */
int FANSI_color_size(int color, int * color_extra) {
  int size = 0;
  if(color == 8 && color_extra[0] == 2) {
    size = 3 + 2 +
      FANSI_digits_in_int(color_extra[1]) + 1 +
      FANSI_digits_in_int(color_extra[2]) + 1 +
      FANSI_digits_in_int(color_extra[3]) + 1;
  } else if (color == 8 && color_extra[0] == 5) {
    size = 3 + 2 +
      FANSI_digits_in_int(color_extra[1]) + 1;
  } else if (color == 8) {
    error("Internal Error: unexpected compound color format");   // nocov
  } else if (color >= 0 && color < 10) {
    size = 3;
  } else if (color >= 0) {
    error("Internal Error: unexpected compound color format 2"); // nocov
  }
  return size;
}
/*
 * Computes how many bytes we need to write out a state
 *
 * No overflow worries here b/c ints are 32bit+
 *
 * Includes the ESC[m size, but not the NULL terminator, so if you are writing
 * to a string that has nothing else in it remember to allocate an extra byte
 * for the NULL terminator.
 */
int FANSI_state_size(struct FANSI_state state) {
  int color_size = FANSI_color_size(state.color, state.color_extra);
  int bg_color_size = FANSI_color_size(state.bg_color, state.bg_color_extra);

  // styles are stored as bits

  int style_size = 0;
  for(int i = 1; i < 10; ++i){
    style_size += ((state.style & (1 << i)) > 0) * 2;
  }
  /*
  Rprintf(
      "%d %d %d %d %d %d %d %d %d\n",
    (state.style & 2) > 0, (state.style & 4) > 0, (state.style & 8) > 0,
    (state.style & 16) > 0, (state.style & 32) > 0, (state.style & 64) > 0,
    (state.style & 128) > 0, (state.style & 256) > 0, (state.style & 512) > 0);

  Rprintf(
    "  size - style: %d %d %d color: %d bg_color: %d\n", state.style,
    style_size, (state.style & 128) > 0, color_size, bg_color_size
  );
  */
  return color_size + bg_color_size + style_size + 2;  // +2 for ESC[
}
/*
 * Write extra color info to string
 *
 * Modifies string by reference.  This assumes
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
    error("Internal Error: color mode must be 3 or 4");  // nocov

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
          string + str_off,
          "2;%d;%d;%d;", color_extra[1], color_extra[2], color_extra[3]
        );
      } else if (color_extra[0] == 5) {
        write_chrs = sprintf(string + str_off, "5;%d;", color_extra[1]);
      } else error("Internal Error: unexpected color code.");

      if(write_chrs < 0) error("Internal Error: failed writing color code.");
      str_off += write_chrs;
    }
  }
  return str_off;
}
/*
 * We split this part out because in some cases we want to modify pre-existing
 * buffers
 *
 * Modifies the buffer by reference.
 *
 * DOES NOT ADD NULL TERMINATOR.
 *
 * return how many bytes were written
 */
int FANSI_csi_write(char * buff, struct FANSI_state state, int buff_len) {
  int str_pos = 0;
  buff[str_pos++] = 27;    // ESC
  buff[str_pos++] = '[';

  // styles

  for(unsigned int i = 1; i < 10; i++) {
    if((1U << i) & state.style) {
      buff[str_pos++] = '0' + i;
      buff[str_pos++] = ';';
  } }
  // colors

  str_pos += FANSI_color_write(
    &(buff[str_pos]), state.color, state.color_extra, 3
  );
  str_pos += FANSI_color_write(
    &(buff[str_pos]), state.bg_color, state.bg_color_extra, 4
  );
  // Finalize (note, in some cases we slightly overrallocate)

  if(str_pos > buff_len)
    // nocov start
    error(
      "Internal Error: tag mem allocation mismatch (%u, %u)", str_pos, buff_len
    );
    // nocov end
  buff[str_pos - 1] = 'm';
  return str_pos;
}
/*
 * Generate the ANSI tag corresponding to the state and write it out as a NULL
 * terminated string.
 */
char * FANSI_state_as_chr(struct FANSI_state state) {
  // First pass computes total size of tag; we need to account for the
  // separator as well

  int tag_len = FANSI_state_size(state);

  // Now allocate and generate tag

  char * tag_tmp = R_alloc(tag_len + 1, sizeof(char));
  int tag_len_written = FANSI_csi_write(tag_tmp, state, tag_len);
  if(tag_len_written > tag_len)
    error("Internal Error: CSI written larger than expected."); // nocov
  tag_tmp[tag_len_written] = 0;
  return tag_tmp;
}
/*
 * Determine whether two state structs have same style
 *
 * This only compares the style pieces (i.e. not the position pieces)
 *
 * Returns 1 if the are different, 0 if they are equal
 */
int FANSI_state_comp(struct FANSI_state target, struct FANSI_state current) {
  return !(
    target.style == current.style &&
    target.color == current.color &&
    target.border == current.border &&
    target.font == current.font &&
    target.ideogram == current.ideogram &&
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
int FANSI_state_has_style(struct FANSI_state state) {
  return
    state.style || state.color >= 0 || state.bg_color >= 0 ||
    state.font || state.border || state.ideogram;
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

  R_xlen_t len = XLENGTH(pos);

  const int res_cols = 4;  // if change this, need to change rownames init
  if(len > R_XLEN_T_MAX / res_cols) {
    error("Argument `pos` may be no longer than R_XLEN_T_MAX / %d", res_cols);
  }
  SEXP text_chr = asChar(text);
  const char * string = CHAR(text_chr);
  struct FANSI_state state = FANSI_state_init();
  struct FANSI_state state_prev = FANSI_state_init();

  struct FANSI_state_pair state_pair, state_pair_old;

  // Allocate result, will be a res_cols x n matrix.  A bit wasteful to record
  // all the color values given we'll rarely use them, but variable width
  // structures are likely to be much slower.  We could encode most color values
  // into one int but it would be a little annoying to retrieve them

  const char * rownames[4] = { // make sure lines up with res_cols
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

  int is_utf8_loc = FANSI_is_utf8_loc();
  string = FANSI_string_as_utf8(text_chr, is_utf8_loc).buff;

  state.string = state_prev.string = string;
  state_pair.cur = state;
  state_pair.prev = state_prev;

  // Compute state at each `pos` and record result in our results matrix

  int type_int = asInteger(type);
  int pos_i, pos_prev = -1;

  for(R_xlen_t i = 0; i < len; i++) {
    R_CheckUserInterrupt();
    pos_i = INTEGER(pos)[i];
    if(text_chr == NA_STRING || pos_i == NA_INTEGER) {
      for(R_xlen_t j = 0; j < res_cols; j++)
        INTEGER(res_mx)[i * res_cols + j] = NA_INTEGER;
    } else {
      if(pos_i < pos_prev)
        error("Internal Error: `pos` must be sorted %d %d.", pos_i, pos_prev);

      // We need to allow the same position multiple times in case it shows up
      // as starts and ends, etc.

      if(pos_i == pos_prev) {
        state_pair = state_pair_old;
      } else {
        state_pair_old = state_pair;
      }
      state_pair = FANSI_state_at_position(
        pos_i, state_pair, type_int, INTEGER(lag)[i], INTEGER(ends)[i]
      );
      state = state_pair.cur;

      // Record position, but set them back to 1 index

      INTEGER(res_mx)[i * res_cols + 0] = FANSI_add_int(state.pos_byte, 1);
      INTEGER(res_mx)[i * res_cols + 1] = FANSI_add_int(state.pos_raw, 1);
      INTEGER(res_mx)[i * res_cols + 2] = FANSI_add_int(state.pos_ansi, 1);
      INTEGER(res_mx)[i * res_cols + 3] =
        FANSI_add_int(state.pos_width_target, 1);

      // Record color tag if state changed

      if(FANSI_state_comp(state, state_prev)) {
        res_chr = PROTECT(mkChar(FANSI_state_as_chr(state)));
      } else {
        res_chr = PROTECT(res_chr_prev);
      }
      SET_STRING_ELT(res_str, i, res_chr);
      res_chr_prev = res_chr;
      UNPROTECT(1);  // note res_chr is protected by virtue of being in res_str
      pos_prev = pos_i;
    }
    state_prev = state;
  }
  SEXP res_list = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res_list, 0, res_str);
  SET_VECTOR_ELT(res_list, 1, res_mx);

  UNPROTECT(7);
  return(res_list);
}
