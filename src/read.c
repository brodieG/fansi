/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 *  This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
 * GENERAL NOTES ON read_ FUNCTIONS
 *
 * * state.pos_byte is taken to be the first unread character.
 * * state.pos_byte will be the first unread character after a call to `read_*`
 * * It is assumed that the string pointed to by a state cannot be longer than
 *   INT_MAX, so we do not check for overflow (this is checked on state init).
 * * Except for width calculations, which we'll have to decided whether we want
 *   to check overflow on or not since in UTF8 currently widest string is 2 wide
 *   and no single byte characters are more than 1 wide.
 */

// Can a byte be interpreted as ASCII number?

static int is_num(const char * string) {
  return *string >= 48 && *string <= 57;
}
// Convert a char value to number by subtracting the zero char; only intended
// for use with string values in [0-9]

static int as_num(const char * string) {
  if(!is_num(string))
    // nocov start
    error(
      "Internal Error: attempt to convert non-numeric char (%d) to int.",
      (int) *string
    );
    // nocov end
  return (int) (*string - '0');
}
/*
 * Reset all the display attributes, but not the position ones
 */
static struct FANSI_sgr reset_sgr(struct FANSI_sgr sgr) {
  sgr.style = 0;
  sgr.color = -1;
  for(int i = 0; i < 4; i++) sgr.color_extra[i] = 0;
  sgr.bg_color = -1;
  for(int i = 0; i < 4; i++) sgr.bg_color_extra[i] = 0;
  sgr.border = 0;
  sgr.ideogram = 0;
  sgr.font = 0;
  return  sgr;
}
// Store the result of reading a parameter substring token

struct FANSI_tok_res {
  unsigned int val;         // The actual value of the token
  int len;                  // How many character in the token
  int err_code;             // see struct FANSI_state
  int last;                 // Whether it is the last parameter substring
  int is_sgr;               // Whether sequence is known to be SGR
  int terminal;             // Whether sequence is last thing before term NULL
};
/*
 * Attempts to read CSI SGR tokens
 *
 * See struct FANSI_tok_res for return value details
 *
 * Note this makes no attempt to interpret the CSI other than indicate there are
 * odd characters in it.
 */

static struct FANSI_tok_res parse_token(const char * string) {
  unsigned int mult, val;
  int len, len_intermediate, len_tail, last, non_standard, private, err_code,
    leading_zeros, not_zero, is_sgr, terminal;
  len = len_intermediate = len_tail = val = last = non_standard = private =
    err_code = leading_zeros = not_zero = is_sgr = terminal = 0;
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
    ++string;
    ++len;
  }
  // check for for intermediate bytes, we allow 'infinite' here even though in
  // practice more than one is likely a bad outcome

  while(*string >= 0x20 && *string <= 0x2F) {
    ++string;
    ++len_intermediate;
  }
  // check for final byte

  terminal = is_sgr = 0;
  last = 1;
  if((*string == ';' || *string == 'm') && !len_intermediate) {
    // valid end of SGR parameter substring
    if(non_standard) err_code = 2;
    if(*string == ';') last = 0;
    // technically non-sgrness is implicit in last + err_code, but because we
    // can parse multiple CSI sequences one after the other, and we accumulate
    // the err_code value, it's cleaner to just explicitly determine whether
    // sequence is actually sgr.
    if(*string == 'm') is_sgr = 1;
    if(last && *(string + 1) == 0) terminal = 1;
  } else if(*string >= 0x40 && *string <= 0x7E && len_intermediate <= 1) {
    // valid final byte
    err_code = 4;
  } else {
    // invalid end, consume all subsequent parameter substrings
    while(*string >= 0x20 && *string <= 0x3F) {
      ++string;
      ++len_tail;
    }
    err_code = 5;
  }
  // Final interpretations; note that anything over 255 cannot be part of a
  // valid SGR sequence

  if(!err_code && (len - leading_zeros) > 3) {
    err_code = 1;
  }
  if(!err_code) {
    int len2 = len - leading_zeros;
    while(len2--) {
      val += (as_num(--string) * mult);
      mult *= 10;
  } }
  if(err_code < 3 && val > 255) err_code = 1;

  // If the string didn't end, then we consume one extra character for the
  // ending

  if(*string) ++len;

  return (struct FANSI_tok_res) {
    .val=val,
    .len=len + len_intermediate + len_tail,
    .err_code=err_code,
    .last=last,
    .is_sgr=is_sgr,
    .terminal=terminal
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
static struct FANSI_state parse_colors(
  struct FANSI_state state, int mode
) {
  if(mode != 3 && mode != 4)
    error("Internal Error: parsing color with invalid mode.");  // nocov

  struct FANSI_tok_res res;
  int rgb[4] = {0};
  int col = 8;
  int i_max;

  // First, figure out if we are in true color or palette mode

  res = parse_token(&state.string[state.pos_byte]);
  state.pos_byte += res.len;
  state.last = res.last;
  state.err_code = res.err_code;
  state.is_sgr = res.is_sgr;
  state.terminal = res.terminal;

  if(!state.err_code) {
    if((res.val != 2 && res.val != 5) || state.last) {
      // weird case, we don't want to advance the position here because
      // `res.val` needs to be interpreted as potentially a non-color style and
      // the prior 38 or 48 just gets tossed (at least this happens on OSX
      // terminal and iTerm)

      state.pos_byte -= (res.len);
      state.err_code = 1;
    } else if (
      // terminal doesn't have 256 or true color capability
      (res.val == 2 && !(state.term_cap & FANSI_TERM_TRUECOLOR)) ||
      (res.val == 5 && !(state.term_cap & FANSI_TERM_256))
    ) {
      // right now this is same as when not 2/5 following a 38, but maybe in the
      // future we want different treatment?
      state.pos_byte -= (res.len);
      state.err_code = 3;
    } else {
      int colors = res.val;
      if(colors == 2) {
        i_max = 3;
      } else if (colors == 5) {
        i_max = 1;
      } else error("Internal Error: 1301341"); // nocov

      rgb[0] = colors;

      // Parse through the subsequent tokens

      for(int i = 0; i < i_max; ++i) {
        res = parse_token(&state.string[state.pos_byte]);
        state.pos_byte += res.len;
        state.last = res.last;
        state.err_code = res.err_code;
        state.is_sgr = res.is_sgr;
        state.terminal = res.terminal;

        if(!state.err_code) {
          int early_end = res.last && i < (i_max - 1);
          if(res.val < 256 && !early_end) {
            rgb[i + 1] = res.val;
          } else {
            // nocov start
            error(
              "Internal Error: invalid color without err_code; ",
              "contact maintainer."
            );
            // nocov end
          }
        } else break;
      }
      // If there is an error code we do not change the color

      if(!state.err_code) {
        if(mode == 3) {
          state.sgr.color = col;
          for(int i = 0; i < 4; i++) state.sgr.color_extra[i] = rgb[i];
        } else if (mode == 4) {
          state.sgr.bg_color = col;
          for(int i = 0; i < 4; i++) state.sgr.bg_color_extra[i] = rgb[i];
        }
  } } }
  return state;
}
/*
 * Read a Character Off when we know it is an ascii char, this is so we have a
 * consistent way of advancing state.
 *
 * See GENERAL NOTES atop.
 */
static struct FANSI_state read_ascii(struct FANSI_state state) {
  ++state.pos_byte;
  ++state.pos_ansi;
  ++state.pos_raw;
  ++state.pos_width;
  ++state.pos_width_target;
  state.last_char_width = 1;
  return state;
}
/*
 * Parses ESC sequences
 *
 * See GENERAL NOTES atop.
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
static struct FANSI_state read_esc(struct FANSI_state state) {
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

  int err_code = 0;                       // track worst error code
  struct FANSI_sgr sgr_prev = state.sgr;

  // consume all contiguous ESC sequences; some complexity due to the addition
  // of the requirement that we only actually interpret the ESC sequences if
  // they are active via `ctl`, but the impossibility of knowing what type of
  // ESC sequence we're dealing with until we've parsed it.

  while(state.string[state.pos_byte] == 27) {
    struct FANSI_state state_prev = state;
    int esc_recognized = 0;

    ++state.pos_byte;  // advance ESC

    if(!state.string[state.pos_byte]) {
      // String ends in ESC
      state.err_code = 7;
      esc_recognized =
        state.ctl & (FANSI_CTL_ESC | FANSI_CTL_CSI | FANSI_CTL_SGR);
    } else if(
      state.string[state.pos_byte] != '[' && state.ctl & FANSI_CTL_ESC
    ) {
      esc_recognized = 1;

      // Other ESC sequence; note there are technically multi character
      // sequences but we ignore them here.  There is also the possibility that
      // we mess up a utf-8 sequence if it starts right after the ESC, but oh
      // well...

      if(
        state.string[state.pos_byte] >= 0x40 &&
        state.string[state.pos_byte] <= 0x7E
      )
        state.err_code = 6;
      else state.err_code = 7;

      // Don't process additional ESC if it is there so we keep looping

      if(state.string[state.pos_byte] != 27)
        ++state.pos_byte;
    } else if(
      state.ctl & (FANSI_CTL_CSI | FANSI_CTL_SGR)
    ) {
      // CSI sequence

      ++state.pos_byte;  // consume '['
      struct FANSI_tok_res tok_res = {.err_code = 0};

      // Loop through the SGR; each token we process successfully modifies state
      // and advances to the next token

      do {
        tok_res = parse_token(&state.string[state.pos_byte]);
        state.pos_byte += tok_res.len;
        state.last = tok_res.last;
        state.err_code = tok_res.err_code;
        state.is_sgr = tok_res.is_sgr;
        state.terminal = tok_res.terminal;

        // Note we use `state.err_code` instead of `tok_res.err_code` as
        // parse_colors internally calls parse_token

        if(!state.err_code) {
          // We have a reasonable CSI value, now we need to check whether it
          // actually corresponds to anything that should modify state
          //
          // Only parse_colors below will modify positions. Otherwise sgr and
          // error codes should be the only things changing.

          if(!tok_res.val) {
            state.sgr = reset_sgr(state.sgr);
          } else if (tok_res.val < 10) {
            // 1-9 are the standard styles (bold/italic)
            // We use a bit mask on to track these
            state.sgr.style |= 1U << tok_res.val;
          } else if (tok_res.val < 20) {
            // These are alternative fonts
            if(tok_res.val == 10) {
              state.sgr.font = 0;
            } else {
              state.sgr.font = tok_res.val;
            }
          } else if (tok_res.val == 20) {
            // Fraktur
            state.sgr.style |= (1U << 10U);
          } else if (tok_res.val == 21) {
            // Double underline
            state.sgr.style |= (1U << 11U);
          } else if (tok_res.val == 22) {
            // Turn off bold or faint
            state.sgr.style &= ~(1U << 1U);
            state.sgr.style &= ~(1U << 2U);
          } else if (tok_res.val == 23) {
            // Turn off italics, fraktur
            state.sgr.style &= ~(1U << 3U);
            state.sgr.style &= ~(1U << 10U);
          } else if (tok_res.val == 24) {
            // Turn off underline, double underline
            state.sgr.style &= ~(1U << 4U);
            state.sgr.style &= ~(1U << 11U);
          } else if (tok_res.val == 25) {
            // Turn off blinking
            state.sgr.style &= ~(1U << 5U);
            state.sgr.style &= ~(1U << 6U);
          } else if (tok_res.val == 26) {
            // reserved for proportional spacing as specified in CCITT
            // Recommendation T.61; implicitly we are assuming this is a single
            // substring parameter, unlike say 38;2;..., but really we have no
            // idea what this is.
            state.sgr.style |= (1U << 12U);
          } else if (tok_res.val >= 20 && tok_res.val < 30) {
            // Turn off the other styles that map exactly from 1-9 to 21-29
            state.sgr.style &= ~(1U << (tok_res.val - 20));
          } else if (tok_res.val >= 30 && tok_res.val < 50) {
            // Colors; much shared logic between color and bg_color, so
            // combining that here
            int foreground = tok_res.val < 40; // true then color, else bg color
            int col_code = tok_res.val - (foreground ? 30 : 40);

            if(col_code == 9) col_code = -1;
            // Handle the special color codes, need to parse some subsequent
            // tokens
            if(col_code == 8) {
              state = parse_colors(state, foreground ? 3 : 4);
            } else {
              // It's possible for col_code = 8 to not actually change color if
              // the parsing fails, so wait until end to set the color
              if(foreground) state.sgr.color = col_code;
              else state.sgr.bg_color = col_code;
            }
          } else if(
            (tok_res.val >= 90 && tok_res.val <= 97) ||
            (tok_res.val >= 100 && tok_res.val <= 107)
          ) {
            // Does terminal support bright colors? We do not consider it an
            // error if it doesn't.

            if(state.term_cap & 1) {
              if (tok_res.val < 100) {
                state.sgr.color = tok_res.val;
              } else {
                state.sgr.bg_color = tok_res.val;
            } }
          } else if(tok_res.val == 50) {
            // Turn off 26
            state.sgr.style &= ~(1U << 12U);
          } else if(tok_res.val > 50 && tok_res.val < 60) {
            // borders
            if(tok_res.val < 54) {
              state.sgr.border |= (1U << (unsigned int)(tok_res.val - 50));
            } else if (tok_res.val == 54) {
              state.sgr.border &= ~(1U << 1);
              state.sgr.border &= ~(1U << 2);
            } else if (tok_res.val == 55) {
              state.sgr.border &= ~(1U << 3);
            } else {
              state.err_code = 1;  // unknown token
            }
          } else if(tok_res.val >= 60 && tok_res.val < 70) {
            // ideograms
            if(tok_res.val < 65) {
              state.sgr.ideogram |= (1U << (unsigned int)(tok_res.val - 60));
            } else if (tok_res.val == 65) {
              state.sgr.ideogram = 0;
            } else {
              state.err_code = 1;  // unknown token
            }
          } else {
            state.err_code = 1;  // unknown token
          }
        }
        if(state.sgr.style > ((1 << (FANSI_STYLE_MAX + 1)) - 1))
          // nocov start
          error(
            "Internal Error: style greater than FANSI_STYLE_MAX; ",
            "contact maintainer."
          );
          // nocov end

        // `tok_res` value can't be used because code above, including
        // parse_colors can change the corresponding value in the `state`
        // struct, so better to deal with that directly
        if(state.err_code > err_code) err_code = state.err_code;
        if(state.last) break;
      } while(1);
      // Need to check that sequence actually is SGR, and if not, we need to
      // restore the state (this is done later on checking esc_recognized).
      if(!state.is_sgr) {
        // CSI
        if(state.ctl & FANSI_CTL_CSI) {
          state.sgr = state_prev.sgr;
          esc_recognized = 1;
        }
      } else if (state.ctl & FANSI_CTL_SGR) {
        // SGR and SGR tracking enabled
        esc_recognized = 1;
      }
    }
    // If the ESC was recognized then record error and advance, otherwise reset
    // the state and advance as if reading an ASCII character.
    if(esc_recognized) {
      if(state.err_code > err_code) err_code = state.err_code;
      int byte_offset = state.pos_byte - state_prev.pos_byte;
      state.pos_ansi += byte_offset;
    } else {
      state = state_prev;
      state = read_ascii(state);
    }
  }
  if(err_code) {
    // All errors are zero width; there should not be any errors if
    // !esc_recognized.
    state.err_code = err_code;  // b/c we want the worst err code
    state.last_char_width = 0;
    if(err_code == 3) {
      state.err_msg =
        "a CSI SGR sequence with color codes not supported by terminal";
    } else if(err_code < 4) {
      state.err_msg = "a CSI SGR sequence with unknown substrings";
    } else if (err_code == 4) {
      state.err_msg = "a non-SGR CSI sequence";
    } else if (err_code == 5) {
      state.err_msg = "a malformed CSI sequence";
    } else if (err_code == 6) {
      state.err_msg = "a non-CSI escape sequence";
    } else if (err_code == 7) {
      state.err_msg = "a malformed escape sequence";
    } else {
      // nocov start
      error("Internal Error: unknown ESC parse error; contact maintainer.");
      // nocov end
    }
  } else {
    // Not 100% sure this is right...
    state.last_char_width = 1;
    state.err_msg = "";
  }
  // Useful to know what prior style was in case this series of escapes ends up
  // being the last thing before terminal NULL.
  state.sgr_prev = sgr_prev;
  return state;
}
/*
 * Need to:
 *
 * * Count how many sub-elements there are.
 * * For each one, allocate two more bytes (ESC + [)
 *
 * * As we copy, seek from ESC to ESC.
 * * Write down the string.
 *
 * So, for write, need a function that accepts:
 *
 * * The input string starting at ESC.
 * * The output buffer ready to append to.
 * * Seeks and copies semi-colon to semi-colon until the N.
 */
static int normalize_state() {
  return 1;
}

/*
 * Read UTF8 character
 *
 * See GENERAL NOTES atop.
 */
static struct FANSI_state read_utf8(struct FANSI_state state) {
  int byte_size = FANSI_utf8clen(state.string[state.pos_byte]);

  // Make sure string doesn't end before UTF8 char supposedly does

  int mb_err = 0;
  int disp_size = 0;
  const char * mb_err_str =
    "use `is.na(nchar(x, allowNA=TRUE))` to find problem strings.";

  for(int i = 1; i < byte_size; ++i) {
    if(!state.string[state.pos_byte + i]) {
      mb_err = 1;
      byte_size = i;
      break;
  } }
  if(mb_err) {
    if(state.allowNA) {
      disp_size = NA_INTEGER;
    } else {
      // nocov start
      // shouldn't actually be possible to reach this point since in all use
      // cases we chose to allowNA, except for `nchar_ctl`, which internally
      // uses `nchar` so would never get here anyway
      error("invalid multiyte string, %s", mb_err_str);
      // nocov end
    }
  } else {
    // In order to compute char display width, we need to create a charsxp
    // with the sequence in question.  Hopefully not too much overhead since
    // at least we benefit from the global string hash table
    //
    // Note that we should probably not bother with computing this if display
    // mode is not width as it's probably expensive.

    if(state.use_nchar) {
      SEXP str_chr =
        PROTECT(mkCharLenCE(state.string + state.pos_byte, byte_size, CE_UTF8));
      disp_size = R_nchar(
        str_chr, Width, state.allowNA, state.keepNA, mb_err_str
      );
      UNPROTECT(1);
    } else {
      // This is not consistent with what we do with the padding where we use
      // byte_size, but in this case we know we're supposed to be dealing
      // with one char

      disp_size = 1;
    }
  }
  // Need to check overflow?  Really only for pos_width?  Maybe that's not
  // even true because you need at least two bytes to encode a double wide
  // character, and there is nothing wider than 2?

  state.pos_byte += byte_size;
  ++state.pos_ansi;
  ++state.pos_raw;
  if(disp_size == NA_INTEGER) {
    state.err_code = 9;
    state.err_msg = "a malformed UTF-8 sequence";
    state.nchar_err = 1;
    disp_size = byte_size;
  }
  state.last_char_width = disp_size;
  state.pos_width += disp_size;
  state.pos_width_target += disp_size;
  state.has_utf8 = 1;
  return state;
}
/*
 * C0 ESC sequences treated as zero width and do not count as characters either
 *
 * See GENERAL NOTES atop.
 */
static struct FANSI_state read_c0(struct FANSI_state state) {
  int is_nl = state.string[state.pos_byte] == '\n';
  if(!is_nl) {
    // question: should we make the comment about tabs as spaces?
    state.err_msg = "a C0 control character";
    state.err_code = 8;
  }
  state = read_ascii(state);
  // If C0/NL are being actively processed, treat them as width zero
  if(
    (is_nl && (state.ctl & FANSI_CTL_NL)) ||
    (!is_nl && (state.ctl & FANSI_CTL_C0))
  ) {
    --state.pos_raw;
    --state.pos_width;
    --state.pos_width_target;
  }
  return state;
}
/*
 * Read a Character Off and Update State
 *
 * See GENERAL NOTES atop.
 *
 * This can probably use some pretty serious optimization...
 */
struct FANSI_state FANSI_read_next(struct FANSI_state state) {
  const char chr_val = state.string[state.pos_byte];
  state.err_code = 0; // reset err code after each char
  // this can only be one if the last thing read is a CSI
  if(chr_val) state.terminal = 0;

  // Normal ASCII characters
  if(chr_val >= 0x20 && chr_val < 0x7F) state = read_ascii(state);
  // UTF8 characters (if chr_val is signed, then > 0x7f will be negative)
  else if (chr_val < 0 || chr_val > 0x7f) state = read_utf8(state);
  // ESC sequences
  else if (chr_val == 0x1B) state = read_esc(state);
  // C0 escapes (e.g. \t, \n, etc)
  else if(chr_val) state = read_c0(state);
  // Shouldn't happen, all code using read_next should bail. It's not a big
  // deal, but it makes terminal detection more complex if we allow it.

  if(state.warn > 0 && state.err_code) {
    warning(
      "Encountered %s, %s%s", state.err_msg,
      "see `?unhandled_ctl`; you can use `warn=FALSE` to turn ",
      "off these warnings."
    );
    state.warn = -state.warn; // only warn once
  }
  return state;
}
