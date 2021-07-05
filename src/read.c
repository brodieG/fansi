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
 * * read_* should never be used directly, use FANSI_read_next.
 * * state.pos_byte is taken to be the first unread character.
 * * state.pos_byte will be the first unread character after a call to `read_*`
 * * It is assumed that the string pointed to by a state cannot be longer than
 *   INT_MAX, so we do not check for overflow (this is checked on state init)
 *   except for width.
 *
 * Functions sometimes use the large `FANSI_state` object, but the hope is that
 * with them static the compilers will do nice things and the overhead will be
 * limited (but we have not checked).  It is a todo to see if this is a
 * performance bottleneck.
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
    leading_zeros, not_zero, is_sgr;
  len = len_intermediate = len_tail = val = last = non_standard = private =
    err_code = leading_zeros = not_zero = is_sgr = 0;
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

  is_sgr = 0;
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
// DANGER: param must be known to be no longer than INT_MAX.
//
// This is going to be quadratic on params * strlen(params) so bad idea to
// search for all params if there are many (there should not be).
//
// @params will be assumed to be colon delimited
// @param param must include trailing =, e.g. "id="

static struct FANSI_string get_url_param(
  struct FANSI_string params, const char * param
) {
  // We don't check any params longer than 128 chars
  struct FANSI_string res = {.val="", .len=0};
  int len = 0;
  while(len <= 128 && *(param + len)) ++len;
  if(*(param + len))
    error("Internal Error: max allowed param len 128 bytes.");
  if(*(param + len - 1) != '=')
    error("Internal Error: trailing param char must be '='.");

  const char * start = params.val;
  const char * end;
  if(len <= params.len) {
    while(*(start + len) && memcmp(start, param, len)) ++start;
    if(*(start + len)) {          // found match
      start = end = start + len;  // param value start
      // Previously checked that there will be at least a ';'
      while(*end && *end != ':' && *end != ';') ++end;
      res = (struct FANSI_string) {start, end - start};
    }
  }
  return res;
}
/*
 * Parse OSC Encoded URL as described by @egmontkob (Egmont Koblinger):
 *
 *   https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
 *
 * @egmontkob appears very involved in terminal emulation discussions and is
 * listed as an iterm2 contributor.  He frequently interacts with @gnachman
 * (iterm2 primary developer) on a seeming peer basis, and does seem to know
 * what he's talking about.  He appears to be a programmer with the Gnome
 * project.
 *
 * Nonetheless, this isn't much of a formal spec.  The description is mostly
 * about how iterm2 operates.
 *
 * Opening sequence is (spaces for clarity, not there in reality):
 *
 * OSC 8 ; params ; URL ST
 *
 * With OSC == ']' and params == 'key1=val1:key2=val2' with the only key that is
 * being discussed as in use at all being 'id'.  Notwithstanding iterm2 doesn't
 * seem to implement id (should connects multiple links into one).
 *
 * Technically there is no closing sequence, just an empty opening sequence acts
 * like a closing sequence (what about non-empty params but empty URL?).
 *
 * > For portability, the parameters and the URI must not contain any bytes
 * > outside of the 32–126 range. If they do, the behavior is undefined. Bytes
 * > outside of this range in the URI must be URI-encoded.
 * >
 * > Due to the syntax, additional parameter values cannot contain the : and ;
 * > characters either.
 *
 */

static struct FANSI_url parse_url(const char *x) {
  const char *end, *x0 = x;
  struct FANSI_url
    url = {
      .url={.val="", .len=0}, .params={.val="", .len=0}, .id={.val="", .len=0}
    };

  if(*x == ']' && *(x + 1) == '8' && *(x + 2) == ';') {
    end = x = x0 + 3;
    // Look for end of escape tracking position of first semi-colons (subsequent
    // ones may be part of the URI).
    int semicolon = 0;

    while(*end && *end != '\a' && !(*end == 0x1b && *(end + 1) == '\\')) {
      if(*end >= 0x20 && *end <= 0x7e) { // URLs allowed narrower set of bytes
        // All good
        if (*end == ';' && !semicolon) semicolon = end - x0;
      } else if (!(*end >= 0x08 && *end <= 0x0d)) {
        // Invalid string
        end = x = x0;
        break;
      }
      ++end;
    }
    // Ended sequence before string
    if(*end) {
      // If semicolon is found, and string is not invalid, it's a URL
      if(*end && semicolon) {
        const char * url_start = x0 + semicolon + 1;
        url.params = (struct FANSI_string) {x, (int) (url_start - x) - 1};
        url.url = (struct FANSI_string) {url_start, end - url_start};
        url.id = get_url_param(url.params, "id=");
      }
      // Record bytes (without initial ESC), even if no semicolon (plain OSC)
      if(end > x0) url.bytes = end - x0 + (*end == 0x1b) + 1;
    }
  }
  return url;
}
/*
 * Return OSC length excluding initial ESC (but including the '['), unless OSC
 * is invalid in which case return 0.
 *
 * OSC may be terminated with either BEL or ST (BEL is not ECMA48 standard, but
 * in common use for OSC based URL anchors).
 *
 * Support of non-ASCII inside OSC (i.e. 0x08-0x0d) does not work correctly on
 * OS X terminal where they are emitted, and it is required that the start be
 * num; and if not re-emitting starts at that point.
 *
 * see parse_url for the special case parsing for iterm2 spec OSC encoded URLs.
 *
 * Returns number of bytes in OSC excluding ESC
 */

static int parse_osc(const char * x) {
  const char * x0 = x;
  if(*x++ != ']') error("Internal Error: OSC must start with ']'."); // nocov
  while(*x){
    ++x;
    if(*x == '\a') {                            // valid end in BELL
      break;
    } else if(*x == 0x1b && *(x + 1) == '\\') { // valid end in ST
      ++x;
      break;
    } else if (!((*x >= 0x08 && *x <= 0x0d) || (*x >= 0x20 && *x <= 0x7e))) {
      x = x0;
      break;  // Invalid character
    }
    ++x;
  }
  // Input string can be at most INT_MAX long
  return (int)(x - x0);
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
  return state;
}
/*
 * Parses ESC sequences
 *
 * See GENERAL NOTES atop, and notes for each parse_ function.
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
 * @param state must be set with .pos_byte pointing to the ESC that begins the
 *   CSI sequence
 * @param seq 1 to read all abutting special escapes in one pass, 0 to read each
 *   one individually.
 * @return a state updated with the SGR sequence info and with pos_byte and
 *   other position info moved to the first char after the sequence.  See
 *   details for failure modes.
 */
static struct FANSI_state read_esc(struct FANSI_state state, int seq) {
  if(state.string[state.pos_byte] != 27)
    // nocov start
    error(
      "Internal error: %s (decimal char %d).",
      "parsing ESC sequence that doesn't start with ESC",
      (int) state.string[state.pos_byte]
    );
    // nocov end

  int err_code = 0;                       // track worst error code
  int seq_start = state.pos_byte;
  int non_normalized = 0;
  unsigned int esc_types = 0;          // 1 == normal, 2 == SGR
  struct FANSI_sgr sgr_prev = state.sgr;
  struct FANSI_url url_prev = state.url;

  // Consume all contiguous ESC sequences, subject to some conditions (see while
  // at the end)
  //
  // We only interpet sequences if they are active per .ctl, but we need to read
  // them in some cases to know what type they are to decide.

  do {
    struct FANSI_state state_prev = state;
    int esc_recognized = 0;

    ++state.pos_byte;  // advance ESC

    if(
      state.string[state.pos_byte] == '[' &&
      state.ctl & (FANSI_CTL_CSI | FANSI_CTL_SGR)
    ) {
      // - CSI -----------------------------------------------------------------

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

        // Note we use `state.err_code` instead of `tok_res.err_code` as
        // parse_colors internally calls parse_token

        if(!state.err_code) {
          // We have a reasonable CSI value, now we need to check whether it
          // actually corresponds to anything that should modify state
          //
          // Only parse_colors below will modify positions. Otherwise sgr and
          // error codes should be the only things changing.

          if(!tok_res.val) {
            non_normalized = 1;
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
            // error if it doesn't because the color will just be ignored
            // and won't cause a trainwreck like unsupported truecolor.

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
              state.sgr.border &= ~(1U << 1U);
              state.sgr.border &= ~(1U << 2U);
            } else if (tok_res.val == 55) {
              state.sgr.border &= ~(1U << 3U);
            } else {
              state.err_code = 1;  // unknown token
            }
          } else if(tok_res.val >= 60 && tok_res.val <= 65) {
            // ideograms
            if(tok_res.val < 65) {
              state.sgr.ideogram |= (1U << (unsigned int)(tok_res.val - 60));
            } else {
              state.sgr.ideogram = 0;
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
        non_normalized = 1;
      } while(1);
      // Need to check that sequence actually is SGR, and if not, we need to
      // restore the state (this is done later on checking esc_recognized).
      if(!state.is_sgr) {
        // CSI
        esc_types |= 1U;
        if(state.ctl & FANSI_CTL_CSI) {
          state.sgr = state_prev.sgr;
          esc_recognized = 1;
        }
      } else if (state.ctl & FANSI_CTL_SGR) {
        // SGR tracking enabled
        esc_recognized = 1;
        esc_types |= 2U;
      }
    } else if(
      state.string[state.pos_byte] == ']' &&
      state.ctl & (FANSI_CTL_OSC | FANSI_CTL_URL)
    ) {
      // - Operating System Command --------------------------------------------
      int osc_bytes = 0;
      if(
        state.string[state.pos_byte + 1] == '8' &&
        state.string[state.pos_byte + 2] == ';' &&
        (state.ctl & FANSI_CTL_URL)
      ) {
        // Possible URL
        struct FANSI_url url = parse_url(state.string + state.pos_byte);
        osc_bytes = url.bytes;
        if(url.bytes) state.url = url;    // success
        else if(osc_bytes) err_code = 4;  // malformed OSC URL
        else err_code = 5;                // Illegal OSC

        // Params other than id
        if(url.params.len && url.id.len + 3 != url.params.len)
          err_code = 2;

      } else if (state.ctl & FANSI_CTL_OSC) {
        // Other OSC
        osc_bytes = parse_osc(state.string + state.pos_byte);
        if(!osc_bytes) err_code = 5;      // Illegal OSC
        else err_code = 4;                // Not URL
      } else {
        // not URL and don't support OSC
        err_code = 4;                     // Not URL
      }
      if(osc_bytes) {
        esc_recognized = 1;
        esc_types |= 2U;
        state.pos_byte += osc_bytes;
      }
    } else if(!state.string[state.pos_byte]) {
      // - String ends in ESC --------------------------------------------------
      state.err_code = 7;
      esc_recognized = state.ctl & FANSI_CTL_ALL;
    } else if(state.ctl & FANSI_CTL_ESC) {
      // -Two Byte ESC ---------------------------------------------------------
      esc_types |= 1U;
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
    }
    // Did we read mixed special and non-special escapes?
    if(esc_types == (1U | 2U)) {
      state = state_prev;
      break;
    }
    // If the ESC was recognized then record error (if any) and advance,
    // otherwise reset the state and advance as if reading an ASCII character.
    if(esc_recognized) {
      if(state.err_code > err_code) err_code = state.err_code;
      int byte_offset = state.pos_byte - state_prev.pos_byte;
      state.pos_ansi += byte_offset;
      // Record other ancillary info here, we don't do it outside of the loop
      // as we want to be able reset to state_prev if things don't work out.
      state.sgr_prev = sgr_prev;
      state.url_prev = url_prev;
      state.non_normalized |= non_normalized;
      if(esc_types & 2U) {
        state.pos_byte_sgr_start = seq_start;
        state.last_special = 1;  // we  just read an SGR/URL, but maybe invalid
      }
    } else {
      state = state_prev;
      state = read_ascii(state);
    }
  } while(state.string[state.pos_byte] == 0x1b && seq);

  // CAREFUL adding changing values to `state` after here.  State could
  // be the unwound state `state_prev`.

  if(err_code) {
    // All errors are zero width; there should not be any errors if
    // !esc_recognized.
    // CARFUL: we rely on specific meaning of codes elsewhere, i.e. 5 && 7 are
    // genuine encoding errors, whereas the others are more warnings.  If we add
    // more error levels, we'll need to clean this up and e.g. have different
    // types of errors.
    state.err_code = err_code;  // b/c we want the worst err code
    if(err_code == 3) {
      state.err_msg =
        "a CSI SGR sequence with color codes not supported by terminal";
    } else if(err_code < 4) {
      state.err_msg =
        "a CSI SGR sequence with unknown substrings or a OSC URL sequence with unsupported parameters";
    } else if (err_code == 4) {
      state.err_msg = "a non-SGR CSI or a non-URL OSC sequence";
    } else if (err_code == 5) {
      state.err_msg = "a malformed CSI or OSC sequence";
    } else if (err_code == 6) {
      state.err_msg = "a non-CSI/OSC escape sequence";
    } else if (err_code == 7) {
      state.err_msg = "a malformed escape sequence";
    } else {
      // nocov start
      error("Internal Error: unknown ESC parse error; contact maintainer.");
      // nocov end
    }
  } else {
    state.last_special = 1;
    state.err_msg = "";
  }
  return state;
}
/*
 * Read UTF8 character
 *
 * See GENERAL NOTES atop.
 */
static struct FANSI_state read_utf8(struct FANSI_state state, R_xlen_t i) {
  int byte_size = FANSI_utf8clen(state.string[state.pos_byte]);

  // Make sure string doesn't end before UTF8 char supposedly does

  int mb_err = 0;
  int disp_size = 0;
  const char * mb_err_str =
    "use `validUTF8()` to find problem strings.";

  for(int i = 1; i < byte_size; ++i) {
    if(!state.string[state.pos_byte + i]) {
      mb_err = 1;
      byte_size = i;
      break;
  } }
  mb_err |= !FANSI_valid_utf8(state.string + state.pos_byte, byte_size);

  if(mb_err) {
    if(state.allowNA) {
      disp_size = NA_INTEGER;
    } else {
      // nocov start
      // shouldn't actually be possible to reach this point since in all use
      // cases we chose to allowNA, except for `nchar_ctl`, which internally
      // uses `nchar` so would never get here anyway
      error(
        "Invalid multiyte string and index [%jd], %s", FANSI_ind(i), mb_err_str
      );
      // nocov end
    }
  } else if(state.use_nchar) {  // only true if in width mode
    // Assumes valid UTF-8!
    int cp = FANSI_utf8_to_cp(state.string + state.pos_byte, byte_size);

    // Hacky grapheme approximation ensures flags (RI) aren't split, sets
    // skin modifiers to width zero (so greedy / not greedy searches will
    // / will not grab them), and sets width zero to anything following a ZWJ
    // (for the same reason).  This will work in many cases, provided that the
    // emoji sequences are valid and recognized by the display device.
    // Other graphemes work similarly to the extent continuation code points
    // are zero width naturally.  Prefixes, sequence interruptors,  and other
    // things will not work.

    if(cp >= 0x1F1E6 && cp <= 0x1F1FF) {         // Regional Indicator
      if(!state.last_ri) state.read_one_more = TRUE;
      disp_size = 1;    // read_next forces reading 2 RIs at a time
      state.last_ri = !state.last_ri;
    } else if (cp >= 0x1F3FB && cp <= 0x1F3FF) { // Skin type
      disp_size = 0;
    } else if (cp == 0x200D) {                   // Zero Width Joiner
      state.last_zwj = 1;
      disp_size = 0;
    } else {
      // In order to compute char display width, we need to create a charsxp
      // with the sequence in question.  Hopefully not too much overhead since
      // at least we benefit from the global string hash table
      SEXP str_chr =
        PROTECT(mkCharLenCE(state.string + state.pos_byte, byte_size, CE_UTF8));
      disp_size = R_nchar(
        str_chr, Width, // Width is an enum
        state.allowNA, state.keepNA, mb_err_str
      );
      UNPROTECT(1);
    }
  } else {
    // This is not consistent with what we do with the padding where we use
    // byte_size, but in this case we know we're supposed to be dealing
    // with one char

    disp_size = 1;
  }
  // We are guaranteed that strings here are at most INT_MAX bytes long, so
  // nothing should overflow, except maybe width if we ever add some width
  // measures that exceed byte count (like the 6 or 10 from '\u' or '\U' encoded
  // versions of Unicode chars).  Shouldn't be an issue, but playing it safe.
  if(disp_size == NA_INTEGER) {
    state.err_code = 9;
    state.err_msg = "a malformed UTF-8 sequence";
    state.nchar_err = 1;
    disp_size = byte_size = 1;
  }
  state.pos_byte += byte_size;
  ++state.pos_ansi;
  ++state.pos_raw;
  if(state.pos_width > FANSI_lim.lim_int.max - disp_size)
    error(
      "String with display width greater than INT_MAX at index [%jd].",
      FANSI_ind(i)
    );
  state.pos_width += disp_size;
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
struct FANSI_state FANSI_read_next(
  struct FANSI_state state, R_xlen_t i, int seq
) {
  char chr_val;

onemoretime:

  chr_val = state.string[state.pos_byte];
  struct FANSI_state state_prev = state;

  // reset flags
  state.last_zwj = state.is_sgr = state.err_code = state.read_one_more =
  state.last_special =  0;
  // this can only be one if the last thing read is a CSI

  int is_ascii = chr_val >= 0x20 && chr_val < 0x7F;
  int is_utf8 = chr_val < 0 || chr_val > 0x7f;

  // Normal ASCII characters
  if(is_ascii) state = read_ascii(state);
  // UTF8 characters (if chr_val is signed, then > 0x7f will be negative)
  else if (is_utf8) state = read_utf8(state, i);
  // ESC sequences
  else if (chr_val == 0x1B) state = read_esc(state, seq);
  // C0 escapes (e.g. \t, \n, etc)
  else if(chr_val) state = read_c0(state);

  if(!is_utf8) state.last_ri = 0;  // reset regional indicator

  if(state.warn > 0 && state.err_code) {
    warning(
      "Encountered %s at index [%jd], %s%s", state.err_msg, FANSI_ind(i),
      "see `?unhandled_ctl`; you can use `warn=FALSE` to turn ",
      "off these warnings."
    );
    state.warn = -state.warn; // only warn once
  }
  if(state_prev.last_zwj && state.use_nchar)
    state.pos_width = state_prev.pos_width;

  if(state.read_one_more && state.string[state.pos_byte])
    goto onemoretime;

  return state;
}
