/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 *  This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
 */

#include "fansi.h"

/*
 * GENERAL NOTES ON read_ FUNCTIONS
 *
 * * read_* should never be used directly, use FANSI_read_(next|until).
 * * state.pos.x will be the first unread character after a call to `read_*`
 * * It is assumed that the string pointed to by a state cannot be longer than
 *   INT_MAX, so we do not check for overflow (this is checked on state init)
 *   except for width.
 *
 * In many cases it is possible to bypass `FANSI_read_next` by e.g. incrementing
 * the `pos.x` offset.  Typically this is done when we don't care about thing
 * such as UTF-8 or widths.  In these cases, care should be taken to ensure none
 * of the subsequent uses of the state object rely on the specific states that
 * `FANSI_read_(next|until)` leaves it in.
 *
 * `FANSI_read_until` will be faster anytime we need to read a sequence of
 * characters, particularly if they are all e.g. "ASCII", as it avoid having to
 * jump in and out of the function requiring moving stuff in and out of memory.
 * `FANSI_read_next` is needed when we specifically just want to read one
 * "character" to check it.  It might be possible to replace `FANSI_read_next`
 * with a careful call to `FANSI_read_until`, but there are enough corner cases
 * that it was deemed easier to just have a different function (and IIRC there
 * might have been deal breaker, but I forget what).
 */

/*- Error Data ----------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

static const char * err_messages[] = {
  "a CSI SGR sequence with unknown substrings or a OSC hyperlink with unsupported parameters",
  "a CSI SGR sequence or OSC hyperlink with invalid substrings",
  "a CSI SGR sequence with color codes not supported by terminal",
  "a non-SGR CSI or a non-URL OSC sequence",
  "a non-SGR CSI or a non-URL OSC sequence with invalid substrings",
  "a malformed CSI or OSC sequence",  // early end
  "a non-CSI/OSC escape sequence",
  "a malformed escape sequence",
  "a C0 control character",
  "a malformed UTF-8 sequence",
  "an illegal non-ASCII byte"
};

/*- UTF8 Helpers --------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

/*
 * Code adapted from src/main/util.c@1186, this code is actually not completely
 * compliant, but we're just trying to match R behavior rather than the correct
 * UTF8 decoding.
 *
 * Among other things note that this allows 5-6 byte encodings which are no
 * longer valid.
 */

/* Number of additional bytes */

static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

static int utf8clen(const char * c, int * mb_err) {
  /* This allows through 8-bit chars 10xxxxxx, which are invalid */
  int res = 0;
  if ((*c & 0xc0) != 0xc0) res = 1;
  else res = 1 + utf8_table4[*c & 0x3f];

  // Make sure string doesn't end before UTF8 char supposedly does
  for(int i = 1; i < res; ++i) {
    if(!*(c + i)) {
      *mb_err = 1;
      res = i;
      break;
  } }
  return res;
}
/*
 * Perfunctory validation, checks there is a zero in the right spot
 * for the first byte, and that continuation bytes start with 10.
 *
 * Assumes correct number of continuation bytes exist and that
 * input was read through utf8clen.
 *
 * DO NOT USE AS STANDALONE UTF8 VALIDATION.
 */
static int valid_utf8(const char * chr, int bytes) {
  int pass = !(*chr & (0x20 >> (bytes - 2)));
  switch(bytes) {
    case 4: pass &= (*(++chr) & 0xc0) == 0x80;
    case 3: pass &= (*(++chr) & 0xc0) == 0x80;
    case 2: pass &= (*(++chr) & 0xc0) == 0x80; break;
    default: pass = 0;
  }
  return pass;
}

// Compute a unicode code point from a _valid_ UTF8 encoding
// Assumes 0 < bytes < 7 (or else bad stuff will happen)

static int utf8_to_cp(const char * chr, int bytes) {
  int cp = 0;
  // Lead byte (trailing bytes only contribute 6 bits each)
  cp |=
    (*chr & (0xff >> (bytes + (bytes > 1)))) // keep  7, 5, 4, or 3 bits
    << (bytes - 1) * 6;                      // shift by byte count

  // Trailing bytes keep trailing 6 bits
  for(int i = bytes - 1; i > 0; --i)
    cp |= (*(chr + bytes - i) & 0x3f) << (i - 1) * 6;

  return cp;
}
/*- Helpers -------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

static unsigned int set_err(unsigned int x, unsigned int err) {
  return FANSI_SET_RNG(x, FANSI_STAT_ERR_START, FANSI_STAT_ERR_ALL, err);
}
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

static void alert(struct FANSI_state * state, R_xlen_t i, const char * arg) {
  unsigned int err_code = FANSI_GET_ERR(state->status);
  int err_mode = (err_code == ERR_BAD_UTF8 || err_code == ERR_NON_ASCII);
  if(
    (
      !(state->status & FANSI_STAT_WARNED) ||
      err_mode // Bad error still happen even if warned already
    ) &&
    err_code &&
    (state->settings & (1U  << (FANSI_SET_WARN + err_code - 1U)))
  ) {
    // Select warn or error depending on severity
    void (*fun)(const char *, ...);
    if(err_mode) fun = error;
    else fun = warning;

    char argp[39];
    if(arg) {
      if(strlen(arg) > 18)
        error("Internal Error: arg name too long for warning.");// nocov
      int try = sprintf(argp, "Argument `%s` contains", arg);
      if(try < 0)
        error("Internal Error: snprintf failed.");  // nocov
    } else {
      strcpy(argp, "Encountered");
    }
    fun(
      "%s %s at index [%jd], %s%s",
      argp, err_messages[err_code - 1], FANSI_ind(i),
      "see `?unhandled_ctl`",
      err_mode ? "." : "; you can use `warn=FALSE` to turn off these warnings."
    );
    state->status |= FANSI_STAT_WARNED;  // only warn once
  }
}

/*- Parsers -------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

/*
 * Attempts to read CSI SGR tokens
 *
 * Returns token value.  `state` is advanced to the last byte of the sequence so
 * that calling code can check for e.g. ';' to know that there may be more
 * parameters to parse.
 */

unsigned int parse_token(struct FANSI_state * state) {
  unsigned int mult, val;
  int len, len_intermediate, len_tail, is_csi, non_standard, private,
    leading_zeros, not_zero, is_sgr, err_code;
  len = len_intermediate = len_tail = is_csi = non_standard = private =
    leading_zeros = not_zero = is_sgr = err_code = 0;
  mult = 1;
  val = 0U;

  const char * string = state->string + state->pos.x;

  // `private` a bit redundant since we don't actually use it differentially to
  // non-normal
  private = *string >= 0x3C && * string <= 0x3F;

  // cycle through valid parameter bytes [0-9:;<=>?]
  while(*string >= 0x30 && *string <= 0x3F && *string != ';') {
    int is_zero = *string == '0';
    if(!is_zero && !not_zero) not_zero = 1;
    if(is_zero && !not_zero) ++leading_zeros;
    non_standard |= *string > 0x39;
    ++string;
    ++len;
  }
  // check for for intermediate bytes, we allow 'infinite' here as per
  // ECMA48, although they note 1 byte is likely sufficient.
  while(*string >= 0x20 && *string <= 0x2F) {
    ++string;
    ++len_intermediate;
  }
  // check for final byte
  is_sgr = 0;
  is_csi = 1;  // Really means "is escape 'complete'"
  if((*string == ';' || *string == 'm') && !len_intermediate) {
    // valid end of SGR parameter substring
    if(non_standard) err_code = ERR_BAD_SUB;
    if(*string == ';') is_csi = 0;
    // technically non-sgrness is implicit in is_csi + err_code, but because we
    // can parse multiple CSI sequences one after the other, and we accumulate
    // the err_code value, it's cleaner to just explicitly determine whether
    // sequence is actually sgr.
  } else if(*string >= 0x40 && *string <= 0x7E) {
    // valid final byte, but not SGR
    err_code = ERR_NOT_SPECIAL;
  } else {
    // Invalid end, consume until find a valid end or end string
    while(*string && (*string < 0x40 || *string > 0x7E)) {
      if((unsigned char)*string > 0x7F) err_code = ERR_NON_ASCII;
      ++string;
      ++len_tail;
    }
    if(*string == 'm' && err_code < ERR_BAD_SUB)
      err_code = ERR_BAD_SUB;
    else if(*string && err_code < ERR_NOT_SPECIAL_BAD_SUB)
      err_code = ERR_NOT_SPECIAL_BAD_SUB;
    else if(!*string && err_code < ERR_BAD_CSI_OSC)
      err_code = ERR_BAD_CSI_OSC;
  }
  if(*string == 'm') is_sgr = 1;

  int bad_sub = err_code == ERR_BAD_SUB || err_code == ERR_NOT_SPECIAL_BAD_SUB;

  // Check num length to avoid overflow.
  if(len - leading_zeros > 3) bad_sub = 1;  // see val > 255 below
  if(!bad_sub && err_code < ERR_BAD_SUB) {
    int len2 = len - leading_zeros;
    while(len2--) {
      val += (as_num(--string) * mult);
      mult *= 10;
  } }
  // Anything over 255 cannot be part of a valid CSI (reference?).  It seems as
  // if the standards only specify it should be a decimal number, so this is not
  // strictly correct.
  if(val > 255) bad_sub = 1;

  if(bad_sub && !is_sgr && err_code < ERR_NOT_SPECIAL_BAD_SUB)
    err_code = ERR_NOT_SPECIAL_BAD_SUB;
  else if(bad_sub && err_code < ERR_BAD_SUB) err_code = ERR_BAD_SUB;

  state->pos.x += len + len_intermediate + len_tail;
  state->status = set_err(state->status, err_code);
  // csi/sgr mutually exclusive
  if(is_sgr) state->status |= FANSI_CTL_SGR;
  else if(is_csi) state->status |= FANSI_CTL_CSI;
  return val;
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
void parse_colors(
  struct FANSI_state * state, int mode
) {
  if(mode != 3 && mode != 4)
    error("Internal Error: parsing color with invalid mode.");  // nocov

  int tok_val = 0;
  int i_max;
  int prev_x = state->pos.x;

  // parse_token doesn't consume trailing, so need to inspect
  if(state->string[state->pos.x] == ';') {
    ++state->pos.x;
    // First, figure out if we are in true color or palette mode
    tok_val = parse_token(state);  // advances state by ref!
    unsigned int err_tmp = FANSI_GET_ERR(state->status);

    if(!err_tmp && state->string[state->pos.x] == ';')  {
      ++state->pos.x;
      if(
        (tok_val != 2 && tok_val != 5) || (state->status & FANSI_CTL_CSI)
      ) {
        // weird case, we don't want to advance the position here because
        // `tok_val` needs to be interpreted as potentially a non-color style
        // and the prior 38 or 48 just gets tossed (at least this happens on OSX
        // terminal and iTerm)
        state->pos.x = prev_x;
        state->status = set_err(state->status, ERR_UNKNOWN_SUB);
      } else if (
        // terminal doesn't have 256 or true color capability
        (tok_val == 2 && !(state->settings & FANSI_TERM_TRUECOLOR)) ||
        (tok_val == 5 && !(state->settings & FANSI_TERM_256))
      ) {
        // right now this is same as when not 2/5 following a 38, but maybe in
        // the future we want different treatment?
        state->pos.x = prev_x;
        state->status = set_err(state->status, ERR_EXCEED_CAP);
      } else {
        unsigned char tmp_col[3] = {0};
        int colors = tok_val;
        int err_col = 0;
        int early_end = 0;
        i_max = colors == 5 ? 1 : 3;
        // Parse through the subsequent tokens
        for(int i = 0; i < i_max; ++i) {
          tok_val = parse_token(state);  // advances state by ref!

          err_col = FANSI_GET_ERR(state->status);
          early_end = state->string[state->pos.x] != ';' && i < (i_max - 1);
          if(early_end && err_col < ERR_BAD_SUB) {
            state->status = set_err(state->status, ERR_BAD_SUB);
            err_col = ERR_BAD_SUB;
          }
          // this may overflow, but usigned so ok, + that's how Iterm2 seems to
          // interpret the value
          tmp_col[i] = tok_val;
          if(state->string[state->pos.x] && i < (i_max - 1) && !early_end)
            ++state->pos.x; // consume semi-colon except last one
          if(early_end) break;
        }
        // Only change a color if all sub-tokens parse correctly (arguably we
        // could allow e.g. 900 as a color channel value as Iterm2 does.
        if(!err_col && !early_end) {
          if(colors == 2) {
            if(mode == 3) state->fmt.sgr.color.x = FANSI_CLR_TRU | 8U;
            else          state->fmt.sgr.bgcol.x = FANSI_CLR_TRU | 8U;
            i_max = 3;
          } else if (colors == 5) {
            if(mode == 3) state->fmt.sgr.color.x = FANSI_CLR_256 | 8U;
            else          state->fmt.sgr.bgcol.x = FANSI_CLR_256 | 8U;
            i_max = 1;
          } else error("Internal Error: 1301341"); // nocov
          if(mode == 3)
            memcpy(state->fmt.sgr.color.extra, tmp_col, sizeof(tmp_col));
          else
            memcpy(state->fmt.sgr.bgcol.extra, tmp_col, sizeof(tmp_col));
        }
      }
    } else if(!err_tmp) state->status = set_err(state->status, ERR_BAD_SUB);
  } else state->status = set_err(state->status, ERR_BAD_SUB);
}

// DANGER: param must be known to be no longer than INT_MAX.
//
// This is going to be quadratic on params * strlen(params) so bad idea to
// search for all params if there are many (there should not be).
//
// @param will be assumed to be colon delimited
// @param param must include trailing =, e.g. "id="

static struct FANSI_offset get_url_param(
  const char * string, unsigned int start, unsigned int len, const char * param
) {
  // We don't check any params longer than 128 chars
  unsigned int plen = 0;
  struct FANSI_offset res = {0};
  while(plen <= 128 && *(param + plen)) ++plen;
  // Can't test the checks, we only ever use this with 'id=' ATM
  if(*(param + plen))
    error("Internal Error: max allowed param len 128 bytes.");  // nocov
  if(plen) {
    if(*(param + plen - 1) != '=')
      error("Internal Error: trailing param char must be '='.");  // nocov

    const char * target = string + start;
    const char * end;
    if(plen <= len) {
      while(
        *target &&
        (target - (string + start) < len - plen) &&
        memcmp(target, param, plen)
      )
        ++target; // could seek for delimiter for speed

      if(*target) {
        int o_pstart = target - string + plen;
        end = target;
        // Previously checked that there will be at least a ';'
        while(*end && *end != ':' && *end != ';') ++end;
        int o_pend = end - string;
        if(o_pstart < 0 || o_pend < 0)
          error("Internal Error: bad url param."); // nocov
        res = (struct FANSI_offset) {
          (unsigned int) o_pstart, (unsigned int) (o_pend - o_pstart)
  }; } } }
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
 * seem to implement id (should connects multiple links into one).  Not entirely
 * clear what we should do with the unsupported params.
 *
 * There is no real harm in carrying them, even though this is contrary to what
 * we do with SGR, because we just keep a pointer to the original string (this
 * should be okay because by the time we return to R this pointer is released,
 * or at least completely inaccessible so in the case of a GC there is no risk
 * of us trying to use it again).
 *
 * The reason we shouldn't do something similar with SGR is we have no idea of
 * the semantics of the tokens.  So we could keep piling on opening and closing
 * sequences of unknown tokens with no understanding of what they do, and as
 * soon as we want to try to understand them and hold on to them, the simple
 * pointer and offset to the original string that we use for OSC does not work.
 * So the key difference with SGR in particular is the non-cumulative nature of
 * OSC wrt to URL paramters (although this is not really clearly documented).
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
 * See also `parse_osc`.
 *
 * @return the length of the osc parsed
 */

#define ID_END(x) ((x).id.len + (x).id.start)
#define URL_END(x) ((x).url.len + (x).url.start)

unsigned int parse_url(struct FANSI_state * state) {
  const char *end, *x0, *x;
  unsigned int err_tmp = 0;
  unsigned int osc_len = 0;
  x0 = state->string;
  x = state->string + state->pos.x;
  if(*(x) == '8' && *(x + 1) == ';') {
  // All o_ variables are offsets relative to beginning of string
    unsigned int o_dat = state->pos.x + 2;
    end = x0 + o_dat;
    // Look for end of escape tracking position of first semi-colons (subsequent
    // ones may be part of the URI).
    unsigned int o_semic = 0;  // semicolon
    unsigned int o_bad = 0;

    while(*end && *end != '\a' && !(*end == 0x1b && *(end + 1) == '\\')) {
      // neither params nor URI must contain bytes outside of 0x20-0x7E. This is
      // a narrower range than stricly allowed by OSC CSI.
      if(*end >= 0x20 && *end <= 0x7e) {
        // All good
        if (*end == ';' && o_semic <= o_dat) o_semic = end - x0;
      } else if((unsigned char)*end > 0x7f) {
        err_tmp = ERR_NON_ASCII;
      } else if (!(*end >= 0x08 && *end <= 0x0d)) {
        // Invalid sub string (these used to be 5)
        if(err_tmp < ERR_BAD_SUB) err_tmp  = ERR_BAD_SUB;
        o_bad = end - x0;
      } else {
        // OK OSC, but non portable URL, used to be separate error from bad osc
        if(err_tmp < ERR_BAD_SUB) err_tmp  = ERR_BAD_SUB;
        o_bad = end - x0;  // really non-portable, but we don't distinguish
      }
      ++end;
    }
    // Ended sequence before string did
    if(*end) {
      // If o_semic is found, and string is not invalid, it's a URL
      if(*end && o_semic >= o_dat) {
        // const char * url_o_dat = x0 + o_semic + 1;
        // Only record params/id if we're sure they don't contain a bad byte
        state->fmt.url = (struct FANSI_url) {.string = state->string};
        struct FANSI_offset id_tmp;
        id_tmp = get_url_param(
          state->string, o_dat, o_semic - o_dat, "id="
        );
        if(id_tmp.start > o_bad) state->fmt.url.id = id_tmp;

        // Only record url if it has neither bad nor non-portable byte
        if(o_semic > o_bad) state->fmt.url.url =
          (struct FANSI_offset) {o_semic + 1, end - (x0 + o_semic + 1)};

        // Detect non id parameters
        if(
          // Params before id (+3 for "id=")
          (state->fmt.url.id.start > o_dat + 3) ||
          // Params after id
          (
            state->fmt.url.id.start &&
            o_semic > state->fmt.url.id.start + state->fmt.url.id.len
          ) ||
          // No id but other params
          (!state->fmt.url.id.start && o_semic > o_dat)
        ) {
          if(err_tmp < ERR_UNKNOWN_SUB) err_tmp = ERR_UNKNOWN_SUB;
        }
        // Sanity check
        if(
          (end - state->string) < URL_LEN(state->fmt.url) ||
          (end - state->string) < ID_LEN(state->fmt.url)
        )
          error("Internal Error: bad URI size.");  // nocov
      }
    } else err_tmp = ERR_BAD_CSI_OSC;

    const char * xdat = x0 + o_dat;
    if(end < xdat) error("Internal Error: bad url data detection\n");
    // Recall ESC] already consumed befor this function
    osc_len = end - xdat + 2 +   // 2 for opening "8;"
      (*end != 0) +              // consume terminator if there is one
      (*end == 0x1b);            // consume extra byte for ST

    // Even if sequence doesn't end, we declare an OSC encoded URL
    state->status |= FANSI_CTL_URL;
    state->pos.x += osc_len;
    state->status = set_err(state->status, err_tmp);
  } else error("Internal Error: non-URL OSC fed to URL parser.\n"); // nocov
  return osc_len;
}
/*
 * Return OSC length excluding initial "ESC]"
 *
 * OSC may be terminated with either BEL or ST (BEL is not ECMA48 standard, but
 * in common use for OSC based URL anchors).
 *
 * Support of C0 inside OSC (i.e. 0x08-0x0d) does not work correctly on
 * OS X terminal where they are emitted, and it is required that the start be
 * num; and if not re-emitting starts at that point.  iterm2 does it correctly.
 *
 * see parse_url for the special case parsing for iterm2 spec OSC encoded URLs.
 *
 * @param x pointer to first byte after "ESC]".
 * @return number of bytes in OSC excluding opening "ESC]", and error code if
 *   any
 */
static struct FANSI_osc parse_osc(const char * x) {
  const char * end = x;
  struct FANSI_osc osc = {.len=0, .error=0};
  while(*end && *end != '\a' && !(*end == 0x1b && *(end + 1) == '\\')) {
    if (!((*end >= 0x08 && *end <= 0x0d) || (*end >= 0x20 && *end <= 0x7e))) {
      if((unsigned char)*end > 0x7f) {
        osc.error = ERR_NON_ASCII;
      }
      else if(osc.error < ERR_NOT_SPECIAL_BAD_SUB)
        osc.error = ERR_NOT_SPECIAL_BAD_SUB;
    }
    ++end;
  }
  // Unterminated
  if(!*end && osc.error < ERR_BAD_CSI_OSC) osc.error = ERR_BAD_CSI_OSC;

  osc.len = end - x +
    (*end != 0) +           // consume terminator if there is one
    (*end == 0x1b);         // consume extra byte for ST
  return osc;
}
/*- Read Interfaces -----------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
 * @param reset whether to reset status before proceeding
 */
static void read_ascii_until(
  struct FANSI_state * state, int until, int reset
) {
  if(reset) state->status = state->status & FANSI_STAT_WARNED;
  int until2 = until - state->pos.w;
  const char * start, * end;
  end = start = state->string + state->pos.x;
  while(IS_PRINT(*end) && (end - start) < until2) ++end;
  int bytes = end - start;
  state->pos.w += bytes;
  state->pos.x += bytes;
}

/*
 * Read a Character Off when we know it is an ascii char, this is so we have a
 * consistent way of advancing state.
 *
 * See GENERAL NOTES atop.
 */
static void read_one(struct FANSI_state * state) {
  ++state->pos.x;
  ++state->pos.w;
}
/*
 * Parses ESC sequences
 *
 * See GENERAL NOTES atop, and notes for each parse_ function.
 *
 * Based on ECMA-48 (see extra/Ecma-048.pdf) and CCITT Recommendation T.416 (the
 * latter cannot be conveyed for copyright reasons, but is published online in
 * draft form).
 *
 * In particular, special treatment for ANSI CSI SGR and OSC URL sequences.
 * Reading is greedy, where sequences will continue to be read until a valid
 * terminator is encountered (or target length achieved), even if there are
 * illegal bytes in the interim.  This allows bad sequences to be stripped.
 * Only correct (or close enough) sequences will result in `state.status`
 * gaining one of the `FANSI_CTL_*` flags.
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
 * * Intermediate bytes 02/00-02/15 (0x20-0x2F): [ !"#$%&'()*+,\-./]
 * * Final bytes 04/00-07/14 (0x40-0x7E): [@A-Z\[\\\]\%_`a-z{|}~]
 *
 * Spec appears to allow any number of decimal digits, but we cap to 3 in
 * [0,255] as integers.
 *
 * @section Possible Outcomes:
 *
 * See `FANSI_STAT_ERR_MASK`.
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
 * @param state must be set with .pos.x pointing to the ESC that begins the
 *   CSI sequence
 * @param term_i whether we're in terminate mode, in which case we will not
 *   read trailing special controls.
 *
 */
void read_esc(struct FANSI_state * state, int term_i) {
  if(state->string[state->pos.x] != 27)
    // nocov start
    error(
      "Internal error: %s (decimal char %d).",
      "parsing ESC sequence that doesn't start with ESC",
      (int) state->string[state->pos.x]
    );
    // nocov end

  unsigned int esc_types = 0;          // 1 == CSI/OSC, 2 == SGR/URL
  // err_code is a bit annoying; we keep resetting the state->status error with
  // each token, so we need err_code to keep track of the overall error, but
  // then we need to store it back in ->status before exit.
  unsigned int err_code = 0;

  state->status &= ~FANSI_CTL_MASK;
  unsigned int sgr_sup = state->settings & FANSI_CTL_SGR;
  unsigned int csi_sup = state->settings & FANSI_CTL_CSI;

  // Consume all contiguous ESC sequences, subject to some conditions (see while
  // at the end).  We only interpet sequences if they are active per .settings,
  // but we need to read them in some cases to know what type they are to
  // decide.

  do {
    struct FANSI_state state_prev = *state;
    // Is the current escape recognized by the `ctl` parameter?  It doesn't
    // matter if it ends up being badly encoded or, not just that it starts out
    // as a presumptive escape that we requested to recognize and such should
    // attempt to parse and advance as far as we're able to parse.
    int esc_recognized = 1;
    ++state->pos.x;  // advance ESC

    // Reset status except warned as we could be reading multiple CSIs or OSCs
    // in a row.  Error is preserved in err_code
    state->status = state->status & FANSI_STAT_WARNED;

    if(state->string[state->pos.x] == '[' && (sgr_sup || csi_sup)) {
      // - CSI -----------------------------------------------------------------
      ++state->pos.x;  // consume '['
      int tok_val = 0;

      // Make sure ->status only contains one CTL per ESC sequence.
      unsigned int ctl_prev = state->status & FANSI_CTL_MASK;
      state->status &= ~FANSI_CTL_MASK;

      // Loop through the SGR; each token we process successfully modifies state
      // and advances to the next token.
      do {
        state->status = set_err(state->status, 0);
        tok_val = parse_token(state);  // advances state by ref!
        if(!FANSI_GET_ERR(state->status)) {
          // We have a reasonable CSI substring, now we need to check whether it
          // actually corresponds to anything that should modify state
          //
          // Only parse_colors below will modify positions. Otherwise sgr and
          // error codes should be the only things changing.

          if(!tok_val) state->fmt.sgr = (struct FANSI_sgr) {0};
          // - Colors ----------------------------------------------------------
          else if (tok_val == 39) state->fmt.sgr.color.x = 0;
          else if (tok_val == 49) state->fmt.sgr.bgcol.x = 0;
          else if (tok_val == 38 || tok_val == 48)
            // parse_colors internally calls parse_token (advances state by ref)
            parse_colors(state, tok_val < 40 ? 3 : 4);
          else if (tok_val >=  30 && tok_val <  48) {
            int fg = tok_val < 40;
            unsigned int col_code = tok_val - (fg ? 30 : 40);
            unsigned int col_enc = FANSI_CLR_8 | col_code;
            if(fg) state->fmt.sgr.color.x = col_enc;
            else   state->fmt.sgr.bgcol.x = col_enc;
          } else if (
            (tok_val >=  90 && tok_val <=  97) ||
            (tok_val >= 100 && tok_val <= 107)
          ) {
            // Does terminal support bright colors? We do not consider it an
            // error if it doesn't because the color will just be ignored
            // and won't cause a trainwreck like unsupported truecolor.
            // However, we do not write it because if the terminal doesn't
            // support it presumably it doesn't change the color at all.
            if(state->settings & FANSI_TERM_BRIGHT) {
              int fg = tok_val < 100;
              unsigned int col_code = tok_val - (fg ? 90 : 100);
              unsigned int col_enc = FANSI_CLR_BRIGHT | col_code;
              if(fg) state->fmt.sgr.color.x = col_enc;
              else   state->fmt.sgr.bgcol.x = col_enc;
            }
          // - Styles On -------------------------------------------------------
          } else if (tok_val < 10) {
            // 1-9 are the standard styles (bold/italic)
            // We use a bit mask on to track these
            state->fmt.sgr.style |= 1U << (tok_val - 1U);
          } else if (tok_val < 20) {
            // These are alternative fonts (10 is reset)
            state->fmt.sgr.style &= ~FANSI_FONT_MASK;
            if(tok_val > 10) state->fmt.sgr.style |= tok_val << FANSI_FONT_START;
          } else if (tok_val == 20) {
            // Fraktur
            state->fmt.sgr.style |= FANSI_STL_FRAKTUR;
          } else if (tok_val == 21) {
            // Double underline
            state->fmt.sgr.style |= FANSI_STL_UNDER2;
          } else if (tok_val == 26) {
            // reserved for proportional spacing as specified in CCITT
            // Recommendation T.61; implicitly we are assuming this is a single
            // substring parameter, unlike say 38;2;..., but really we have no
            // idea what this is.
            state->fmt.sgr.style |= FANSI_STL_PROPSPC;

          // - Styles Off ------------------------------------------------------
          } else if (tok_val == 22) {
            state->fmt.sgr.style &= ~FANSI_STL_BOLD;
            state->fmt.sgr.style &= ~FANSI_STL_BLUR;
          } else if (tok_val == 23) {
            state->fmt.sgr.style &= ~FANSI_STL_ITALIC;
            state->fmt.sgr.style &= ~FANSI_STL_FRAKTUR;
          } else if (tok_val == 24) {
            state->fmt.sgr.style &= ~FANSI_STL_UNDER;
            state->fmt.sgr.style &= ~FANSI_STL_UNDER2;
          } else if (tok_val == 25) {
            state->fmt.sgr.style &= ~FANSI_STL_BLINK1;
            state->fmt.sgr.style &= ~FANSI_STL_BLINK2;
          }
          else if (tok_val == 27)
            state->fmt.sgr.style &= ~FANSI_STL_INVERT;
          else if (tok_val == 28)
            state->fmt.sgr.style &= ~FANSI_STL_CONCEAL;
          else if (tok_val == 29)
            state->fmt.sgr.style &= ~FANSI_STL_CROSSOUT;
          else if(tok_val == 50)
            state->fmt.sgr.style &= ~FANSI_STL_PROPSPC;

          // - Borders / Ideograms ---------------------------------------------
          else if(tok_val > 50 && tok_val < 60) {
            switch(tok_val) {
              case 51: state->fmt.sgr.style |= FANSI_BRD_FRAMED; break;
              case 52: state->fmt.sgr.style |= FANSI_BRD_ENCIRC; break;
              case 53: state->fmt.sgr.style |= FANSI_BRD_OVERLN; break;
              case 54:
                state->fmt.sgr.style &= ~(FANSI_BRD_FRAMED | FANSI_BRD_ENCIRC);
                break;
              case 55:
                state->fmt.sgr.style &= ~FANSI_BRD_OVERLN;
                break;
              default:
                state->status = set_err(state->status, ERR_UNKNOWN_SUB);
            }
          } else if(tok_val >= 60 && tok_val <= 65) {
            switch(tok_val) {
              case 60: state->fmt.sgr.style |= FANSI_IDG_UNDERL;  break;
              case 61: state->fmt.sgr.style |= FANSI_IDG_UNDERL2; break;
              case 62: state->fmt.sgr.style |= FANSI_IDG_OVERL;   break;
              case 63: state->fmt.sgr.style |= FANSI_IDG_OVERL2;  break;
              case 64: state->fmt.sgr.style |= FANSI_IDG_STRESS;  break;
              default: // ony 65
                state->fmt.sgr.style &= ~FANSI_IDG_MASK;
            }
          } else {
            state->status = set_err(state->status, ERR_UNKNOWN_SUB);
          }
        }
        if(FANSI_GET_ERR(state->status) > err_code)
          err_code = FANSI_GET_ERR(state->status);
        if(state->string[state->pos.x] != ';') break;
        ++state->pos.x;
      } while(1);
      // Consume closing char (parse_token already checked it is correct
      if(state->string[state->pos.x]) ++state->pos.x;

      // We have no way of knowning whether something could be SGR or other CSI
      // until we read the whole sequence.  If we do not support SGRs, the
      // behavior should be to just treat it as a two char ESC.  If we do
      // support it but it is bad, we consume as much of it as we can and treat
      // it as CSI.
      //
      // CSI and SGR status are exclusive (i.e. you get one xor the other from
      // parse_token)

      unsigned int sgr, csi;
      sgr = state->status & FANSI_CTL_SGR;
      csi = state->status & FANSI_CTL_CSI;
      if(sgr && sgr_sup) esc_types |= 2U;
      else if(csi && csi_sup) esc_types |= 1U;
      else if((sgr && !sgr_sup) || (csi && !csi_sup)) {
        // Good sequence, but don't support it, so just read 1 char from prev
        *state = state_prev;
        state->status &= FANSI_STAT_WARNED;
        read_one(state);  // can't use read_ascii_until as this is not ascii
      }
      else error("Internal Error: unexpected CSI/SGR status."); // nocov

      // If we're reading multiple ESCs in series, we could have more than one
      // of CSI/SGR in status.
      state->status |= ctl_prev;
    } else if(
      state->string[state->pos.x] == ']' &&
      state->string[state->pos.x + 1] == '8' &&
      state->string[state->pos.x + 2] == ';' &&
      state->settings & FANSI_CTL_URL
    ) {
      // - OSC Encoded URL -----------------------------------------------------
      ++state->pos.x;    // consume ']'
      parse_url(state);  // advances state by ref!
      esc_types |= 2U;
    } else if(
      state->string[state->pos.x] == ']' &&
      state->settings & FANSI_CTL_OSC
    ) {
      // - Other OSC System Command --------------------------------------------
      int osc_bytes = 0;
      ++state->pos.x;  // consume ']'
      struct FANSI_osc osc = parse_osc(state->string + state->pos.x);
      osc_bytes = osc.len;
      err_code = osc.error;
      state->status |= FANSI_CTL_OSC;
      esc_types |= 1U;
      state->pos.x += osc_bytes;
    } else if(
      !state->string[state->pos.x] && (state->settings & FANSI_CTL_ESC_CTL)
    ) {
      // - String ends in ESC --------------------------------------------------

      // Assume 2-byte ESC, although could be any other hence _CTL_MASK
      state->status |= FANSI_CTL_ESC;
      esc_types |= 1U;
      err_code = ERR_ESC_OTHER_BAD;
    } else if(state->settings & FANSI_CTL_ESC) {
      // -Two Byte ESC ---------------------------------------------------------
      esc_types |= 1U;
      state->status |= FANSI_CTL_ESC;

      // Other ESC sequence; note there are technically multi character
      // sequences but we ignore them here.  There is also the possibility that
      // we mess up a utf-8 sequence if it starts right after the ESC, but oh
      // well...
      if(
        state->string[state->pos.x] >= 0x40 &&
        state->string[state->pos.x] <= 0x7E
      )
        err_code = ERR_ESC_OTHER;
      else if((unsigned char)state->string[state->pos.x] > 0x7F)
        err_code = ERR_NON_ASCII;
      else
        err_code = ERR_ESC_OTHER_BAD;

      // Process additional character, even if bad ESC, for consistency with
      // greedy byte consumption.
      if(state->string[state->pos.x]) ++state->pos.x;
    } else {
      esc_types |= 1U;
      esc_recognized = 0;
    }
    // Did we read mixed special and non-special escapes?
    if(esc_types == (1U | 2U)) {
      *state = state_prev;
      break;
    }
    if(FANSI_GET_ERR(state->status) > err_code)
      err_code = FANSI_GET_ERR(state->status);

    // If the ESC was recognized then record error (if any) and advance,
    // otherwise reset the state and advance as if reading an ASCII character.
    if(esc_recognized) {
      if(
        esc_types == 2U && (
          err_code <= ERR_EXCEED_CAP ||
          // URLs always special since we know from begining what they are
          (state->status & FANSI_CTL_URL)
      ) ) {
        if(term_i && !state->string[state->pos.x]) {
          *state = state_prev;
          state->status |= FANSI_STAT_DONE;
          break;
        } else state->status |= FANSI_STAT_SPECIAL;
      }
    } else {
      *state = state_prev;
      read_one(state);   // read one byte, can's use read_ascii_until
      break;
    }
  } while(
    state->string[state->pos.x] == 0x1b && !(state->settings & FANSI_SET_ESCONE)
  );
  if(err_code > FANSI_GET_ERR(state->status))
    state->status = set_err(state->status, err_code);
}
/*
 * C0 ESC sequences treated as zero width and do not count as characters either
 *
 * See GENERAL NOTES atop.
 */
void read_c0(struct FANSI_state * state) {
  int is_nl = state->string[state->pos.x] == '\n';
  state->status &= FANSI_STAT_WARNED;
  if(!is_nl) state->status = set_err(state->status, ERR_C0);
  read_one(state);
  // If C0/NL are being actively processed, treat them as width zero
  if(
    (is_nl && (state->settings & FANSI_CTL_NL)) ||
    (!is_nl && (state->settings & FANSI_CTL_C0))
  ) {
    --state->pos.w;
    state->status |= is_nl ? FANSI_CTL_NL : FANSI_CTL_C0;
  }
}

/*
 * Read a Series of UTF8 (and ASCII) Characters
 *
 * See GENERAL NOTES atop.
 *
 * Hacky grapheme approximation ensures flags (RI) aren't split, sets skin
 * modifiers to width zero (so greedy / not greedy searches will / will  not
 * grab them), and sets width zero to anything following a ZWJ (for  the same
 * reason).  This will work in many cases, provided that the emoji sequences
 * are valid and recognized by the display device.  Other graphemes work
 * similarly to the extent continuation code points are  zero width naturally.
 * Prefixes, sequence interruptors, and other things will not work.
 *
 * @param until width target
 * @param overshoot allow overshooting of target width, if set to 0 it is
 *   critical that calling code check for the _DONE flag.  Must be 1 or 0
 */
void read_utf8_until(struct FANSI_state * state, int until, int overshoot) {
  char x;
  unsigned int w_mode =
    FANSI_GET_RNG(state->settings, FANSI_SET_WIDTH, FANSI_COUNT_ALL);
  unsigned int cur_ri = state->status & FANSI_STAT_RI;
  unsigned int cur_zwj = state->status & FANSI_STAT_ZWJ;
  state->status = state->status & FANSI_STAT_WARNED;

  while((x = state->string[state->pos.x]) && IS_UTF8(x)) {
    // These status are tracked in state->  because they must be retained
    // across escape sequences.
    unsigned int prev_zwj = cur_zwj;
    unsigned int prev_ri = cur_ri;
    cur_zwj = cur_ri = 0;
    // Reset prior state info

    int mb_err = 0;
    int disp_size = 0;
    const char * mb_err_str = "use `validUTF8()` to find problem strings.";

    int byte_size = utf8clen(state->string + state->pos.x, &mb_err);
    mb_err |= !valid_utf8(state->string + state->pos.x, byte_size);

    if(mb_err) {
      // mimic what R_nchar does on mb error.  This will also break the loop
      // later.
      disp_size = NA_INTEGER;
    } else if(w_mode == FANSI_COUNT_WIDTH || w_mode == FANSI_COUNT_GRAPH) {
      // Assumes valid UTF-8!  Should have been checked.
      int cp = utf8_to_cp(state->string + state->pos.x, byte_size);

      // Probably shouldn't set RI or ZWJ until we're sure we're okay, but we
      // do.  It works, but likely only b/c of bailouts and resets elsewhere
      if(cp >= 0x1F1E6 && cp <= 0x1F1FF) {    // Regional Indicator
        // First RI is width two, next zero
        if(!(prev_ri)) {
          cur_ri |= FANSI_STAT_RI;
          disp_size = 2;
        } else {
          disp_size = 0;
        }
        // we rely on external logic to forece reading two RIs
      } else {
        if (cp >= 0x1F3FB && cp <= 0x1F3FF) { // Skin type
          disp_size = 0;
        } else if (cp == 0x200D) {            // Zero Width Joiner
          cur_zwj |= FANSI_STAT_ZWJ;
          disp_size = 0;
        } else if (prev_zwj) {
          disp_size = 0;
        } else {
          // Need CHARSXP for `R_nchar`, hopefull not too expensive.
          SEXP str_chr = PROTECT(
            mkCharLenCE(state->string + state->pos.x, byte_size, CE_UTF8)
          );
          disp_size = R_nchar(
            str_chr, Width, // Width is an enum
            state->status & FANSI_SET_ALLOWNA,
            state->status & FANSI_SET_KEEPNA,
            mb_err_str
          );
          UNPROTECT(1);
      } }
      if(w_mode == FANSI_COUNT_GRAPH && disp_size > 1) disp_size = 1;
    } else if(w_mode == FANSI_COUNT_BYTES) {
      disp_size = byte_size;
    } else disp_size = 1;
    // toggle RI
    if(prev_ri) cur_ri = 0;

    if(disp_size == NA_INTEGER) {
      // Both for R_nchar and if we directly detect an mb_err.  Whether this
      // throws an errors is a function of what ->settings has been set to.
      state->status = set_err(state->status, ERR_BAD_UTF8);
      disp_size = byte_size = 1;
    }
    int total_width = state->pos.w + disp_size;
    if(total_width > until && !overshoot) {
      state->status |= FANSI_STAT_DONE;
      break;
    } else {
      // We allow one overshoot if necessary and requested
      if(total_width == until) overshoot = 0;
      else if(total_width > until && overshoot) {
        state->status |= FANSI_STAT_OVERSHOT;
        until = total_width;
        overshoot = 0;
      }
      state->pos.x += byte_size;
      state->utf8 = state->pos.x;  // record after so no ambiguity about 0
      state->status &= cur_ri | ~FANSI_STAT_RI;
      state->status &= cur_zwj | ~FANSI_STAT_ZWJ;;
      // This could possibly overflow if byte_size >> 2 (e.g. if at some point
      // width reports the \U escape codes.
      if(state->pos.w > FANSI_lim.lim_int.max - disp_size)
        error("Internal Error:  width greater than INT_MAX"); // nocov

      // Don't count width of things following ZWJ
      int w_or_g = w_mode == FANSI_COUNT_WIDTH || w_mode == FANSI_COUNT_GRAPH;
      if(!prev_zwj || !w_or_g) {
        state->pos.w += disp_size;
      }
    }
    // Always break for error reporting
    if(mb_err) break;
  }
}
/*
 * Read a Character Off and Update State
 *
 * See GENERAL NOTES atop.
 */
void FANSI_read_next(
  struct FANSI_state * state, R_xlen_t i, const char * arg
) {
  unsigned char chr_val = (unsigned char) state->string[state->pos.x];
  // reset all flags except persistent ones
  state->status = state->status & FANSI_STAT_PERSIST;

  int is_utf8 = IS_UTF8(chr_val);
  int is_print = IS_PRINT(chr_val);
  if(is_print) read_one(state);
  else if (is_utf8) read_utf8_until(state, state->pos.w + 1, 1);
  else if (IS_ESC(chr_val)) read_esc(state, 0);
  else if(chr_val) read_c0(state);

  // Non-control or printable reset UTF8 state
  if(is_print || (!is_utf8 && !(state->status & FANSI_CTL_MASK)))
    state->status &= ~(FANSI_STAT_RI | FANSI_STAT_ZWJ);

  // Trigger errors / warnings if warranted
  alert(state, i, arg);
}
/*
 * Consume bytes until a certain width is met.
 *
 * See GENERAL NOTES atop.
 *
 * Will read as many bytes as possible within each of the child `read_*`
 * functions, but will need to drop out occasionally.  UTF8 state should survive
 * across calls to escape functions as terminals ignore them when composing
 * compound UTF8 graphemes.
 *
 * We always consume all controls less than target `until` but never those
 * greater than that.
 *
 * Two loop structure, where the second one cleans up any additional items that
 * do not add width.  `read_utf8_until` does read trailing zero width, but
 * cannot handle the case where there are controls sandwiched between zero width
 * chars so we need to do it here.
 *
 * @param until width measure not to exceed (subject to overshoot)
 * @param overshoot allow a wide character to overshoot `until`
 * @param term_i if true will not read trailing special sequences.
 * @param mode 0: 'start mode', read all controls, 1: 'stop mode', only read
 *   controls followed by zero width.
 */
void FANSI_read_until(
  struct FANSI_state * state, int until, int overshoot, int term_i,
  int mode, R_xlen_t i, const char * arg
) {
  char x;
  // - Basic Read --------------------------------------------------------------

  state->status = state->status & FANSI_STAT_WARNED;
  while(
    (x = state->string[state->pos.x]) &&
    state->pos.w < until &&
    !(state->status & (FANSI_STAT_OVERSHOT | FANSI_STAT_DONE))
  ) {
    // reset non-persistent flags
    state->status = state->status & (FANSI_STAT_PERSIST);

    if(IS_PRINT(x)) read_ascii_until(state, until, 1);
    else if(IS_UTF8(x)) read_utf8_until(state, until, overshoot);
    else if(IS_ESC(x)) read_esc(state, term_i);
    else if(x) read_c0(state);        // C0 escapes (e.g. \t, \n, etc)

    // Trigger errors / warnings if warranted
    alert(state, i, arg);
    if(FANSI_GET_ERR(state->status) == ERR_BAD_UTF8) goto EXIT;
  }
  // - Trail Read --------------------------------------------------------------

  // This handles mixed zero widths and escapes in trailing (utf8_until only
  // does zero width utf8s).
  //
  // 1. Consume all remaining zero width characters (+ those made so by ZWJ/RI)
  // 2. Consume all trailing controls, so long as they are followed by more
  //    zero width char or we are in start mode.

  overshoot = 0;
  until = state->pos.w;  // reset until to accomodate over/undershoot
  struct FANSI_state state_tmp = *state;
  while(
    (x = state_tmp.string[state_tmp.pos.x]) &&
    state_tmp.pos.w == until &&
    !IS_PRINT(x) &&
    !(state_tmp.status & FANSI_STAT_DONE)
  ) {
    // reset non-persistent flags
    state_tmp.status = state_tmp.status & (FANSI_STAT_PERSIST);
    // If a special, save the prior point if in terminate mode
    if(IS_UTF8(x)) {
      int pos_prev = state_tmp.pos.x;
      read_utf8_until(&state_tmp, until, overshoot);
      // Next one was not zero width, so UTF8 cannot be advanced
      if(state_tmp.pos.x == pos_prev) break;
      // There was a zero width, so advance
      if(state_tmp.pos.w == state->pos.w) *state = state_tmp;
    } else {
      if(IS_ESC(x)) read_esc(&state_tmp, term_i);
      else if(x) read_c0(&state_tmp);
      // If it ended up not being a control we're done
      if(!(state_tmp.status & FANSI_CTL_MASK)) break;
      // If in 'start' mode, read trailing controls
      else if (mode == 0) *state = state_tmp;
    }
    alert(&state_tmp, i, arg);
    if(FANSI_GET_ERR(state->status) == ERR_BAD_UTF8) goto EXIT;
  }
  alert(&state_tmp, i, arg);  // because of breaks
EXIT:
  state->status |= state_tmp.status & FANSI_STAT_WARNED;
}
void FANSI_read_all(struct FANSI_state * state, R_xlen_t i, const char * arg) {
  int term_i = 0;
  int until = FANSI_lim.lim_int.max;
  int overshoot = 0;
  int mode = 1;
  FANSI_read_until(state, until, overshoot, term_i, mode, i, arg);
}
