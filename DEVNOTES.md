# Developer Notes

These are internal developer notes.

## Todo

* How does `substr_ctl` manage continuation of styles across elements?
* Bunch of docs don't have @return tags, oddly.
* rename normalize -> expand (normalize not quite right).
* add tests with sgr -> normalize -> html comparisons
* Try to ensure index is in the same spot in all the writing funs.
* Document the semantics of style across a vector with multiple elements.
* All writing functions should advance for consistency, and have same sig.
* Make sure we check we're not using `intmax_t` or `uintmax_t` in a tight loop
  anywhere.
* Review all overflow checks.
* Cleanup limits structure, is it really needed now we have a better view of
  what we're dealing with?
* Can we manage the stack better with the growing buffer so we don't keep all
  the prior half sized ones around until we exit so they are eligible for gc?
* Do sgr_to_HTML (sgr_to_html2?), add check to sgr_to_html if any of the bad
  characters are found to escape or use `sgr_to_html2`.

## Done

* Confirm that in e.g. `intmax_t > int` everything the comparison is done in
  `intmax_t` terms, not int.

Yes, the only issue is when a possibly signed value needs to be promoted to an
unsigned one (I think).

* Make FANSI_writeline static.

* Change state_init to intake a CHARSXP to ensure we cannot initialize with
  something larger than R_len_t.

Currently takes STRSXP.  A little awkward though, but we did it that way because
we had the index.

## Crayon Compatibility

### Updating Crayon

We could modify the code to add in addition to `st$open` and `st$close` a
`st$closedby`, where the last would be transformed into a regex that matches any
closing sequence.  This will work even nested as in e.g.

red('hello ', red('wor\033[0mld') , ' yo')

Result would be:

    \033[32mhello \033[31mwo\033[0\033[32m\033[31mld\033[39m\033[32m yo\033[39m

What happens is that the inner step is done first, adding its color after
`\033[0m`, but then the outer step happens, and adds its color in between the
`\033[0m` and the `\033[31m` just added, so the inner step dominates, which is
exactly what we want.

Issue are:

* We need to use regular expressions, not fixed, so might be a little slower.
* It will not deal with things like "\033[1;31m".

### Fansi Changes

`normalize_sgr`: take all known SGRs form `\033[31;42m` and re-write them into
`\033[31m\033[42m`.

> What about non SGR escapes?  Should process be: strip non-SGR, then normalize?
> Or do we just ignore non-SGR and let the user decide what they want to do with
> the warnings?

One problem with `normalize_sgr` is it will substantially complicate the code
for anything computing width if we try to make it part of it.  So likely best
way to deal with it is in "crayon.compat" mode, do a two pass process: first
normalize, second do our thing.  Obviously this will be slower.

Should `normalize_sgr` convert `\033[0m` and similar to the exploded version?
And should it be exploded to all closing tags, or only to the active ones?
Probably only the active ones.

It's probably out of the question to normalize as we process, but it's not out
of the question to write the closing tags on each.  What would it take to
normalize as we process?

* Known how to compute the normalized size.  Won't be able to use the
  `copy_or_measure` framework as we won't know ahead of time what the string is,
  which means we would have to generate it twice to use `copy_or_measure`.
* Instead of skipping all the way to the end of the line and copying the whole
  thing, we'd have to copy up to each ESC, and write the normalized version of
  it.

We get most of the value by just writing the end-tags, and that's easy and cheap
to do.  It's just that then there is no great interface.  Either we normalize
everything after the fact at the cost of rewriting it all, or we do a half-assed
version where we hope we don't have to normalize the internals and write the
normalized end tags.  Maybe that's the compromise.  Look for unnormalized tags
inside, and if any exist, then use the full second pass, otherwise just write
the normalized end tags.

We also need to decide what is unnormalized.  For crayon, all we really care
about is that "close" tags be pulled out, and they don't even have to be really
pulled out, they just need to be appended.

There is also the R level functions which use `substr`, so those will have to be
two-pass unless we rewrite substr internally.

So maybe first thing to do is the normalize version?  Then we can think if we
want to change writelines?

Aside: if we truly want to insulate `fansi` output from external SGR, we really
should be starting with `\033[0m` too.  Maybe another option is to "insulate",
which would just start and end with the null SGR, optionally?

The other thing we can do is accept an "state.initial" input, and output a
"state.end".  This way we can merge with any other SGR strings.

### Normalize

Biggest issue is dealing with the concept of closing tags, which we're really
not set up to deal with.  However, we can have a pre-state, and a post-state,
and we can figure out what transformations need to be applied to go from one to
the other.

So maybe we can't get fully away from mapping all the open and close styles in
some way.

If pre had color, but post does not, close color.
If pre had x, but post does not, issue closing tag.

Generate a list of the styles that pre had but post doesn't, and only those.
Then, generate a `close_active_sgr` on that style.


## Overflow

Set R_LEN_T_MAX to INT_MAX - N, and check that on expansion we get the correct
error at write time.

Tests:

* State carrying over from one string element to the next?

* Close all styles, half of the styles, the other half.

## To HTML

Done.

## FANSI_find_esc

Clearly our changes messed stuff up.  In particular, there is now the
possibility that FANSI_find_esc won't advance if there are no handled `ctl`
sequences found, which is different since before all sequences were guaranteed
to advance.  We kludged a fix for `nzchar`, but this is causes problems
elsewhere.  We need to review all uses of `FANSI_find_esc` and figure out what a
compatible way to handle the possibility that `FANSI_find_esc` will find a
_Control Sequence_ that isn't actually treated as _Control Sequence_.

## Width of C0 And Others

The correct way to handle this is probably to keep existing behavior for `_ctl`
functions, but make sure there are `_sgr` functions for everything.  In the
latter, unless we're dealing with special cases (e.g. perhaps a future
`substr2_sgr`, not sure this even makes sense), things other than SGR are
counted as their character width.  Even other CSI escapes?

Implications for warnings?  Or do we still emit those because even the
zero-width assumption may be incorrect?  The latter probably.

So the main issue is that we need a mechanism for conveying the `what` as we do
in `FANSI_find_esc`.  Do we make it part of `state`, or do we pass it along as
an extra argument?  `FANSI_find_esc` specifically does not use `state` object at
all, so maybe we do make it part of the `state` object when it is available.

Also need to resolve the naming of `strip`.  It was very convenient in the
context of `strip_ctl` and even kind of made sense with `nchar_ctl`, but is
really a stretch in the other functions.  `what` may be more appropriate.
Probably even better is `ctrl`, and figure out if there is a backwards
compatible way to detect this.  Possibly switch `ctrl` to be in the same
position in the signature, put `strip` at the end, and if `strip` is specified
issue a message that it is deprecated.  The alternative is to live with it and
then deal with the duplicate parameter docs.

Need to add tests for:

* use of 'ctl' parameter everywhere
* use of deprecated 'strip'/'which' parameters everywhere
* treatment of control sequences excluded

BIG QUESTION: is behavior of the `_sgr` variety of functions going to change
with respect to embedded control sequences (yes..., is okay to do this change,
almost certainly no one will notice).

## TIL

Some random thoughts for a possible post about the perils of compiler
optimization based on the ICC issue we had in #52.

* char vs unsigned char vs signed char
* the compiler is always right
* ICC is proprietary
* just because you are paranoid

## SGR Capabilities

* 256 colors     (yes)
* Bright Colors  (yes)
* Truecolor      (no)

* Border
* Ideogram
* Fonts

Mostly we need to worry about capabilities in the case were not having them
causes stuff to be interpreted differently.  In particular, with 256 and true
color, not having capabilities changes the interpretation of the next character.

Also with alternate color schemes like 90-100 if we don't support them then they
won't interfere with existing colors.

All the other codes don't really interfere with each other so we don't have to
worry too much about them.

## Tabs

Started off thinking that we should account for tabs everywhere, but decided in
the end that the only rational thing to do is to switch tabs for spaces in the
input string, and then wrap that.

This means we have to go back and remove all the tab business we started adding
when we thought we were going to do tab handling everywhere.

## Functions

* `substr`
* `strip_ansi`
* `strwrap`
* `trim`

## What do we do Next?

* Spend time figuring out how to integrate character width?
* All C substr?
* Generating a table of state vs. position

## State vs. Position

Should position be in bytes?  Probably in characters to use in combination with
ansi_strsub or whatever that ends up called.

## UTF-8 issues

### Grapheme recognition

Need to implement graphemes and word boundaries, although that is likely to
happen later on if at all.

  * [unicode segmentation](http://www.unicode.org/reports/tr29/)
  * [utf8lite](https://github.com/patperry/utf8lite) seems to implement this

Additionally, it's not clear how useful it is to fully implement this since it
relies on terminals displaying correctly anyway.  Interestingly, the hangul
business is recognized properly in the zero-length follow on chars:

```
> cat('\u1100\u1161\u11A8\n')
각
> nchar('\u1100\u1161\u11A8\n')
[1] 4
> nchar('\u1100\u1161\u11A8', type='width')
[1] 2
> nchar('\u1161\u11A8', type='width')
[1] 0
> nchar('\u1161', type='width')
[1] 0
```

### Emoji

Main problem with emoji is that UTF-8 handling and emoji handling in R seem
pretty terrible (though maybe not all of this is Rs fault).  Several problems:

* `nchar` typical returns 1 even in width mode; maybe this is okay because in
  terminal display these are actually displayed as one width and you end up wiht
  overlapping emoji.  However, this calculation then isn't particularly portable
* ZWJ width is correctly calculated at zero; however, the total width of a
  sequence doesn't seem to collapse.  Maybe this is okay because the sequences
  themselves aren't collapsed by the terminal (and even firefox).
* Emoji modifiers do appear to be collapsed by the terminal, but
  `nchar(type='width')` does not recognize this `"\U1F466\U1F3FF"`
  (dark person).
```
nchar("\U1F466")                      # man
nchar("\U1F466", type='width')
nchar("\U1F466\U1F3FF", type='width') # dark man
cat("\U1F466\U1F3FF\n")
```
* Some combining characters are correctly recognized (e.g. `"A\u30A"`, combining
  ring), interestingly the combining ring itself is reported as width zero.


## What About Strip and Wrap?

We could:

* record position and content of all ANSI tags,
* strip them,
* `strwrap`
* Re-insert tags
    * This will require counting bytes on every chunked line

One problem with this is that we end up having to copy the strings several
times, including complex piecewise copies to insert the tags in the middle of
strings. Another issue specific to `strwrap` is that `strwrap` takes several
liberties with the strings, so we would have to make sure that they are in a
form that `strwrap` won't feel compelled to modify, otherwise we'll end up
offset.

What about `substr`?  Could we just do the same.  Strip all tags, run the
`substr` normally, and then inject the tags back?  But with `substr` we will
have no idea what the byte positions of the substrings are, so this is probably
not an option.

For `strwrap` less of an issue because we should have the entire string so we
can count from the beginning.  However, with the white space modification and
indents and all that business it starts getting hairy to keep track of
everything.

Another problem with this is that we pretty much have to process the tags on the
whole string as we can't be sure how long it is.  E.g. if the user asks for 20
characters for `substr`, we can't stop because we don't know how many bytes it
would take to get to twenty characters.

Another problem is that we have to issue multiple R calls.

The alternative is to compute the width of every dubious character ourselves and
figure out the byte position of the wrap points.  The main issue with this is
that we have to :

* Create the STRSXP for use with R_nchar.
    * Need to compute the bytes for each element, including UTF-8, etc
    * Possibly need to figure out combining characters (e.g. emojis)
* Need a vector as long as # of characters to keep track of offsets or TRUE
  position, or, more likely, for each character the position?  We'll need the
  length too.

So we need to track the ANSI offsets as well as the UTF8 many bytes to one or
many bytes to many.  So we need for each character, it's offset, or for each
special character sequence, the offset it adds?  For stwrap we need to track
both, for substr we only need to track the number of characters, not their
display width.

Do we want to do this on an as-needed basis?  Possibly, only issue is if we use
`substr` with the same string repeated and overlapping cuts.  I guess we just
need to record the byte position of each start/end spot, so we can still do on
an as needed basis.

So we will walk the string until we pass all the cut points.

## substr_csi

* For each cut point
* Compute all offsets (CSI and UTF-8)

* Need:
    * Byte position
    * Char length in bytes (use 0 for sub-elements of UTF8 sequences)?
    * Display width (0 for ANSI, and 0 for sub-elements)

## Benchmarks

### Strip

Testing with the following:

```
strings.all <- crayon::red(
  paste("hello ", crayon::green("green"), "world", 1:1000)
)
strings <- strings.raw <- strip_esc(strings.all)
strings.index <- sample(1:1000, 100)
strings[strings.index] <- strings.all[strings.index]
```

After `csi_pos`:

```
Unit: microseconds
                              expr     min       lq      mean   median
     crayon:::strip_style(strings) 285.397 290.5625 356.48010 321.1050
 crayon:::strip_style(strings.raw) 212.972 215.9980 257.45429 225.1195
 crayon:::strip_style(strings.all) 822.599 827.4575 988.03038 890.1975
               strip_ansi(strings)  46.927  49.2775  61.26973  52.0475
           strip_ansi(strings.raw)  22.405  23.9160  29.50136  25.2045
           strip_ansi(strings.all) 226.428 242.0230 286.31130 253.0315
```

After we switch to two pass, doesn't really seem to help at all, in fact,
potentially hurts.

```
Unit: microseconds
                              expr      min        lq       mean    median
     crayon:::strip_style(strings)  351.474  358.0145  433.34345  382.6325
 crayon:::strip_style(strings.raw)  228.161  231.9630  284.77289  248.2040
 crayon:::strip_style(strings.all) 1285.489 1301.6820 1557.43844 1413.1550
               strip_ansi(strings)   47.150   50.7820   66.48662   57.0415
           strip_ansi(strings.raw)   14.434   15.1005   18.33791   15.9875
           strip_ansi(strings.all)  270.206  274.5760  335.49526  296.2285
```

Back to single pass:

```
Unit: microseconds
                              expr      min        lq       mean    median
     crayon:::strip_style(strings)  352.317  359.2645  449.05838  384.8680
 crayon:::strip_style(strings.raw)  228.327  235.5820  289.30838  250.6775
 crayon:::strip_style(strings.all) 1288.614 1303.5800 1587.23839 1434.4435
               strip_ansi(strings)   47.324   48.8360   61.07693   53.0505
           strip_ansi(strings.raw)   18.575   19.5350   24.16552   20.7085
           strip_ansi(strings.all)  254.820  258.8915  320.55084  267.1585
                  has_csi(strings)   17.205   18.4330   24.54812   20.4120
```

A little odd that single pass is slower for the raw strings.

One major source of slowness in existing implementation is that `gregexpr` is
really slow:

```
strings3 <- paste("hello ", format(1:1e4), "\033[0m world")

Unit: microseconds
                                              expr       min        lq
     grep(crayon:::ansi_regex, strings3, perl = T)  2789.884  3314.459
 gregexpr(crayon:::ansi_regex, strings3, perl = T) 31016.941 46144.304
                                 has_csi(strings3)   773.735   936.246
           gsub(crayon:::ansi_regex, "", strings3) 19612.712 23770.707
      mean    median        uq        max neval
  3679.146  3518.972  3891.466   6459.200   100
 58982.953 51241.905 60528.666 144145.516   100
  1101.074  1013.913  1135.477   4346.849   100
 25907.785 25017.828 27647.528  36121.900   100
 ```
 This pretty is unfortunate as it would allow us to take advantage of the
 correct character offsets for use with `substr` in a very easy manner.


### Wrap

```
Unit: microseconds
                                     expr      min       lq      mean    median
 yy <- ansi_substr2(y.rep, starts, stops)  329.705  342.663  516.4295  579.5910
       zz <- substr(x.rep, starts, stops)   62.264   62.839   71.8798   67.0075
     zz <- stri_sub(x.rep, starts, stops)   10.290   14.488  372.0339   15.9330
             strwrap(x.paste, width = 60) 1076.706 1107.804 1265.9602 1153.7485
           stri_wrap(x.paste, width = 60)  651.140  672.613 2847.7724  684.6655
```

### nchar

Seems like we wasted a lot of time writing dedicated code...:

```
source('tests/unitizer/_pre/lorem.R')

utf8.big <- rep(c(lorem.ru, lorem.cn), 10000)
system.time(utf8.big.wrap <- strwrap2_esc(utf8.big, 71, wrap.always=TRUE))
utf8.c <- fansi:::colorize(utf8.big.wrap)
utf8.c <- colorize(utf8.big.wrap)
library(microbenchmark)
microbenchmark(times=1,
  nchar(utf8.big.wrap),
  nchar(utf8.big.wrap, type='width'),
  nchar_esc(utf8.c, type='width'),
  nchar(strip_esc(utf8.c), type='width')
)
## Unit: milliseconds
##                                      expr       min        lq      mean
##                      nchar(utf8.big.wrap)  131.7149  131.7149  131.7149
##      nchar(utf8.big.wrap, type = "width") 3749.5948 3749.5948 3749.5948
##         nchar_esc(utf8.c, type = "width") 3930.9955 3930.9955 3930.9955
##  nchar(strip_esc(utf8.c), type = "width") 3930.2643 3930.2643 3930.2643
```

Just doesn't seem worth the hassle even if we could get better.
