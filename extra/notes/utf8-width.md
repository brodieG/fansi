# Notes for Replacing `R_nchar`

## Docs

DB at `https://www.unicode.org/ucd/`

DB Docs: `https://www.unicode.org/reports/tr44/`

## Fast Lookups

https://here-be-braces.com/fast-lookup-of-unicode-properties/

We're skipping this for now

## Known Differences

### Vs Base R

We compared widths with extra/width/width-compare.R and UCD 13.0.0 and R4.5.1
and got:

    --- fansi vs base ---
         base
    fansi      0      1      2
        0   2237      0      5
        1      0 927364      4
        2      0     26 182428

The 26 are the regional indicators, which fansi treats specially when they show
up as single characters.  The 5 at (0,2) are the skin tones.  The 4 at (1,2) are
end of plane reserved chars.

### Vs utf8

    --- fansi vs utf8 ---
          utf8
    fansi       0      1      2   <NA>
      0      2124    159      5     67
      1      3771  25192      0 898238
      2         2     26 117183  65297
      <NA>      0      0      0      0

The 3771 in (0,1) are mostly deprecated tag characters, plus the specials block
in FFF0-8, plus FFA0 a halfwidth filler (the latter does appear to render as
zero but we haven't tried it in korean context).

The (0,1) is the Jamo stuff, the (0,2) the skin tones, the (0,NA) the C1
controls plus fff[9-b], the (2,0) ones are hangul fillers.

My terminal does not seem to render the hangul fillers, but it does seem they
are supposed to be spacing characters.

## R Implementation

Use [`uniset`](https://github.com/depp/uniset) (note: not sure this is the one)
to generate `src/main/rlocale_widths.h`, then used by `Ri18n_wcwidth` in
`src/main/rlocale.c`

We need zero-width, as well as 2 width.

## Emoji

## East Asian Width

## Zero Width?

