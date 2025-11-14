
# Notes for Replacing `R_nchar`

## Docs

DB at https://www.unicode.org/ucd/

## Fast Lookups

https://here-be-braces.com/fast-lookup-of-unicode-properties/

## R Implementation

Use [`uniset`](https://github.com/depp/uniset) (note: not sure this is the one)
to generate `src/main/rlocale_widths.h`, then used by `Ri18n_wcwidth` in
`src/main/rlocale.c`

We need zero-width, as well as 2 width.

## Emoji

## East Asian Width

## Zero Width?

