# fansi Release Notes

## v0.3.0

* `fansi::set_knit_hooks` makes it easy to automatically convert ANSI CSI SGR
  sequences to HTML in Rmarkdown documents.  We also add a vignette that
  demonstrates how to do this.
* [#53](https://github.com/brodieG/fansi/issues/53): fix for systems where
  'char' is signed (found and fixed by @QuLogic).
* [#52](https://github.com/brodieG/fansi/issues/52): fix bad compilation under
  ICC (@kazumits).
* [#51](https://github.com/brodieG/fansi/issues/51): documentation improvements
  (@krlmlr).
* [#50](https://github.com/brodieG/fansi/issues/50): run tests on R 3.1 - 3.4
  tests for the rc branch only (@krlmlr).
* [#48](https://github.com/brodieG/fansi/issues/48): malformed call to error
  in FANSI_check_enc (@msannell).
* [#47](https://github.com/brodieG/fansi/issues/47): compatibility with R
  versions 3.2.0 and 3.2.1 (@andreadega).

## v0.2.3

* [#45](https://github.com/brodieG/fansi/issues/45): add capability to run under
  R 3.1 [hadley](https://github.com/hadley), [Gábor
  Csárdi](https://github.com/gaborcsardi).
* [#44](https://github.com/brodieG/fansi/issues/44): include bright color
  support in HTML conversion (h/t [Will Landau](https://github.com/wlandau)).

Other minor fixes ([#43](https://github.com/brodieG/fansi/issues/43), [#46](https://github.com/brodieG/fansi/issues/46)).

## v0.2.2

* Remove valgrind uninitialized string errors by avoiding `strsplit`.
* Reduce R dependency to >= 3.2.x (@gaborcsardi).
* Update tests to handle potential change in `substr` behavior starting with
  R-3.6.

## v0.2.1

* All string inputs are now encoded to UTF-8, not just those that are used in
  width calculations.
* UTF-8 tests skipped on Solaris.

## v0.2.0

* Add `strsplit_ctl`.

## v0.1.0

Initial release.


