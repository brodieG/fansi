# fansi Release Notes

## v0.2.3

* [#44](https://github.com/brodieG/fansi/issues/44): include bright color
  support in HTML conversion.

## v0.2.2

* Remove valgrind uninitialized string errors by avoiding `strsplit`.
* Reduce R dependency to >= 3.2.x (@gaborcsardi)
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


