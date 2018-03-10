<!-- README.md is generated from README.Rmd. Please edit that file -->


# fansi - ANSI Control Sequence Aware String Functions

[![](https://travis-ci.org/brodieG/fansi.svg?branch=master)](https://travis-ci.org/brodieG/fansi)
[![](https://codecov.io/github/brodieG/fansi/coverage.svg?branch=master)](https://codecov.io/github/brodieG/fansi?branch=master)
[![](http://www.r-pkg.org/badges/version/fansi)](https://cran.r-project.org/package=fansi)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

Counterparts to R string manipulation functions that account for the effects of
ANSI text formatting control sequences.

## Formatting String with Control Sequences

Many terminals will recognize special sequences of characters in strings and
change display behavior as a result.  For example, on my terminal the sequence
`"\033&#91;42m"` turns text background green:

![hello
world](https://raw.githubusercontent.com/brodieG/fansi/rc/extra/images/hello.png)

The sequence itself is not shown, but the text display changes.

This type of sequence is called an ANSI CSI SGR control sequence.  Most &#42;nix
terminals support them, and newer versions of Windows and Rstudio consoles do
too.  You can check whether your display supports them by running
`term_cap_test()`.

Whether the `fansi` functions behave as expected depends on many factors,
including how your particular display handles Control Sequences.  See `?fansi`
for details, particularly if you are getting unexpected results.

## Control Sequences Require Special Handling

ANSI control characters and sequences (Control Sequences hereafter) break the
relationship between byte/character position in a string and display position.

For example, in `"Hello \033&#91;42mWorld, Good\033&#91;m Night Moon!"` the `,`
after "World" is thirteenth displayed character, but the eighteenth in
memory.  If we try to split the string after the comma with `substr` things go
wrong in several ways:


![bad substring](https://raw.githubusercontent.com/brodieG/fansi/rc/extra/images/substr.png)

We end up cutting up our string in the middle of "World", and worse the
formatting bleeds out of our string into the prompt line.  Compare to what
happens when we use `substr_ctl`, the Control Sequence aware version of
`substr`:

![good substring](https://raw.githubusercontent.com/brodieG/fansi/rc/extra/images/substr_ctl.png)

## Functions

`fansi` provides counterparts to the following string functions:

* `substr`
* `strwrap`
* `strtrim`
* `nchar` / `nzchar`

These are drop-in replacements that behave (almost) identically to the base
counterparts, except for the Control Sequence awareness.  There are also
slightly improved versions of them that provide additional functionality, and
utility functions such as `strip_ctl` to remove Control Sequences and `has_ctl`
to detect whether strings contain them.

Most of `fansi` is in C so you should find performance of the `fansi` functions
to be comparable to the base functions, or in some cases like `strwrap_ctl` much
faster.

## HTML Translation

You can translate ANSI CSI SGR formatted strings into their HTML counterparts:

![translate to html](https://raw.githubusercontent.com/brodieG/fansi/rc/extra/images/sgr_to_html.png)

## Installation

This package is currently only available on Github.  You can install it with `substr2_ctl(utf8.bad, 1, 7, type = "width")` or:


```r
fansi.dl <- tempfile()
fansi.uz <- tempfile()
download.file('https://github.com/brodieG/fansi/archive/master.zip', fansi.dl)
unzip(fansi.dl, exdir=fansi.uz)
install.packages(file.path(fansi.uz, 'fansi-master'), repos=NULL, type='source')
unlink(c(fansi.dl, fansi.uz))
```

## References

* [ECMA-48 - Control Functions For Coded Character
  Sets](http://www.ecma-international.org/publications/standards/Ecma-048.htm),
  in particular pages 10-12, and 61.
* [CCITT Recommendation T.416](https://www.itu.int/rec/dologin_pub.asp?lang=e&id=T-REC-T.416-199303-I!!PDF-E&type=items)
* [ANSI Escape Code - Wikipedia](https://en.wikipedia.org/wiki/ANSI_escape_code)
  for a gentler introduction.

## Acknowledgments

* R Core for developing and maintaining such a wonderful language.
* CRAN maintainers, for patiently shepherding packages onto CRAN and maintaining
  the repository, and Uwe Ligges in particular for maintaining
  [Winbuilder](http://win-builder.r-project.org/).
* [Gábor Csárdi](https://github.com/gaborcsardi) for getting me started on the
  journey through the crazy world of ANSI control sequences.
* [Jim Hester](https://github.com/jimhester) because
  [covr](https://cran.r-project.org/package=covr) rocks.
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
  Csárdi](https://github.com/gaborcsardi) and the
  [R-consortium](https://www.r-consortium.org/) for
  [Rhub](https://github.com/r-hub), without which testing bugs on R-devel and
  other platforms would be a nightmare.
* [Tomas Kalibera](https://github.com/kalibera) for
  [rchk](https://github.com/kalibera/rchk) and rcnst to help detect errors in
  compiled code.
* [Winston Chang](https://github.com/wch) for the
  [r-debug](https://hub.docker.com/r/wch1/r-debug/) docker container,
  especially now that valgrind is no longer working on my OSX system.
* Hadley Wickham for [devtools](https://cran.r-project.org/package=devtools) and
  [roxygen2](https://cran.r-project.org/package=roxygen2).
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by extension
  John MacFarlane for [pandoc](http://pandoc.org/).
* Olaf Mersmann for
  [microbenchmark](https://cran.r-project.org/package=microbenchmark), because
  microsecond matter.
* All open source developers out there that make their work freely available
  for others to use.
* [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
  [Codecov](https://codecov.io/), [Vagrant](https://www.vagrantup.com/),
  [Docker](https://www.docker.com/), [Ubuntu](https://www.ubuntu.com/),
  [Brew](https://brew.sh/) for providing infrastructure that greatly simplifies
  open source development.
* [Free Software Foundation](http://fsf.org/) for developing the GPL license and
  promotion of the free software movement.



