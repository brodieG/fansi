<!-- README.md is generated from README.Rmd. Please edit that file
library(rmarkdown)
render('README.Rmd', output_format=md_document())
render('README.Rmd', output_format=html_document())
 -->
fansi - ANSI Control Sequence Aware String Functions
====================================================

[![R build
status](https://github.com/brodieG/fansi/workflows/R-CMD-check/badge.svg)](https://github.com/brodieG/fansi/actions)
[![](https://codecov.io/gh/brodieG/fansi/branch/master/graphs/badge.svg?branch=master)](https://codecov.io/github/brodieG/fansi?branch=master)
[![](http://www.r-pkg.org/badges/version/fansi)](https://cran.r-project.org/package=fansi)
[![Dependencies
direct/recursive](https://tinyverse.netlify.app/badge/fansi)](https://tinyverse.netlify.app/)

Counterparts to R string manipulation functions that account for the
effects of ANSI text formatting control sequences.

Formatting Strings with Control Sequences
-----------------------------------------

Many terminals will recognize special sequences of characters in strings
and change display behavior as a result. For example, on my terminal the
sequence `"\033[42m"` turns text background green:

![hello
world](https://raw.githubusercontent.com/brodieG/fansi/rc/extra/images/hello.png)

The sequence itself is not shown, but the text display changes.

This type of sequence is called an ANSI CSI SGR control sequence. Most
\*nix terminals support them, and newer versions of Windows and Rstudio
consoles do too. You can check whether your display supports them by
running `term_cap_test()`.

Whether the `fansi` functions behave as expected depends on many
factors, including how your particular display handles Control
Sequences. See `?fansi` for details, particularly if you are getting
unexpected results.

Control Sequences Require Special Handling
------------------------------------------

ANSI control characters and sequences (*Control Sequences* hereafter)
break the relationship between byte/character position in a string and
display position.

For example, in `"Hello \033[42mWorld, Good\033[m Night Moon!"` the
space after “World,” is thirteenth displayed character, but the
eighteenth actual character (“\\033” is a single character, the ESC). If
we try to split the string after the space with `substr` things go wrong
in several ways:

![bad
substring](https://raw.githubusercontent.com/brodieG/fansi/master/extra/images/substr.png)

We end up cutting up our string in the middle of “World”, and worse the
formatting bleeds out of our string into the prompt line. Compare to
what happens when we use `substr_ctl`, the *Control Sequence* aware
version of `substr`:

![good
substring](https://raw.githubusercontent.com/brodieG/fansi/master/extra/images/substr_ctl.png)

Functions
---------

`fansi` provides counterparts to the following string functions:

-   `substr` (and `substr<-`)
-   `strsplit`
-   `strtrim`
-   `strwrap`
-   `nchar` / `nzchar`
-   `trimws`

These are drop-in replacements that behave (almost) identically to the
base counterparts, except for the *Control Sequence* awareness.

`fansi` also includes improved versions of some of those functions, such
as `substr2_ctl` which allows for width based substrings. There are also
utility functions such as `strip_ctl` to remove *Control Sequences* and
`has_ctl` to detect whether strings contain them.

Much of `fansi` is written in C so you should find performance of the
`fansi` functions to be slightly slower than the corresponding base
functions, with the exception that `strwrap_ctl` is much faster.
Operations involving `type = "width"` will be slower still. We have
prioritized convenience and safety over raw speed in the C code, but
unless your code is primarily engaged in string manipulation `fansi`
should be fast enough to avoid attention.

HTML Translation
----------------

You can translate ANSI CSI SGR formatted strings into their HTML
counterparts with `to_html`:

![translate to
html](https://raw.githubusercontent.com/brodieG/fansi/master/extra/images/sgr_to_html.png)

Rmarkdown
---------

It is possible to set `knitr` hooks such that R output that contains
ANSI CSI SGR is automatically converted to the HTML formatted equivalent
and displayed as intended. See the
[vignette](https://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/fansi/issue61/doc/sgr-in-rmd.html)
for details.

Installation
------------

This package is available on CRAN:

    install.packages('fansi')

It has no runtime dependencies.

For the development version use
`remotes::install_github('brodieg/fansi@development')` or:

    f.dl <- tempfile()
    f.uz <- tempfile()
    github.url <- 'https://github.com/brodieG/fansi/archive/development.zip'
    download.file(github.url, f.dl)
    unzip(f.dl, exdir=f.uz)
    install.packages(file.path(f.uz, 'fansi-development'), repos=NULL, type='source')
    unlink(c(f.dl, f.uz))

There is no guarantee that development versions are stable or even
working. The master branch typically mirrors CRAN and should be stable.

Related Packages and References
-------------------------------

-   [crayon](https://github.com/r-lib/crayon), the library that started
    it all.
-   [ansistrings](https://github.com/r-lib/ansistrings/), which
    implements similar functionality.
-   [ECMA-48 - Control Functions For Coded Character
    Sets](https://www.ecma-international.org/publications-and-standards/standards/ecma-48/),
    in particular pages 10-12, and 61.
-   [CCITT Recommendation
    T.416](https://www.itu.int/rec/dologin_pub.asp?lang=e&id=T-REC-T.416-199303-I!!PDF-E&type=items)
-   [ANSI Escape Code -
    Wikipedia](https://en.wikipedia.org/wiki/ANSI_escape_code) for a
    gentler introduction.

Acknowledgments
---------------

-   R Core for developing and maintaining such a wonderful language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository, and Uwe Ligges in particular for
    maintaining [Winbuilder](http://win-builder.r-project.org/).
-   [Gábor Csárdi](https://github.com/gaborcsardi) for getting me
    started on the journey ANSI control sequences, and for many of the
    ideas on how to process them.
-   [Jim Hester](https://github.com/jimhester) for
    [covr](https://cran.r-project.org/package=covr), and with Rstudio
    for [r-lib/actions](https://github.com/r-lib/actions).
-   [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
    Boettiger](https://github.com/cboettig) for the
    [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
    Csárdi](https://github.com/gaborcsardi) and the
    [R-consortium](https://www.r-consortium.org/) for
    [Rhub](https://github.com/r-hub), without which testing bugs on
    R-devel and other platforms would be a nightmare.
-   [Tomas Kalibera](https://github.com/kalibera) for
    [rchk](https://github.com/kalibera/rchk) and the accompanying
    vagrant image, and rcnst to help detect errors in compiled code.
-   [Winston Chang](https://github.com/wch) for the
    [r-debug](https://hub.docker.com/r/wch1/r-debug/) docker container,
    in particular because of the valgrind level 2 instrumented version
    of R.
-   [Hadley Wickham](https://github.com/hadley/) and [Peter
    Danenberg](https://github.com/klutometis) for
    [roxygen2](https://cran.r-project.org/package=roxygen2).
-   [Yihui Xie](https://github.com/yihui) for
    [knitr](https://cran.r-project.org/package=knitr) and [J.J.
    Allaire](https://github.com/jjallaire) et al. for
    [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by
    extension John MacFarlane for [pandoc](https://pandoc.org/).
-   [Gábor Csárdi](https://github.com/gaborcsardi), the
    [R-consortium](https://www.r-consortium.org/), et al. for
    [revdepcheck](https://github.com/r-lib/revdepcheck) to simplify
    reverse dependency checks.
-   Olaf Mersmann for
    [microbenchmark](https://cran.r-project.org/package=microbenchmark),
    because microsecond matter, and [Joshua
    Ulrich](https://github.com/joshuaulrich) for making it lightweight.
-   All open source developers out there that make their work freely
    available for others to use.
-   [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
    [Codecov](https://about.codecov.io/),
    [Vagrant](https://www.vagrantup.com/),
    [Docker](https://www.docker.com/), [Ubuntu](https://ubuntu.com/),
    [Brew](https://brew.sh/) for providing infrastructure that greatly
    simplifies open source development.
-   [Free Software Foundation](https://www.fsf.org/) for developing the
    GPL license and promotion of the free software movement.
