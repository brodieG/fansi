## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

Addresses valgrind errors Prof. Ripley
requested be fixed.  Previously
I did not test with a level 2 valgrind
build of R, so I did not detect the
errors that showed up on CRAN.  I was
able to reproduce and correct those
errors with a level 2 build.

I noticed some of the earlier versions
of R-devel (e.g. 3/16) produced some
errors, but those were all within R
code (e.g. from gregexpr_Regexc),
although I do not see those errors with
the most recent R-devel versions.

## R CMD check --as-cran

Status: OK

### Test Environments

* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-03-30 r74497)
    * R version 3.4.4 (2017-01-27)
    * R version 3.2.5 (2017-01-27)
* Winbuilder
    * R version 3.4.4 (2017-01-27)
      https://win-builder.r-project.org/R42Hn7ptEbNL
    * R version 3.5.0 alpha (2018-03-27 2018-03-30 r74498)
      https://win-builder.r-project.org/vepEmtGc4iSc
* r-debug docker container, level 2 valgrind
    * R devel (2018-03-16 r74417)
* Locally on Mac OS 10.12.6
    * R version 3.4.1 (2017-06-30)
* rhub i386-pc-solaris2.10 (32-bit):
    * R version 3.4.1 Patched (2017-07-15 r72924)
      http://builder.r-hub.io/status/fansi_0.2.2.tar.gz-581df17de80f449d9600bcc3f50f8f0c


