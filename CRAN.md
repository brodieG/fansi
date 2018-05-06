## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

This is a maintenance release that adds
compatibility for R 3.1 and fixes some
minor bugs.

## R CMD check --as-cran

Status: OK

### Test Environments

* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-05-05 r74699)
    * R version 3.1.3 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
    * R version 3.5.0 (2017-01-27)
* Winbuilder
    * R version 3.5.0 alpha (2018-03-27 2018-03-30 r74498)
      https://win-builder.r-project.org/S6gm2uv0hNw4
* r-debug docker container, level 2 valgrind
    * R devel (2018-05-01 r74677)
* rchk image
* Locally on Mac OS 10.12.6
    * R version 3.4.1 (2017-06-30)
* rhub i386-pc-solaris2.10 (32-bit):
    * R version 3.4.1 Patched (2017-07-15 r72924)
      http://builder.r-hub.io/status/fansi_0.2.3.tar.gz-0c26eb12021b4fe69c70cb8785be09f1


