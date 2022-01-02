<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{The title of the vignette}
  \usepackage[utf8]{inputenc}
-->

---
title: "ANSI CSI SGR Sequences in Rmarkdown"
author: "Brodie Gaslam"
css: styles.css
---
A
```{.R echo=FALSE resuts=FALSE}
library(fansi)
eval_capt_vis <- function(x, env=parent.frame()) {
  capture.output({
    res <- eval(bquote(withVisible(.(x))), envir=env)
    if(res[['visible']]) {
      if(isS4(res[['value']])) show(res[['value']])
      else print(res[['value']])
    }
  })
}
out_fun <- (
  function() {
    env <- new.env()
    function(
      code, language = "R", id = "",
      echo = TRUE, eval = TRUE, results = TRUE, comment = "## ",
      post = identity,
      ...
    ) {
      exps <- parse(text=code)
      deps <- lapply(exps, deparse)
      deps <- vapply(deps, paste0, "", collapse="\n")
      vals <- if(eval) {
        vals <- lapply(exps, function(x) eval_capt_vis(x, env))
        vals <- vapply(
          vals,
          function(x) if(length(x)) paste0(comment, x, collapse="\n") else "",
          ""
        )
        if(results) vals
      }
      all <- c(deps, vals)[order(rep(seq_along(vals), 2))]
      all <- paste0(all[nzchar(all)], collapse="\n")
      if(is.character(post)) post <- get(post, mode='function')

      simplermarkdown::markdown_block(post(all), language=language, id=id)
} } )()
esc_esc <- function(x) if(is.character(x)) gsub("\033", "\uFFFD", x) else x
```
B

### Browsers Do Not Interpret ANSI CSI SGR Sequences

Over the past few years color has been gaining traction in the R terminal,
particularly since Gábor Csárdi's [crayon](https://github.com/r-lib/crayon)
made it easy to format text with [ANSI CSI SGR
sequences](https://en.wikipedia.org/wiki/ANSI_escape_code).  At the
same time the advent of JJ Alaire and Yihui Xie `rmarkdown` and `knitr`
packages, along with John MacFarlane `pandoc`, made it easy to automatically
incorporate R code and output in HTML documents.

Unfortunately ANSI CSI SGR sequences are not recognized by web browsers and end
up rendering weirdly<a href=#f1><sup>1</sub></a>:

```{.R fun=out_fun post=esc_esc}
sgr.string <- c(
  "\033[43;34mday > night\033[0m",
  "\033[44;33mdawn < dusk\033[0m"
)
writeLines(sgr.string)
```

### Automatically Convert ANSI CSI SGR to HTML

`fansi` provides the `to_html` function which converts the ANSI CSI SGR
sequences and OSC hyperlinks into HTML markup.  When we combine it with
`knitr::knit_hooks` we can modify the rendering of the `rmarkdown` document such
that ANSI CSI SGR encoding is shown in the equivalent HTML.

`fansi::set_knit_hooks` is a convenience function that does just this.  You
should call it in an `rmarkdown` document with the:

  * Chunk option `results` set to "asis".
  * Chunk option `comments` set to "" (empty string).
  * The `knitr::knit_hooks` object as an argument.

The corresponding `rmarkdown` hunk should look as follows:

````
```{r, comment="", results="asis"}
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)
```
````

```{.R comment="" echo=FALSE fun=output_raw}
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)
```
We run this function for its side effects, which cause the output to be
displayed as intended:

```{.R fun=out_fun post=esc_esc}
writeLines(sgr.string)
```

If you are seeing extra line breaks in your output you may need to use:

````
```{r, comment="", results="asis"}
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks, split.nl=TRUE)
```
````

If you use `crayon` to generate your ANSI CSI SGR style strings you may need to
set `options(crayon.enabled=TRUE)`, as in some cases `crayon` suppresses the SGR
markup if it thinks it is not outputting to a terminal.

We can also set hooks for the other types of outputs, and add some additional
CSS styles.

````
```{r, comment="", results="asis"}
styles <- c(
  getOption("fansi.style", dflt_css()),  # default style
  "PRE.fansi CODE {background-color: transparent;}",
  "PRE.fansi-error {background-color: #DDAAAA;}",
  "PRE.fansi-warning {background-color: #DDDDAA;}",
  "PRE.fansi-message {background-color: #AAAADD;}"
)
old.hooks <- c(
  old.hooks,
  fansi::set_knit_hooks(
    knitr::knit_hooks,
    which=c("warning", "error", "message"),
    style=styles
) )
```
````
```{.R comment="" echo=FALSE fun=output_raw}
styles <- c(
  getOption("fansi.style", dflt_css()),  # default style
  "PRE.fansi CODE {background-color: transparent;}",
  "PRE.fansi-error {background-color: #DDAAAA;}",
  "PRE.fansi-warning {background-color: #DDDDAA;}",
  "PRE.fansi-message {background-color: #AAAADD;}"
)
old.hooks <- c(
  old.hooks,
  fansi::set_knit_hooks(
    knitr::knit_hooks,
    which=c("warning", "error", "message"),
    style=styles
) )
```
```{r error=TRUE}
message(paste0(sgr.string, collapse="\n"))
warning(paste0(c("", sgr.string), collapse="\n"))
stop(paste0(c("", sgr.string), collapse="\n"))
```

You can restore the old hooks at any time in your document with:

<!--
```{.R fun=out_fun post=esc_esc, eval=FALSE}
do.call(knitr::knit_hooks$set, old.hooks)
writeLines(sgr.string)
```
-->

See `?fansi::set_knit_hooks` for details.

----
<a name='f1'></a><sup>1</sup>For illustrative purposes we output raw ANSI
CSI SGR sequences in this document.  However, because the ESC control character
causes problems with some HTML rendering services we replace it with the �
symbol.  Depending on the browser and process it would normally not be
visible at all, or substituted with some other symbol.

