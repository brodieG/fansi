## Special thank-you to @mikefc for the magick tutorial [1]

suppressPackageStartupMessages(library(magick))
im <- image_read("~/Downloads/Rlogo.png")  # from [2]
geom <- geometry_size_pixels(width=40, height=20, preserve_aspect=FALSE)
ims <- image_scale(im, geometry=geom)

## Scale to 6^3 colors as that is what ANSI SGR 256 supports.  Assuming channel
## 4 is alpha and background is white, blend in alpha into the three other
## channels

clrs.raw <- as.integer(ims[[1]])
alpha <- array((255 - clrs.raw[,,4])/255, dim=c(dim(clrs.raw[,,1:3])))
clrs <- round((clrs.raw[,,1:3] * (1 - alpha) + 255 * alpha) / 255 * 5)

## Generate the ANSI CSI SGR, use spaces with background colors to represent
## pixels

sgr.256.raw <- apply(
  clrs, 1:2,
  function(x) sprintf("\033[48;5;%dm \033[m", x[3] + x[2] * 6 +  x[1] * 36 + 16)
)
sgr.256 <- do.call(
  paste0,
  c(
    as.data.frame(sgr.256.raw),                # logo
    as.list(rep("\033[48;5;231m \033[m", 20))  # padding
) )
## Scroll the text by taking the first column of text and moving it to the end,
## and repeat indefinitely.  CTRL + C to exit.

library(fansi)
scrolling_banner <- function(x) {
  repeat {
    for(i in seq_len(nchar_sgr(x[1]) + 1)) {
      writeLines(c("", paste0("\r  ", x)))
      x <- paste0(
        substr_sgr(x, nchar_sgr(x), nchar_sgr(x)),
        substr_sgr(x, 1, nchar_sgr(x) - 1)
      )
      cat(sprintf("\033[%dA", length(x) + 1))
      Sys.sleep(0.05)
    }
  }
}
stop("scrolling banner loaded!")
## scrolling_banner(sgr.256)

## [1]: https://coolbutuseless.github.io/2018/09/27/creating-nonograms-with-nonogram-and-magick-packages/
## [2]: https://www.r-project.org/logo/
