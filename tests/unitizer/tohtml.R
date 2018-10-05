library(unitizer)
library(fansi)

unitizer_sect('colors', {
  style <- "width: 16px; height: 16px; display: inline-block;"
  span <- '<span style="background-color: %s; %s"></span>'

  colors.8 <- fansi:::esc_color_code_to_html(rbind(c(0:7), 0L, 0L, 0L, 0L))
  colors.8

  # error
  fansi:::esc_color_code_to_html(matrix(c(9L, 0L, 0L, 0L, 0L)))

  colors.255 <- fansi:::esc_color_code_to_html(rbind(8L, 5L, 0:255, 0L, 0L))
  colors.255

  # Small sampling of tru color colors

  vals <- c(0L, 127L, 255L)
  colors.tru <- fansi:::esc_color_code_to_html(
    do.call(rbind, c(list(8L, 2L), expand.grid(vals, vals, vals)))
  )
  colors.tru

  # ## The following is some code to display all the colors in an HTML page for
  # ## review

  # cells.8 <- sprintf(span, colors.8, style)

  # cells.255 <- sprintf(span, colors.255, style)
  # cells.255.color <- sapply(
  #   split(head(tail(cells.255, -16), 216), rep(1:6, each=36)),
  #   function(x) sprintf('<div>%s</div>', paste0(x, collapse=""))
  # )

  # vals <- as.integer(255 / 15 * 0:15)
  # vals.tru.raw <- expand.grid(vals, vals, vals)
  # vals.tru.raw <- vals.tru.raw[with(vals.tru.raw, order(Var1, Var2, Var3)),]
  # vals.tru.mx <- do.call(rbind, c(list(8L, 2L), vals.tru.raw))
  # colors.tru <- fansi:::esc_color_code_to_html(vals.tru.mx)
  # cells.tru <- sprintf(span, colors.tru, style)

  # cells.tru.rows <- sapply(
  #   split(cells.tru, rep(1:64, each=64)),
  #   function(x) sprintf('<div>%s</div>', paste0(x, collapse=""))
  # )
  # tmp <- tempfile()
  # writeLines(
  #   c(
  #     '<html>',
  #     '<h3>8 colors</h3>',
  #     '<div>', paste0(cells.8, collapse=""), '</div>',
  #     '<h3>255 colors</h3>',
  #     '<div>', paste0(cells.255[1:16], collapse=""), '</div>',
  #     cells.255.color,
  #     '<div>', paste0(cells.255[(256-23):256], collapse=""), '</div>',
  #     '<h3>True Color</h3>',
  #     cells.tru.rows,
  #     '</html>'
  #   ),
  #   tmp
  # )
  # browseURL(tmp)
})
unitizer_sect("simple html conversion", {
  as_html_page <- function(x) {
    # note this will clutter temp directory, but needed so we can examine source
    tmp <- tempfile()
    writeLines(c("<html><pre>", as.character(x), "</pre></html>"), tmp)
    browseURL(tmp)
  }
  sgr_to_html("hello \033[31;42;1mworld\033[0m")
  sgr_to_html("hello \033[31;48;5;23;1mworld\033[m")

  # this turned out to be a good corner case, italic is not actually
  # italicized

  sgr_to_html(
    "\033[1mbold\033[22m \033[2mfaint\033[22m \033[mitalic\033[24m\n"
  )
  # similarly, we mistakenly seem to have thought below that 24 turns off
  # italic, when it actually doesn't.

  csi_string <- c(
    "\033[1mbold\033[22m \033[2mfaint\033[22m \033[3mitalic\033[24m",
    "\033[4munderline\033[24m \033[5mslow-blink\033[25m",
    "\033[6mfast-blink\033[25m",
    "\033[31;42mred-fg-green-bg\033[7minverse \033[7minverse-off\033[39;49m",
    "\033[8mconceal\033[28m reveal \033[9mcrossed-out\033[29mclear\033[m",
    "\033[1;41mbold\033[22m \033[2;42mfaint\033[22m \033[3;43mitalic\033[23m",
    "\033[4;44munderline\033[24m \033[5;45mslow-blink\033[25m",
    "\033[6;46mfast-blink\033[25m",
    "\033[31;42mred-fg-green-bg\033[7minverse \033[7minverse-off\033[39;49m",
    "\033[8mconceal\033[28m reveal \033[9mcrossed-out\033[29mclear\033[m",
    "\033[3mitalic again\033[24m not italic?\033[m"
  )
  html_string <- sgr_to_html(csi_string)
  html_string
  # tmp <- tempfile()
  # writeLines(c("<html><pre>", html_string, "</pre></html>"))
})
unitizer_sect("Bright Colors", {
  sgr_to_html("hello\033[94;101m world\033[39m yow\033[49mza")
  # oob color
  sgr_to_html("hello\033[98;101m world\033[39m yow\033[49mza")
})
unitizer_sect("Corner cases", {
  sgr_to_html("hello\033[0m")
  sgr_to_html("hello\033[31m")

  # A string that shrinks; multiple repeated SGRs reduced to a single span

  sgrs <- paste0(rep("\033[31m", 20), collapse="")
  sgr_to_html(sprintf("%shello world\033[m", sgrs))

  # non character inputs

  sgr_to_html(1:3)



})
unitizer_sect("Bad inputs", {
  fansi:::esc_color_code_to_html(matrix(1:12, 4))

  sgr_to_html(1:3)
  sgr_to_html("a", warn=1:3)
  sgr_to_html("a", term.cap=1:3)
  sgr_to_html("a", term.cap="hello")
})
unitizer_sect("issue54", {
  string <- c("\033[31m", "\033[39m")
  fansi::sgr_to_html(string)

  string1 <- c("\033[31mhello", "world\033[39m moon")
  fansi::sgr_to_html(string1)

  string2 <- c("\033[3mhello\033[24m", "world\033[23m moon")
  fansi::sgr_to_html(string2)

})
