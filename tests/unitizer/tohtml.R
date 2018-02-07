library(unitizer)

unitizer_sect('colors', {
  style <- "width: 16px; height: 16px; display: inline-block;"
  span <- '<span style="background-color: %s; %s"></span>'

  colors.8 <- fansi:::esc_color_code_to_html(rbind(c(0:7, 9), 0L, 0L, 0L, 0L))
  colors.8

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
  #   split(head(tail(cells, -16), 216), rep(1:6, each=36)),
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
  #     '<div>', paste0(cells[1:16], collapse=""), '</div>',
  #     cells.255.color,
  #     '<div>', paste0(cells[(256-23):256], collapse=""), '</div>',
  #     '<h3>True Color</h3>',
  #     cells.tru.rows,
  #     '</html>'
  #   ),
  #   tmp
  # )
  # browseURL(tmp)
})
