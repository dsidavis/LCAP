#
# plot_geometry.R
#
# Functions for plotting PDF geometry.

library(xml2)


plot_pdf_lines = function(x, lty = 1, col = 1, ...) {
  tx = t(x)
  matlines(tx[c(1, 3), ], tx[c(2, 4), ], lty = lty, col = col, ...)
}


plot_pdf_rects = function(x, ...) {
  rect(x[, 1], x[, 2], x[, 3], x[, 4], ...)
}


plot_pdf_new = function(page, ...) {
  attrs = xml_attrs(page)
  xlim = as.numeric(attrs[c("left", "width")])
  ylim = as.numeric(attrs[c("height", "top")])

  plot.new()
  plot.window(xlim, ylim, asp = 1)
  axis(1)
  axis(2)
}


#' Plot PDF Page Geometry
#'
#' This function plots the geometry in a page node from an XML-converted PDF.
#'
#' @param page A page node from an XML-converted PDF.
plot_pdf_page = function(page, ...) {
  plot_pdf_new(page)

  # Plot all lines.
  lines = xml_find_all(page, "./line")
  lines = pdf_bbox(lines)
  plot_lines(lines)

  # Plot all rects.
  rects = xml_find_all(page, "./rect")
  rects = pdf_bbox(rects)
  rect(rects[, 1], rects[, 2], rects[, 3], rects[, 4], border = "red")
}
