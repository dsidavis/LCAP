#
# main.R
#
# Testbed for PDF table extraction.


# FIXME:
library(XML)
source("find_rectangles.R")

main = function() {
  xml = xmlParse("../xml/ABCUnified_LCAP_2015.2018.xml")

  page = getNodeSet(xml, "//page/text[contains(text(), 'GOAL 1:')]/..")[[1]]
  browser()

  lines = getNodeSet(page, ".//line")
  bbox = bbox_matrix(lines)

  r = find_rectangles(bbox, 2)

  plot_page(page)
  rect(r[, 1], r[, 2], r[, 3], r[, 4], lwd = 4, border = "orange")

  browser()
}


#' Plot Lines And Rects
#'
#' This function plots the lines and rects from one page of an XML'd PDF.
#'
#' @param page A page from an XML'd PDF.
plot_page = function(page) {
  attrs = xmlAttrs(page)
  xlim = as.numeric(attrs[c("left", "width")])
  ylim = as.numeric(attrs[c("height", "top")])

  plot.new()
  plot.window(xlim, ylim, asp = 1)
  axis(1)
  axis(2)

  # Plot all lines.
  lines = getNodeSet(page, ".//line")
  sapply(lines, function(line) {
    attrs = xmlAttrs(line)

    # left, bottom, right, top
    bbox = as.numeric(strsplit(attrs["bbox"], ",")[[1]])
    if ( all(is.na(bbox)) )
      return (NULL)

    lwd = as.numeric(attrs["lineWidth"])
    if (is.na(lwd))
      lwd = 1.0

    lty = if ("dashes" %in% names(attrs)) "dashed" else "solid"
    #stroke = as.numeric(strsplit(attrs["stroke.color"], ",")[[1]])
    #stroke = do.call("rgb", as.list(stroke))

    col = "black"
    hlen = abs(bbox[3] - bbox[1])
    if (0 < hlen && hlen < 20) {
      col = "blue"
      lwd = 10
    }
    lines(bbox[c(1, 3)], bbox[c(2, 4)], lwd = lwd, lty = lty, col = col)
  })

  # Plot all rects.
  rects = getNodeSet(page, ".//rect")
  sapply(rects, function(rect) {
    attrs = xmlAttrs(rect)

    # left, bottom, right, top
    bbox = as.numeric(strsplit(attrs["bbox"], ",")[[1]])
    if ( all(is.na(bbox)) )
      return (NULL)

    lwd = as.numeric(attrs["lineWidth"])
    if (is.na(lwd))
      lwd = 1.0

    rect(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]], lwd = lwd, col = "gray90",
      border = "red")
  })
}
