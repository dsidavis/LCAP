#
# plot_geometry.R
#
# Functions for plotting PDF geometry.


#' Plot Lines And Rects
#'
#' This function plots the lines and rects from one page of an XML'd PDF.
#'
#' @param page A page from an XML'd PDF.
plot_page = function(page) {
  attrs = xml_attrs(page)
  xlim = as.numeric(attrs[c("left", "width")])
  ylim = as.numeric(attrs[c("height", "top")])

  plot.new()
  plot.window(xlim, ylim, asp = 1)
  axis(1)
  axis(2)

  # Plot all lines.
  lines = xml_find_all(page, "./line")
  sapply(lines, function(line) {
    attrs = xml_attrs(line)

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
  rects = xml_find_all(page, "./rect")
  sapply(rects, function(rect) {
    attrs = xml_attrs(rect)

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
