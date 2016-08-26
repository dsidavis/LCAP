#
# find_rectangles.R
#
# Functions for manipulating lines in PDFs.


#' Find Rectangles Enclosed By Lines
#'
#' @param lines (numeric matrix) A bounding box matrix for the lines.
#' @param xtol (numeric)
#' @param ytol (numeric)
#'
#' @return A bounding box matrix for the rectangles.
find_rectangles = function(lines, xtol = 5, ytol = xtol) {
  # Separate the lines into verticals and horizontals.
  is_hz = abs(lines[, 2] - lines[, 4]) == 0
  hz = lines[is_hz, ]
  vt = lines[!is_hz, ]

  hz = hz[order(hz[, 2], hz[, 1]), ]
  vt = vt[order(vt[, 1], vt[, 2]), ]

  regions = list()
  y_below  = numeric(nrow(vt))

  for ( i in seq_len(nrow(hz)) ) {
    # Ignore verticals outside the scanline's x-interval.
    active = hz[i, 1] - xtol < vt[, 1] & vt[, 1] < hz[i, 3] + xtol
    if (!any(active))
      next
    vt_a = vt[active, , drop = FALSE]

    # Find verticals that end on or intersect the scanline.
    y = hz[i, 2]
    bot = y - ytol < vt_a[, 4] & vt_a[, 4] < y + ytol
    mid = vt_a[, 2] < y & y < vt_a[, 4]

    # Check for completed rectangles.
    ends = bot | mid
    if (sum(ends) > 1) {
      x = vt_a[ends, 1]
      ybottom = head(y_below[active][ends], -1)

      regions[[length(regions) + 1]] = 
        cbind(head(x, -1), ybottom, tail(x, -1), y, deparse.level = 0)
    }

    # Mark the scanline.
    y_below[active] = y
  }

  return (do.call(rbind, regions))
}


#' Get Bounding Boxes
#'
#' This function gets the bounding boxes for all nodes in a nodeset.
#'
#' @param nodeset
#'
#' @return
bbox_matrix = function(nodeset) {
  bbox_mat =
    vapply(nodeset, function(node) {
      bbox = xmlGetAttr(node, "bbox")
      if (is.null(bbox))
        return (rep_len(NA_real_, 4))

      as.numeric(strsplit(bbox, ",")[[1]])
    }, numeric(4))

  bbox_mat = t(bbox_mat)
  
  # Ensure bottom is less than top.
  #swap = bbox_mat[, 2] > bbox_mat[, 4]
  #tmp = bbox_mat[swap, 2]
  #bbox_mat[swap, 2] = bbox_mat[swap, 4]
  #bbox_mat[swap, 4] = tmp

  return (bbox_mat)
}
