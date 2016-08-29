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

  regions = NULL
  y_below  = numeric(nrow(vt))

  for ( i in seq_len(nrow(hz)) ) {
    points(hz[i, 1], hz[i, 2], cex = 1, pch = 8)
    # Ignore verticals outside the scanline's x-interval.
    active = hz[i, 1] - xtol < vt[, 1] & vt[, 1] < hz[i, 3] + xtol
    if (!any(active))
      next
    vt_a = vt[active, , drop = FALSE]

    # Find verticals that start on, intersect, or end on the scanline.
    y = hz[i, 2]
    top = y - ytol < vt_a[, 2] & vt_a[, 2] < y + ytol
    mid = vt_a[, 2] < y & y < vt_a[, 4]
    bot = y - ytol < vt_a[, 4] & vt_a[, 4] < y + ytol

    # FIXME:
    points(vt_a[top, 1], rep_len(y, sum(top)), cex = 4, pch = 6)
    points(vt_a[mid, 1], rep_len(y, sum(mid)), cex = 4, pch = 5)
    points(vt_a[bot, 1], rep_len(y, sum(bot)), cex = 4, pch = 2)

    # Check for completed rectangles.
    ends = bot | mid
    if (sum(ends) > 1) {
      ends = which(ends)
      x = vt_a[ends, 1]
      ybottom = y_below[active][head(ends, -1)]

      regions = cbind(regions,
        rbind(head(x, -1), ybottom, tail(x, -1), y, deparse.level = 0)
      )

      browser()
      y_below[active][head(ends, -1)] = y

      # FIXME:
      rect(regions[1, ], regions[2, ], regions[3, ], regions[4, ],
        border = "orange", lwd = 2)
    }

    y_below[active][top] = y

    # This should only be set when a line starts or a box is closed.
    # Mark the scanline.
    #y_below[active] = y
  }

  if (!is.null(regions))
    regions = t(regions)

  return (regions)
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
      tag = xml_name(node)

      if (tag %in% c("line", "rect")) {
        bbox = xml_attr(node, "bbox")
        bbox = as.numeric(strsplit(bbox, ",")[[1]])

      } else if (tag == "text") {
        bbox = xml_attrs(node)[c("left", "top", "width", "height")]
        bbox = as.numeric(bbox)
        bbox[3:4] = bbox[3:4] + bbox[1:2]

      } else {
        stop(sprintf("Cannot get bbox for node '%s'.\n", tag))
      }

      return (bbox)
    }, numeric(4))

  bbox_mat = t(bbox_mat)
  
  # Make sure left <= right and bottom <= top (despite plotting top-down).
  to_swap = bbox_mat[, 1] > bbox_mat[, 3]
  tmp = bbox_mat[to_swap, 1]
  bbox_mat[to_swap, 1] = bbox_mat[to_swap, 3]
  bbox_mat[to_swap, 3] = tmp

  to_swap = bbox_mat[, 2] > bbox_mat[, 4]
  tmp = bbox_mat[to_swap, 2]
  bbox_mat[to_swap, 2] = bbox_mat[to_swap, 4]
  bbox_mat[to_swap, 4] = tmp

  return (bbox_mat)
}
