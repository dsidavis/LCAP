#
# geometry.R
#
# Functions for working with rects and lines.


#' Simplify Vertical Lines
#'
#' @param lines (numeric) A matrix of vertical line coordinates.
#' @param tol_line (numeric) The tolerance for determining colinearity.
#' @param tol_join (numeric) The tolerance for joining colinear segments.
#'
#' @return (list) A list of matrices, one for each set of colinear segments.
simplify_lines_vt = function(lines, tol_line = 0, tol_join = 0) {
  # Group lines by horizontal distance.
  lines = lines[order(lines[, 1]), ]
  groups = cumsum( c(FALSE, diff(lines[, 1]) > tol_line) )
  indexes = split(seq_along(groups), groups)

  # Merge overlapping lines within each group.
  lines = lapply(indexes, function(i) {
    ll = lines[i, , drop = FALSE]
    if (nrow(ll) < 2)
      return (ll)

    x = mean(ll[, 1])
    y = union_segments(ll[, 2], ll[, 4], tol_join)

    cbind(x, y[, 1], x, y[, 2], deparse.level = 0)
  })

  return (lines)
}


#' Simplify Horizontal Lines
#'
#' @param lines (numeric) A matrix of horizontal line coordinates.
#' @param tol_line (numeric) The tolerance for determining colinearity.
#' @param tol_join (numeric) The tolerance for joining colinear segments.
#'
#' @return (list) A list of matrices, one for each set of colinear segments.
simplify_lines_hz = function(lines, tol_line = 0, tol_join = 0) {
  # Group lines by vertical distance.
  lines = lines[order(lines[, 2]), ]
  groups = cumsum( c(FALSE, diff(lines[, 2]) > tol_line) )
  indexes = split(seq_along(groups), groups)

  # Merge overlapping lines within each group.
  lines = lapply(indexes, function(i) {
    ll = lines[i, , drop = FALSE]
    if (nrow(ll) < 2)
      return (ll)

    y = mean(ll[, 2])
    x = union_segments(ll[, 1], ll[, 3], tol_join)

    cbind(x[, 1], y, x[, 2], y, deparse.level = 0)
  })

  return (lines)
}


union_segments = function(begins, ends, tol = 0) {
  ord = order(begins, ends, decreasing = FALSE)
  n = length(ord)

  begins = begins[ord]
  ends = ends[ord]

  seg = numeric(n)
  seg[1] = begins[1]
  j = 2

  mark = ends[1]
  for (i in seq_len(n - 1) + 1) {
    begin = begins[i]
    end = ends[i]

    if (mark + tol < begin) {
      seg[j] = mark
      seg[j + 1] = begin
      j = j + 2
      mark = end
    } else if (mark < end) {
      mark = end
    }
  }
  seg[j] = mark

  matrix(seg[1:j], ncol = 2, byrow = TRUE)
}


#' Split Lines Into Horizontals & Verticals
#'
#' @param (numeric) A matrix of line coordinates.
#'
#' @return (list) A list which contains a matrix of horizontal lines and a
#' matrix of vertical lines.
split_lines_hv = function(lines, tol = 0) {

  ht = lines[, 4] - lines[, 2] 
  is_hz = 0 - tol <= ht & ht <= 0 + tol

  wd = lines[, 3] - lines[, 1] 
  is_vt = 0 - tol <= wd & wd <= 0 + tol

  hz = lines[is_hz & !is_vt, ]
  vt = lines[!is_hz & is_vt, ]

  list(hz = hz, vt = vt)
}


#' Convert Rects To Lines
#'
#' @param x (numeric) A matrix of rect coordinates.
#' 
#' @return (numeric) A matrix of line coordinates.
rects_to_lines = function(rects) {
  n = nrow(rects)
  if (is.null(n)) {
    rects = matrix(rects, 1, 4)
    n = 1
  }

  lines = vapply(seq_len(n), function(i) {
    rects[i, c(
      1, 2, 1, 4, # left
      1, 2, 3, 2, # bottom
      3, 2, 3, 4, # right
      1, 4, 3, 4  # top
    )]
  }, numeric(16))

  matrix(lines, ncol = 4, byrow = TRUE)
}


#' Convert Lines To Cells
#'
#' @param lines (numeric) A matrix of line coordinates.
#' @param xtol (numeric) The tolerance for TODO
#' @param ytol (numeric) The tolerance for TODO
#'
#' @return A matrix of cell (non-overlapping rectangle) coordinates.
lines_to_cells = function(lines, xtol = 5, ytol = xtol) {
  lines = split_lines_hv(lines)

  hz = simplify_lines_hz(lines$hz, tol_line = 0, tol_join = xtol * 2)
  vt = do.call(rbind, simplify_lines_vt(lines$vt, tol_line = 0))

  regions = NULL
  y_below = numeric(nrow(vt))

  for (hz_a in hz) {
    # Ignore verticals outside the scanline's x-intervals.
    active = in_interval(hz_a[, 1] - xtol, vt[, 1], hz_a[, 3] + xtol)
    if (!any(active))
      next
    vt_a = vt[active, , drop = FALSE]

    # Find verticals that start on, intersect, or end on the scanline.
    y = hz_a[1, 2]
    top = y - ytol < vt_a[, 2] & vt_a[, 2] < y + ytol
    mid = vt_a[, 2] < y & y < vt_a[, 4]
    bot = y - ytol < vt_a[, 4] & vt_a[, 4] < y + ytol

    # FIXME:
    #abline(h = y, lty = "dashed", col = "gray80")
    points(vt_a[top, 1], rep_len(y, sum(top)), cex = 0.5, pch = 6)
    points(vt_a[mid, 1], rep_len(y, sum(mid)), cex = 0.5, pch = 5)
    points(vt_a[bot, 1], rep_len(y, sum(bot)), cex = 0.5, pch = 2)

    # Check for completed rectangles.
    ends = (bot | mid) & !top
    if (sum(ends) > 1) {
      ends = which(ends)
      x = vt_a[ends, 1]
      ybottom = y_below[active][head(ends, -1)]

      regions = cbind(regions,
        rbind(head(x, -1), ybottom, tail(x, -1), y, deparse.level = 0)
      )

      y_below[active][head(ends, -1)] = y

      # FIXME:
      rect(regions[1, ], regions[2, ], regions[3, ], regions[4, ],
        border = "orange", lty = "dotted")
    }

    y_below[active][top] = y
  }

  browser()

  if (!is.null(regions))
    regions = t(regions)

  return (regions)
}


in_interval = function(a, x, b) {
  (findInterval(x, as.vector(rbind(a, b))) %% 2) == 1
}


# Approximately 5x slower than in_interval().
in_interval2 = function(a, x, b, include_ends = FALSE) {
  if (include_ends) {
    op1 = `<=`
    op2 = `>=`
  } else {
    op1 = `<`
    op2 = `>`
  }

  colSums( outer(a, x, op1) & outer(b, x, op2) ) > 0
}


#' Find Rectangles Enclosed By Lines
#'
#' @param lines (numeric matrix) A bounding box matrix for the lines.
#' @param xtol (numeric)
#' @param ytol (numeric)
#'
#' @return A bounding box matrix for the rectangles.
find_rectangles = function(lines, xtol = 5, ytol = xtol) {
  lines = split_lines_hv(lines)
  hz = lines$hz
  vt = lines$vt

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


