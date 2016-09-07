#
# geometry.R
#
# Functions for working with rects and lines.


#' Simplify Lines
#'
simplify_lines = function(lines, xtol = 10) {
  lines = split_lines_hv(lines)
  
  # TODO: Group lines within some tolerance.
  #hz = hz[order(hz[, 2]), ]
  #groups = cumsum(c(0, diff(hz[, 2])) > tol)

  # Join overlapping horizontals.
  hz = lines$hz
  hz = tapply(seq_len(nrow(hz)), hz[, 2], function(group) {
    h = hz[group, , drop = FALSE]
    if (nrow(h) < 2)
      return (h)

    x = join_colinear_segments(h[, 1], h[, 3], xtol)
    y = h[1, 2]

    cbind(x[, 1], y, x[, 2], y, deparse.level = 0)
  })

  # Join overlapping verticals.
  vt = lines$vt
  vt = tapply(seq_len(nrow(vt)), vt[, 1], function(group) {
    v = vt[group, , drop = FALSE]
    if (nrow(v) < 2)
      return (v)

    y = join_colinear_segments(v[, 2], v[, 4])
    x = v[1, 1]

    cbind(x, y[, 1], x, y[, 2], deparse.level = 0)
  })

  list(hz = hz, vt = vt)
}


join_colinear_segments = function(begins, ends, tol = 0) {
  ord = order(begins, ends, decreasing = FALSE)
  n = length(ord)

  first = begins[ord[1]]
  begins = begins[ord[-1]]

  last = ends[ord[n]]
  ends = ends[ord[-n]]

  breaks = ends < begins - tol

  matrix(c(first, begins[breaks], ends[breaks], last), ncol = 2)
}


#' Split Lines Into Horizontals & Verticals
#'
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
#' @param x (numeric) A matrix of rect coordinates (left, bottom, right, top).
#'
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


lines_to_cells = function(lines, xtol = 5, ytol = xtol) {
  if (!is(lines, "list")) {
    if (is(lines, "matrix"))
      lines = simplify_lines(lines)
    else
      stop("Argument `lines` must be a list or matrix.")
  }

  hz = lines$hz
  vt = do.call(rbind, lines$vt)

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
    abline(h = y, lty = "dashed", col = "gray80")
    points(vt_a[top, 1], rep_len(y, sum(top)), cex = 1, pch = 6)
    points(vt_a[mid, 1], rep_len(y, sum(mid)), cex = 1, pch = 5)
    points(vt_a[bot, 1], rep_len(y, sum(bot)), cex = 1, pch = 2)

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


