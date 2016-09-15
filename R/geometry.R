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

  hz = simplify_lines_hz(lines$hz, tol_line = 5, tol_join = xtol * 2)
  vt = do.call(rbind, simplify_lines_vt(lines$vt, tol_line = 5, tol_join = 1))

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
    #points(vt_a[top, 1], rep_len(y, sum(top)), cex = 0.5, pch = 6)
    #points(vt_a[mid, 1], rep_len(y, sum(mid)), cex = 0.5, pch = 5)
    #points(vt_a[bot, 1], rep_len(y, sum(bot)), cex = 0.5, pch = 2)

    # Check for completed rectangles.
    ends = (bot | mid) & !top
    if (sum(ends) > 1) {
      ends = which(ends)
      x = vt_a[ends, 1]
      ybottom = y_below[active][head(ends, -1)]

      regions = cbind(regions,
        rbind(head(x, -1), ybottom, tail(x, -1), y, deparse.level = 0)
      )

      browser() 

      # FIXME:
      rect(regions[1, ], regions[2, ], regions[3, ], regions[4, ],
        border = "black", lty = "dotted", col = "gray80")

      y_below[active][head(ends, -1)] = y
    }

    y_below[active][top] = y
  }

  if (!is.null(regions))
    regions = t(regions)

  return (regions)
}


lines_to_cells2 = function(lines, tol_x = 2, tol_y = tol_x) {
  # Ensure there are horizontals at the top and bottom of the page.
  far_lt = min(lines[, 1])
  far_tp = min(lines[, 2])
  far_rt = max(lines[, 3])
  far_bt = max(lines[, 4])
  lines = rbind(lines,
    c(far_lt, far_tp, far_rt, far_tp), # top
    c(far_lt, far_bt, far_rt, far_bt)  # bottom
  )

  lines = split_lines_hv(lines)

  hz = simplify_lines_hz(lines$hz, tol_line = 5, tol_join = tol_x * 2)
  vt = do.call(rbind, simplify_lines_vt(lines$vt, tol_line = 5, tol_join = 1))

  plot_pdf_lines(do.call(rbind, hz), col = "orange")
  plot_pdf_lines(vt, col = "violet")

  cells = NULL
  connect = matrix(FALSE, nrow(vt), nrow(vt))
  mask = lower.tri(connect, TRUE)
  old_y = rep_len(NA_real_, nrow(vt))

  for (i in seq_along(hz)) {
    hz_a = hz[[i]]
    y = hz_a[1, 2]

    abline(h = y, lty = "dashed", col = "gray80")

    # Select Verticals
    # ----------------
    a = seq_len(nrow(vt))

    in_y = y - tol_y <= vt[a, 4] &  # sweep starts before vertical ends
           vt[a, 2] <= y + tol_y    # vertical starts before sweep ends
    if (!any(in_y))
      next
    a = a[in_y]

    which_x = which_segment(vt[a, 1], hz_a[, c(1, 3)], tol_x)
    in_x = !is.na(which_x)
    if (!any(in_x))
      next
    which_x = which_x[in_x]
    a = a[in_x]

    # Detect Rectangles
    # -----------------
    # FIXME:
    top = y - tol_y <= vt[a, 2]  # sweep starts before vertical starts
    bot = vt[a, 4] <= y + tol_y  # vertical ends before sweep ends
    mid = !(top | bot)

    # TODO: combine loops into one loop over segments.

    # Close a rectangle for each pair of mid | bot.
    end = mid | (bot & !top)
    corners_set = split(a[end], which_x[end])
    for (corners in corners_set) {
      if (length(corners) > 1) {
        lt  = head(corners, -1)
        is_con = apply(connect[lt, corners, drop = FALSE], 1,
          function(r) match(TRUE, r)
        )
        rt = corners[is_con]

        regions = cbind(vt[lt, 1], old_y[lt], vt[rt, 3], y, deparse.level = 0)
        cells = rbind(cells, regions)

        rect(regions[, 1], regions[, 2], regions[, 3], regions[, 4],
          border = "green")
      }
    } 

    # Open a rectangle for each pair of top | mid.
    begin = mid | (top & !bot)
    corners_set = split(a[begin], which_x[begin])
    for (corners in corners_set) {
      if (length(corners) > 1) {
        lt = head(corners, -1)
        connect[lt, corners] = TRUE
        old_y[lt] = y

        points(vt[corners, 1], rep_len(y, length(corners)), cex = 0.5)
      }
    }
    connect[mask] = FALSE
  }

  return (cells[rowSums(is.na(cells)) == 0, ])
}


which_segment = function(x, segments, tol) {
  tol = tol * rep_len(c(-1, 1), length(segments))
  idx = findInterval(x, as.vector(t(segments)) + tol)

  idx[(idx %% 2) != 1] = NA
  idx = (idx + 1) / 2

  return (idx)
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


pt_in_rect = function(pt, rect) {
  rect[1:2] <= pt & pt <= rect[3:4]
}

contains_rect = function(rects) {
  rect_lt = t(rects[, 1:2])
  rect_rb = t(rects[, 3:4])

  n = nrow(rects)
  r_in_c = vapply(seq_len(n), function(i) {
    inner = colSums(rect_lt <= rects[i, 1:2] & rects[i, 3:4] <= rect_rb) == 2
    inner[i] = FALSE
    return (inner)
  }, logical(n))
  idx = which(r_in_c, arr.ind = TRUE)

  browser()
}
