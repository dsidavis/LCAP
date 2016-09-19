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
  # Remove extremely short lines.
  lines = lines[lines[4, ] - lines[2, ] > tol_join, ]

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
  # Remove extremely short lines.
  lines = lines[lines[3, ] - lines[1, ] > tol_join, ]

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
#' @param tol_x (numeric) The tolerance for joining lines along the x-axis.
#' @param tol_y (numeric) The tolerance for joining lines along the y-axis
#' @param plot (logical) If TRUE, plot cell detection information.
#'
#' @return A matrix of cell (rectangle) coordinates.
lines_to_cells = function(lines, tol_x = 5, tol_y = tol_x, plot = FALSE) {
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
  vt = simplify_lines_vt(lines$vt, tol_line = 2, tol_join = tol_y * 2)
  vt = do.call(rbind, vt)

  if (plot) {
    plot_pdf_lines(do.call(rbind, hz), col = "orange")
    plot_pdf_lines(vt, col = "violet")
  }

  cells = NULL
  connect = matrix(FALSE, nrow(vt), nrow(vt))
  mask = lower.tri(connect, TRUE)
  old_y = rep_len(NA_real_, nrow(vt))

  for (i in seq_along(hz)) {
    hz_a = hz[[i]]
    y = hz_a[1, 2]

    if (plot) abline(h = y, lty = "dashed", col = "gray80")

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

        if (plot)
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

        if (plot)
          points(vt[corners, 1], rep_len(y, length(corners)), cex = 0.5)
      }
    }
    connect[mask] = FALSE
  }
  
  cells = cells[rowSums(is.na(cells)) == 0, ]

  return (cells)
}

  # Add virtual cells.
  #x = unique(as.vector(cells[, c(1, 3)]))
  ##x = cbind(head(x, -1), tail(x, -1))
  #y = unique(as.vector(cells[, c(2, 4)]))
  #ny = length(y) - 1
  #z = cbind(
  #  rep(head(x, -1), each = ny), head(y, -1),
  #  rep(tail(x, -1), each = ny), tail(y, -1),
  #  deparse.level = 0
  #)
  #cells2 = rbind(cells, z)
  #cells2 = cells2[!is_nested(cells2), ]


cells_to_rows = function(cells, tol = 5) {
  # NOTE: the row detection below might fail if there are nested cells.
  cells = cells[!is_nested(cells), ]

  cells = cells[order(cells[, 2], cells[, 1], decreasing = FALSE), ]

  # Rows are determined by the heights of the leftmost cells.
  is_leftmost = cells[, 1] <= min(cells[, 1]) + tol
  row_id = findInterval(cells[, 2], cells[is_leftmost, 2])

  cells = cbind(cells, row_id)

  return (cells)
}

which_segment = function(x, segments, tol) {
  tol = tol * rep_len(c(-1, 1), length(segments))
  idx = findInterval(x, as.vector(t(segments)) + tol)

  idx[(idx %% 2) != 1] = NA
  idx = (idx + 1) / 2

  return (idx)
}


# NOTE: For duplicated rectangles, both are marked as nested.
is_nested = function(rects) {
  rects_lb = t(rects[, 1:2])
  rects_rt = t(rects[, 3:4])

  vapply(seq_len(nrow(rects)), function(i) {
    any(
      pt_in_rects(rects[i, 1:2], rects)[-i] &
      pt_in_rects(rects[i, 3:4], rects)[-i]
    )
  }, logical(1))
}


pt_in_rects = function(x, rects, open_rb = FALSE) {
  `%<%` = if (open_rb) `<` else `<=`
  return (
    rects[, 1] <= x[1] & x[1] %<% rects[, 3] & # x inside
    rects[, 2] <= x[2] & x[2] %<% rects[, 4]   # y inside
  )
}


which_rect = function(x, rects) {
  apply(x, 1, function(x_) {
    match(TRUE, pt_in_rects(x_, rects, open_rb = TRUE))
  })
}
#  rects_lb = t(rects[, 1:2])
#  rects_rt = t(rects[, 3:4])
#
#  apply(x, 1, function(r) {
#    match(TRUE, .in_rects(r, rects_lb, rects_rt))
#  })
#}


.in_rects = function(pt, rects_lb, rects_rt) {
  colSums(rects_lb <= pt & pt < rects_rt) == 2
}


# UNUSED:
in_interval = function(a, x, b) {
  (findInterval(x, as.vector(rbind(a, b))) %% 2) == 1
}


# UNUSED:
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
