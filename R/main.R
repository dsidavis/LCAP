#
# main.R
#
# Testbed for PDF table extraction.


library(XML)
source("find_rectangles.R")

main = function() {
  files = list.files("../xml", "LCAP_2015", full.names = TRUE)
  files = "../xml/Acton-AguaDulceUnified_LCAP_2015.2018.xml"

  ans = lapply(files, function(file) {
    cat(sprintf("File: %s\n", file))
    text = sec2_extract(file)
    sec2_parse(text)
  })
  names(ans) = files

  saveRDS(ans, "foo.rds")
  browser()
}


#' Extract LCAP Section 2 Text
#'
sec2_extract = function(file = "../xml/ABCUnified_LCAP_2015.2018.xml") {
  xml = xmlParse(file)

  # Locate Section 2.
  target = "Section 2:"
  xpath =  sprintf("//page[text[contains(text(), '%s')]]", target)
  sec2 = getNodeSet(xml, xpath) 
  if (length(sec2) == 0) {
    browser()
    stop("Unable to locate 'Section 2:'.")
  }
  browser()

  # Look for the goals table.
  page = getNodeSet(sec2[[1]],
    "following-sibling::page[line and text[contains(text(), 'GOAL')]]"
  )[[1]]
  browser()

  cells = character(0)
  # Process each page into text.
  repeat {
    # Find rectangles.
    lines = getNodeSet(page, ".//line")
    if (length(lines) == 0)
      break
    bbox = bbox_matrix(lines)
    r = find_rectangles(bbox, 2)
    r = r[order(r[, 2], r[, 1]), ]

    # Group text into rectangles.
    texts = getNodeSet(page, ".//text")
    browser()

    idx = vapply(texts, function(text) {
      xy = get_xy(text)
      match(TRUE, in_rect(xy, r), NA_real_)
    }, numeric(1))
    idx = factor(idx, seq_len(nrow(r)))

    texts = tapply(texts, idx, function(grp) {
      paste0(vapply(grp, xmlValue, character(1)), collapse = "\n")
    })

    cells = append(cells, texts)

    page = getSibling(page)
  }

  return (cells)
}


#' Parse LCAP Section 2 Text
#'
sec2_parse = function(text) {
  text[is.na(text)] = ""

  # Split table into goals.
  groups = cumsum(grepl("GOAL +[1-9][0-9]*:", text, ignore.case = TRUE))
  goals = split(text, groups)

  lapply(goals, function(goal) {
    # Split goal into LCAP years.
    years = split(goal, cumsum(grepl("LCAP Year [1-9]:", goal)))
    header = years[[1]]

    # Header
    # ------
    #   GOAL 1 | <goal> | <priorities>
    #   Identified Need: | <need>
    #   Goal Applies to: | Schools: <schools>
    #                    | Subgroups: | <subgroups>
    data = list(
      goal              = header[1:2],
      priorities        = header[3],
      need              = header[4:5],
      applies_schools   = header[7:8],
      applies_subgroups = header[9:10]
    )
    data = lapply(data, paste0, collapse = " ")

    # LCAP Years
    # ----------
    data$years = lapply(years[-1], function(year) {
      #   <year>
      #   EAMO: | <eamo>
      #   Actions/Services | Scope   | Pupils   | Budgeted Expenditures
      #   <actions>        | <scope> | <pupils> | <expend>
      eamo    = paste0(year[2:3], collapse = " ")
      actions = year[-(1:3)]
      year    = year[[1]]

      if (length(actions) %% 4 != 0) {
        browser()
        stop(sprintf("Actions table has %i cells.", length(actions)))
      }
      colnames = chartr("\n", " ", actions[1:4])
      actions = matrix(actions[-(1:4)], ncol = 4, byrow = TRUE,
        dimnames = list(NULL, colnames))

      # Remove empty rows.
      actions = actions[rowSums(actions == "") < 4, ]

      # FIXME: Merge cells that were broken across pages.
      #br = grep("^[0-9]+[.]", actions[, 1], invert = TRUE)
      #if (length(br)) {
      #  actions[br - 1, ] = paste(actions[br - 1, ], actions[br, ], sep = "\n")
      #  actions = actions[-br, ]
      #}

      list(year = year, eamo = eamo, actions = actions)
    })

    return (data)
  })
}


#'
#' Check whether a point falls in a rectangle.
in_rect = function(pt, rect) {
  rect[, 1] < pt[1] & pt[1] < rect[, 3] & rect[, 2] < pt[2] & pt[2] < rect[, 4]
}


get_xy = function(node) {
  xy = xmlAttrs(node)[c("left", "top")]
  as.numeric(xy)
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
  #rects = getNodeSet(page, ".//rect")
  #sapply(rects, function(rect) {
  #  attrs = xmlAttrs(rect)

  #  # left, bottom, right, top
  #  bbox = as.numeric(strsplit(attrs["bbox"], ",")[[1]])
  #  if ( all(is.na(bbox)) )
  #    return (NULL)

  #  lwd = as.numeric(attrs["lineWidth"])
  #  if (is.na(lwd))
  #    lwd = 1.0

  #  rect(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]], lwd = lwd, col = "gray90",
  #    border = "red")
  #})
}
