#
# main.R
#
# Testbed for PDF table extraction.


library(xml2)
library(stringr)
source("find_rectangles.R")

main = function() {
  files = list.files("../xml", "LCAP_2015", full.names = TRUE)
  #files = files[-c(36, 698, 764)]

  # FIXME: tables broken across pages do not have bottom lines
  #files = "../xml/Acton-AguaDulceUnified_LCAP_2015.2018.xml"

  # FIXME: lots of tiny rectangles, no lines
  #files = "../xml/AdelantoElementary_LCAP_2015.2018.xml"

  # FIXME:
  files = "../xml/Alameda_AlamedaUnified_LCAP_2015.2016.xml"

  # Memory
  # ======
  # FIXME: ../xml/Amador_AmadorCountyUnified_LCAP_2015.2016.xml
  # FIXME: ../xml/SpreckelsUnionElementary_LCAP_2015.2016.xml
  # FIXME: ../xml/WeaverUnion_LCAP_2015.2016.xml

  #ans = lapply(files, function(file) {
  for (file in files) {
    cat(sprintf("File: %s\n", file))

    xml = xml2::read_xml(file)
    first = sec2_locate_first(xml)
    last = sec2_locate_last(xml)
    cat(sprintf("  Found %i-%i.\n", first, last))
    
    pages = xml_find_all(xml, sprintf(
      "//page[%s <= @number and @number <= %s]"
      , first
      , last
    ))

    plot_page(pages[[1]])

    browser()
    lines = xml_find_all(pages[[1]], "./line")
    cells = find_rectangles(bbox_matrix(lines), 10)
    rect(cells[, 1], cells[, 2], cells[, 3], cells[, 4], lwd = 2,
      border = "orange")
    browser()

    #text = sec2_extract_table(pages)
    #browser()
    #sec2_parse(text)
  }#)
  #names(ans) = files

  #saveRDS(ans, "foo.rds")
  #browser()
}


#' Get LCAP Section 2 Text
#'
#' This function extracts text in each cell of a table.
sec2_extract_table = function(pages) {
  ans = character(0)

  for (page in pages) {
    # Get all cells on the page.
    lines = getNodeSet(page, "./line")
    if (length(lines) == 0) {
      # FIXME: Skip Annual Update page if it has no lines.
      # FIXME: What to do when there are no lines?
      browser()
      next
    }
    cells = find_rectangles(bbox_matrix(lines), 2)
    cells = cells[order(cells[, 2], cells[, 1]), ]

    texts = getNodeSet(page, "./text")

    # Split the text nodes by cell.
    idx = vapply(texts, function(text) {
      match(TRUE, in_rect(get_xy(text), cells), NA_real_)
    }, numeric(1))
    idx = factor(idx, seq_len(nrow(cells)))
    texts = split(texts, idx)

    # Collapse the text nodes in each cell into a newline-separated string.
    texts = vapply(texts, function(grp) {
      paste0(vapply(grp, xmlValue, character(1)), collapse = "\n")
    }, character(1))

    ans = append(ans, texts)
  }

  names(ans) = NULL
  return (ans)
}


sec2_locate_first = function(xml) {
  first = xml_find_all(xml, sprintf(
    # Fixed for Magnolia Elementary
    "//page[
      (
        text[contains(normalize-space(text()), '%s')]
        and not(text[contains(normalize-space(text()), '%s')])
      )
      or text[contains(normalize-space(text()), '%s')]
    ][1]/@number"
    , "Related State and/or Local"
    , "Identify the state and/or local"
    , "Related State and /or Local"
  ))
  if (length(first) > 0)
    return (xml_integer(first[[1]]))

  cat("Unable to locate Section 2.", "\n")
  browser()
  return (NULL)
}


sec2_locate_last = function(xml) {
  last = xml_find_all(xml, sprintf(
    "//page[
      text[contains(normalize-space(text()), '%s')]
      or text[contains(normalize-space(text()), '%s')]
      or text[contains(normalize-space(text()), '%s')]
    ][1]/@number"
    , "Annual Update Instructions"
    , "Original"
    , "Section 3: Use of Supplemental"
  ))
  if (length(last) > 0)
    return (xml_integer(last[[1]]))

  cat("Unable to locate Annual Update.", "\n")
  browser()
  return (NULL)
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
  xy = xml_attrs(node)[c("left", "top")]
  as.numeric(xy)
}


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
    browser()
  })
}
