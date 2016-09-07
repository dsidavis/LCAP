#
# main.R
#
# Entry point for PDF data extraction.

library(xml2)
library(stringr)

source("pdf_xml.R")
source("geometry.R")
source("plot_geometry.R")


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

    page = pages[[1]]

    plot_pdf_new(page)
    rects = pdf_bbox(xml_find_all(page, "./rect"))
    lines = rects_to_lines(rects)
    #lines = split_lines_hv(lines)
    lines = simplify_lines(lines)
    #rapply(lines, plot_pdf_lines)
    lines_to_cells(lines)

    browser()
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
    lines = xml_find_all(page, "./line")
    rects = xml_find_all(page, "./rect")
    if (length(lines) == 0 && length(rects) == 0) {
      # FIXME: Skip Annual Update page if it has no lines.
      # FIXME: What to do when there are no lines?
      cat(sprintf("Page %s does not have line or rect tags.\n",
          xml_attr(page, "number")))
      next
    }
    lines = find_rectangles(pdf_bbox(lines), 2)
    rects = pdf_bbox(rects)

    cells = rbind(lines, rects)
    cells = cells[order(cells[, 2], cells[, 1]), ]
    cells = cells[!duplicated(cells), ]

    texts = xml_find_all(page, "./text")

    # Split the text nodes by cell.
    idx = vapply(texts, function(text) {
      match(TRUE, in_rect(get_xy(text), cells), NA_real_)
    }, numeric(1))
    idx = factor(idx, seq_len(nrow(cells)))
    texts = split(texts, idx)

    # Collapse the text nodes in each cell into a newline-separated string.
    texts = vapply(texts, function(text) {
      paste0(xml_text(text), collapse = "\n")
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
  groups = cumsum(grepl("GOAL\\s+[1-9]\\w*:", text, ignore.case = TRUE))
  goals = split(text, groups)
  if (groups[1] == 0)
    goals = goals[-1]

  lapply(goals, function(goal) {
    # Split goal into LCAP years.
    years = split(goal, cumsum(grepl("LCAP Year [1-9]:", goal)))
    header = years[[1]]
    browser()

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
  rect[, 1] <= pt[1] & pt[1] <= rect[, 3] &
    rect[, 2] <= pt[2] & pt[2] <= rect[, 4]
}


get_xy = function(node) {
  xy = xml_attrs(node)[c("left", "top")]
  as.numeric(xy)
}
