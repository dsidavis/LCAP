#
# main.R
#
# Entry point for PDF data extraction.

library(dplyr)
library(stringr)
library(tidyr)
library(xml2)

source("pdf_xml.R")
source("geometry.R")
source("plot_geometry.R")

printf = function(fmt, ...) cat(sprintf(fmt, ...))

main = function() {
  files = list.files("../xml", "LCAP_2015", full.names = TRUE)
  #files = files[-c(36, 698, 764)]

  # FIXME: tables broken across pages do not have bottom lines
  #files = "../xml/Acton-AguaDulceUnified_LCAP_2015.2018.xml"

  # FIXME: lots of tiny rectangles, no lines
  #files = "../xml/AdelantoElementary_LCAP_2015.2018.xml"

  # FIXME:
  #files = "../xml/ABCUnified_LCAP_2015.2018.xml"
  #files = "../xml/Alameda_AlamedaUnified_LCAP_2015.2016.xml" # 3 3
  #files = "../xml/Alameda_BerkeleyUnified_LCAP_2015.2018.xml"

  # Quarantine bad PDFs
  files = files[!(files %in% c(
    "../xml/Acton-AguaDulceUnified_LCAP_2015.2018.xml"  # lines around page
    , "../xml/AdelantoElementary_LCAP_2015.2018.xml"    # hz for dashed vt
    , "../xml/Alameda_CastroValleyUnified_LCAP_2015.2018.xml" # no lines
    , "../xml/Alameda_DublinUnified_LCAP_2015.2018.xml" # rows over 3 pages
    , "../xml/Alameda_EmeryUnified_LCAP_2015.2016.xml"  # rows over 3 pages
    , "../xml/Alameda_FremontUnified_LCAP_2015.2016.xml" # no lines
    # lines around page
    , "../xml/Alameda_LivermoreValleyJointUnified_LCAP_2015.2016.xml"
    , "../xml/Alameda_NewarkUnified_LCAP_2015.2016.xml" # no lines
    , "../xml/Alameda_NewHavenUnified_LCAP_2015.2018.xml" # no lines
    , "../xml/Alameda_OaklandUnified_LCAP_2015.2018.xml" # hz for dashed vt
    # memory:
    , "../xml/Amador_AmadorCountyUnified_LCAP_2015.2016.xml"
    , "../xml/SpreckelsUnionElementary_LCAP_2015.2016.xml"
    , "../xml/WeaverUnion_LCAP_2015.2016.xml"
  ))]

  files = data_frame(xml = files)
  files$rds = str_replace(basename(files$xml), "[.]xml$", ".rds")
  files$rds = file.path("..", "data", files$rds)
  files$tol_x = 5
  files$tol_y = 5

  # Set a lower tolerance for some files.
  low_tol = files$xml %in% c(
    "../xml/Alameda_AlamedaUnified_LCAP_2015.2016.xml" # 3 3
  )
  files$tol_x[low_tol] = 3
  files$tol_y[low_tol] = 3

  for (i in seq_len(nrow(files))) {
    file = files[i, ]
    printf("%s\n", file$xml)

    if (file.exists(file$rds)) {
      printf("  Skipped, RDS exists.\n")
      next
    }

    xml = xml2::read_xml(file$xml)
    first = sec2_locate_first(xml)
    last = sec2_locate_last(xml)
    printf("  Found %i-%i.\n", first, last)
    
    pages = xml_find_all(xml, sprintf(
      "//page[%s <= @number and @number <= %s]"
      , first
      , last
    ))

    tryCatch({
      text = sec2_extract_text(pages, file$tol_x, file$tol_y)
      data = sec2_parse(text)
      saveRDS(data, file$rds)
    }, error = function(e) {
      printf("  Failed!\n")
    })
  }
}


#' Get LCAP Section 2 Text
#'
#' This function extracts text in each cell of a table.
sec2_extract_text = function(pages, tol_x, tol_y) {
  all_text = lapply(seq_along(pages), function(i) {
    page = pages[[i]]
    page_no = as.integer(xml_attr(page, "number"))

    # Get page geometry.
    lines = pdf_bbox(xml_find_all(page, "./line"))
    rects = pdf_bbox(xml_find_all(page, "./rect"))
    if (length(lines) == 0 && length(rects) == 0) {
      # FIXME: Skip Annual Update page if it has no lines.
      # FIXME: What to do when there are no lines?
      printf("  Page %s does not have line or rect tags.\n", page_no)
      return (NULL)
    }
    lines = rbind(lines, rects_to_lines(rects))

    cells = lines_to_cells(lines, tol_x, tol_y)
    cells = cells_to_rows(cells, tol_x)

    #if (page_no == 14) {
    #  plot_pdf_new(page)
    #  plot_pdf_rects(lines)
    #  browser()
    #}

    # Get page text.
    texts = pdf_text(page)
    texts$page   = page_no
    texts$cell   = which_rect(texts[1:2], cells)
    texts$row    = cells[texts$cell, 5]
    texts$c_left = cells[texts$cell, 1]

    # Associate orphaned text nodes with an x-adjacent cell.
    is_orphan = is.na(texts$cell)
    #texts$cell[is_orphan]
    orphan_loc = apply(texts[is_orphan, 1:2], 1,
      # Compute (cell, row, c_left)
      function(orphan) {
        in_row = which(cells[, 2] <= orphan[2] & orphan[2] < cells[, 4])
        if (length(in_row) == 0)
          return (rep(NA, 3)) # no cells found

        dst = cells[in_row, 3] - orphan[1]
        to_left = dst <= 0
        if (any(to_left)) {
          # found cell to left
          idx = in_row[which.max(dst[to_left])]
          return (c(idx + 0.1, cells[idx, 5], cells[idx, 3]))
        } else {
          # found cell to right
          idx = in_row[which.min(dist_left)]
          return (c(idx - 0.1, cells[idx, 5], orphan[1]))
        }
    })
    
    texts$cell[is_orphan]   = orphan_loc[1, ]
    texts$row[is_orphan]    = orphan_loc[2, ]
    texts$c_left[is_orphan] = orphan_loc[3, ]

    return (texts)
  })
  
  do.call(rbind, all_text)
}


sec2_parse = function(text) {
  # Separate text that wasn't tagged with a cell.
  is_orphan = is.na(text$cell)
  orphans = text[is_orphan, ]
  text = text[!is_orphan, ]

  # Sort in table order.
  text = arrange(text, page, row, cell)

  # FIXME: Split "Additional Supplemental Funds" Alameda

  # Split GOALs.
  re = regex("^\\s*GOAL\\s*([1-9]\\w?)?:?$", ignore_case = FALSE)
  is_goal = str_detect(text$text, re)
  goals = split(text, cumsum(is_goal))

  goals = lapply(goals, function(goal) {
    is_year = str_detect(goal$text, "LCAP Year [1-9]:")
    years = split(goal, cumsum(is_year))

    data = sec2_parse_header(years[[1]])
    data$years = lapply(years[-1], sec2_parse_year)

    return (data)
  })

  return (goals)
}


sec2_parse_header = function(header) {
  #   GOAL 1 | <goal> | <priorities>
  #   Identified Need: | <need>
  #   Goal Applies to: | Schools: <schools>
  #                    | Subgroups: | <subgroups>
  text = header$text

  idx = match(TRUE, str_detect(text, fixed("Related State and")))
  goal = text[1:(idx - 1)]
  text = text[idx:length(text)]

  idx = match(TRUE, str_detect(text, fixed("Identified Need")))
  if (is.na(idx)) {
    idx = match(TRUE, str_detect(text, "^\\s*Identified"))
  }
  priorities = text[1:(idx - 1)]
  text = text[idx:length(text)]

  idx = match(TRUE, str_detect(text, fixed("Goal Applies")))
  need = text[1:(idx - 1)]
  text = text[idx:length(text)]

  idx = match(TRUE, str_detect(text, fixed("Applicable Pupil")))
  applies_schools = text[1:(idx - 1)]
  text = text[idx:length(text)]

  list(
    goal                = goal
    , priorities        = priorities
    , need              = need
    , applies_schools   = applies_schools
    , applies_subgroups = text
  )
}


sec2_parse_year = function(year) {
  #   <year>
  #   EAMO: | <eamo>
  #   Actions/Services | Scope   | Pupils   | Budgeted Expenditures
  #   <actions>        | <scope> | <pupils> | <expend>

  # Merge text in each cell, then merge cells in each row by left edge.
  year = summarize(group_by(year, page, row, cell),
    text = paste(text, collapse = "\n"), c_left = min(c_left))
  year = summarize(group_by(year, c_left, add = TRUE),
    text = paste(text, collapse = "\n"), cell = min(cell))
  year = arrange(year, page, row, c_left, cell)
  rows = nest(group_by(year, page, row))

  # LCAP Year & EAMO
  # ----------------
  lcap_year = rows$data[[1]]$text
  rows = rows[-1, ]

  eamo = rows$data[[1]]$text
  rows = rows[-1, ]

  row_text = rows$data[[1]]$text
  if (length(row_text) == 1 || str_detect(row_text[[1]], "^\\s*$")) {
    eamo = c(eamo, row_text)
    rows = rows[-1, ]
  }

  # Actions/Services Table
  # ----------------------
  row_text = rows$data[[1]]$text
  if ( any(str_detect(row_text, "Actions\\s*(/|and)\\s*Services")) ) {
    rows = rows[-1, ]
  }

  cols = quantile(unnest(rows)$c_left, seq.int(0, 1, length.out = 4)) - 10

  actions = matrix(NA_character_, nrow(rows), 4)
  prev_page = rows$page[[1]]
  for ( i in seq_len(nrow(rows)) ) {
    text = rows$data[[i]]$text

    # Skip blank rows.
    if (all(str_trim(text) == ""))
      next

    # Check for rows that don't have length 4.
    if (length(text) < 4) {
      idx = findInterval(rows$data[[i]]$c_left, cols)
      if (any(idx == 0)) {
        stop("  Error: Found short row in actions table.\n")
        browser()
      }
      temp = character(4)
      temp[idx] = text
      text = temp

    } else if (length(text) > 4) {
      stop("  Error: Found long row in actions table.\n")
      browser()
    }

    # Check for split rows.
    re = regex("^\\s*[X_]*\\s*ALL", ignore_case = TRUE)
    if (prev_page != rows$page[[i]] && !str_detect(text[3], re)) {
      actions[i - 1, ] = paste(actions[i - 1, ], text, sep = "\n")
    } else {
      actions[i, ] = text
    }

    prev_page = rows$page[[i]]
  }

  actions = actions[rowSums(is.na(actions)) == 0, ]

  list(
    year      = lcap_year
    , eamo    = eamo
    , actions = actions
  )
}


# Both of the section 2 locator find matches for all LCAPs.
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
