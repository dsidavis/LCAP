# We should look at the CVRead package for some ideas and possible shared code.

library(XML)
library(RCSS)

# Get the LCAP Year value.

if(FALSE) {
z = getGoal1("pdf2htmlEX/Winton_LCAP_2015.2018.html")
names(z)  # [1] "goal"       "priorities" "need"       "appliesTo"  "eamo"       "actions"   
}


# Without flattening 
# doc = htmlParse("pdf2htmlEX/Winton_LCAP_2015.2018.html")
# g = getPageNode(getTable2Root(doc))
# zz = getTable2Cells(g)

# With flattening
# doc = htmlParse("pdf2htmlEX/Winton_LCAP_2015.2018.html")
# g = getTable2Root(doc)
# The parent is the container of all the nodes across all the pages.
# So we have html/body/div[id=page-container]/<all nodes>
# So we can then get the nodes we want.
#
#
# After flattening
# top = xmlRoot(doc)[["body"]][[4]]
# gls = getNodeSet(top, "./div[./preceding-sibling::div[not(contains(., 'Goal 1:'))] and not(following-sibling::div[contains(., 'Goal 2:')])]")
#
#
#
# Get the goal
#
# 
#

getGoal1 =
function(doc)
{
   if(is.character(doc))
      doc = htmlParse(doc)
  processGoal1( getGoal1Nodes(doc, flatten = TRUE), css = getCSS(doc) )
}

getGoal1Nodes =
    # assumes doc has been flattend.
function(doc, top = xmlRoot(doc)[["body"]][[4]], flatten = FALSE)
{
   if(is.character(doc))
      doc = htmlParse(doc)   
   if(flatten)
      doc = flattenPages(doc)

     # While Winton had GOAL   Goal 1:, Horicon and others have GOAL 1: in the first cell of the table.
   g1 = getNodeSet(top, "./div[contains(., 'Goal 1:') ]/preceding-sibling::div[1]")[[1]]
   if(is.null(g1))
       g1 = getNodeSet(top, "./div[contains(., 'GOAL 1:') ]")[[1]]

   if(is.null(g1))
       stop("cannot find Goal 1")
   
   nxt = getNodeSet(g1, "./following-sibling::div")
   i = grep("GOAL", sapply(nxt, xmlValue))
   nodes = c(g1, nxt[1:(i[1]-1)])
      #  Probably don't want the blank spaced <div> nodes.
   nodes = nodes[ XML:::trim(sapply(nodes, xmlValue)) != ""]
   txt = sapply(nodes, xmlValue)
   w = grepl("^(Page [0-9]+ of [0-9]+|(Revised & )?Board Approved.*[0-9]{,2}, [0-9]{4}|Complete a copy of this table for each of the LEA)", txt)
   nodes = nodes[!w]

   nodes
}

getCSS =
function(doc)
{
   cssNodes = getNodeSet(doc, "//style[@type = 'text/css']")
   css = readCSS( xmlValue( cssNodes[[3]] ), asText = TRUE)
   css
}

getNodeBBox =
function(node, css, elNames = c("bottom", "height", "width", "left"))
{
    cssi = sapply(getCSSRules(node, css), asCSSObject)
    els = unlist( lapply(cssi, slot, "declarations"))
    ans = as.data.frame(lapply(els[elNames], function(x) if(is.null(x)) NA else x@value), stringsAsFactors = FALSE)
    names(ans) = elNames
    ans
}

getBBox =
function(nodes, css)
{
    vals = lapply(nodes, getNodeBBox, css)
#    len = sapply(vals, length)
#    if(!all(len == 4))
#       stop("sort this out. Just get the left, width, height, ")
    bbox = do.call(rbind, vals)
    bbox$text = sapply(nodes, xmlValue)
    bbox
}

mergePageNodes =
    #
    # We want to combine successive <div>s that are at essentially the same place.
    #
function(page, css)
{
  if(xmlGetAttr(page, "data-page-no") == "16") browser()    
  d = page[[1]]
  bbox = getBBox(xmlChildren(d), css)
  D = as.matrix(dist(bbox[, 1:4]))
  close = sapply(1:nrow(D),function(i) c(i, which(D[,i] > 0 & D[,i] < 2)))
  close = close[ sapply(close, length) > 1]
  
  page
}



processGoal1 =
    # Called with output from getGoal1Nodes
function(nodes, css)    
{
#    nodes = mergeNodes(nodes, getBBox(nodes, css))
    
    txt = sapply(nodes, xmlValue)    
    eamo = getEAMO(nodes, txt)
    
    ends = grep("LCAP Year", txt)
    ends = c(ends[-1] - 1, length(txt)) 
    starts = grep("Actions/Services", txt)

    ans = lapply(seq(along = starts),
                  function(i) {
                     makeTable(nodes[starts[i]:ends[i]], css)
                  })
    
    list(goal = xmlValue(nodes[[2]]),
         priorities = sapply(nodes[4:5], xmlValue),
         need = xmlValue(nodes[[7]]),
         appliesTo = c(schools = xmlValue(nodes[[10]]), pupils = xmlValue(nodes[[12]])),
         eamo = eamo, actions = ans)
}

getEAMO =
function(nodes, txt = sapply(nodes, xmlValue))
{
   i = grep("Expected Annual Measurable Outcomes", txt)

   lapply(nodes[i+1], processEAMO)
}

processEAMO =
function(node)
{
   divs = xmlChildren(node)
   txt = XML:::trim(sapply(divs, xmlValue))
   els = split(txt, cumsum(txt == ""))
   ans = XML:::trim(sapply(els, paste, collapse = "\n"))
   ans[ans != ""]
}


if(FALSE)
flattenPages =
    # See below for a more brute force way.
    #
    # replaces the page div with its children so that all the contents are at the same level
    #
    # removeImages is just for the ones as the first node in each page. There may be others in the content.
    # THe onese we are removing are the lines on the pages for the table.
    #
function(doc, pageNodes = getNodeSet(doc, "//div[@data-page-no]"), removeImages = TRUE, dropFirstPage = TRUE)
{

browser()    
    if(dropFirstPage) {
       removeNodes(pageNodes[[1]])
       pageNodes = pageNodes[-1]
    }
    
    if(removeImages)
       removeNodes(getNodeSet(doc, "//div[@data-page-no]/div[1]/img"))

    removeNodes(getNodeSet(doc, "//span[@class = '_ _0']"))    


      # A page has 2 elements - the real content and
      # <div class="pi" data-data="{&quot;ctm&quot;:[1.000000,0.000000,0.000000,1.000000,0.000000,0.000000]}"/>
      # ctm is the current transformation matrix. So let's drop these
      # probably want to dig down one more level to get
    removeNodes(lapply(pageNodes, `[[`, 2))
    
      # Each page has 3 ancestors. It is  html body div div (the last div is the page itself)
      # The first ancestor/parent is a div with @id="page-container"
      # So we probably want to take the first child of each page and replace the page
      # Then replace the page-container nodes with their child.

    
#    containers = lapply(pageNodes, xmlParent)
#    lapply(pageNodes, replaceNodeWithChildren)
#    lapply(containers, replaceNodeWithChildren)    

    doc
}


flattenPages =
    # 2nd version. more brute force.
function(doc)
{
    removeNodes(getNodeSet(doc, "//span[@class = '_ _0']"))
    
    p = getNodeSet(doc, "//div[@data-page-no]")
    removeNodes(sapply(p, `[[`, 2))

    img = getNodeSet(doc, "//div[@data-page-no]/div/img")
    removeNodes(img)

#    lapply(p, mergePageNodes, getCSS(doc))

    sapply(p, function(x) replaceNodeWithChildren(x[[1]]))

    sapply(p, replaceNodeWithChildren)

      # probably replace this with an xpath expression.
    b = xmlRoot(doc)[["body"]]
    topdiv = b[[4]]
    removeNodes(topdiv[ names(topdiv) == "text"])

    doc
}

removePages =
    #
    # From CVRead. Merge back if it is worth it.
    #
function(doc = as(pages[[1]], "XMLInternalDocument"), pages = getNodeSet(doc, "//page"))
{
    lapply(pages, replaceNodeWithChildren)
    doc
}

#---------------------------

getExpectedOutcomes =
function(root)
{
    getNodeSet(doc, ".//")
}


###########################
# Everything below here was done with pages.
# So should be simpler if we flatten pages.

getTable2Root =
    #
    # Find the root node for the GOAL  |  Goal 1
    # This is the GOAL cell, not the page.
    # Matching GOAL will match all questions. We want just the Goal 1 for now.
    #
function(doc)
{
   a = getNodeSet(doc, "//div[contains(text(), 'Goal 1')]/../preceding-sibling::div[contains(.,'GOAL')]")
   a[[1]]
}


getTable2Cells =
    # This is for the Actions/Services part of the table up to the next LCAP Year...
    #  
    # Given the <div>GOAL</div>, get all the cells for this page 
    # and subsequent pages of the same table before the next LCAP Year....
    #
function(root, doc = as(root, "XMLInternalDocument"))
{    
   els = getFirstPageCells(root)
   page = getPageNode(root)

   pageNum = 1   
   while(TRUE)  {
      cat(pageNum, "\n")
      page = getNextPage(page)
      content = page[[1]][-(1:5)]
      r1 = getNodeSet(page, "./div/div[contains(., 'Action:') and contains(., 'Services') and not(preceding-sibling::div[starts-with(., 'LCAP Year')])]")      
      rem = getNodeSet(r1[[1]], sprintf("./following-sibling::div[position() < %d]", length(r1) *4))
      els = c(els, r1[[1]], rem)

        # if there are more cells on this page, then we have reached the next LCAP Year ...
      if(length(rem) + 1  < xmlSize(page[[1]]) - 5)
          break

      pageNum = pageNum + 1
   }

   # We should return the node where we ended or the page number or whatever, so we can pick up from there for the next part of the table.
   makeTable(els)
}

makeTable =
    #
    # Given the cells from the different pages, assemble them into a matrix
    # We get the locations of each node, i.e. top, 
    #
function(els, css, ncol = 4)
{
      # Get the bounding box for each of these nodes. Then we  figure out the rows
      # the x coordinate of the columns
  bbox = getBBox(els, css)


     # Now break the cells into columns by finding the typical
     # left aligned space
  horiz = bbox[, "left"]
  counts = table(floor(horiz))
  breaks = as.integer(names(counts))
  d = diff(breaks)
  breaks = breaks[c(TRUE, d > 20)] - 5
        # cut(, ncol) doesn't do the job.
  cols = cut(horiz, c(breaks, Inf))
  bbox$col = cols

  top = bbox[,1] + bbox[, 2]  
  rows = split(bbox, cumsum(c(0, abs(diff(top))) > 30))
 
     # Now split each row.
  ans = lapply(rows, makeRow)

  ans = do.call(rbind, ans)

  structure(ans[-1,], dimnames = list(NULL, ans[1,]))

# Original version for Winton...
#  h = els[1:ncol]
#  els = els[-(1:ncol)]
#  m = matrix(sapply(els, function(x) paste(xmlSApply(x, xmlValue), collapse = " ")),  , ncol, byrow = TRUE)
}

makeRow =
    # A data frame with bottom height with left text and col
    # Group by col and collapse
function(df)
{
   tapply(df$text, df$col, paste, collapse = "\n")
}

getNextPage =
    #
    # Given the root node of one page, find the next page.
    # This skips over a blankempty text node.
    #
function(page)
{
    getNodeSet(page, "./following-sibling::div")[[1]]
}

getFirstPageCells =
    #
    # Find the Actions/Services node in the current page. Assumes we have identified the first page in root.
    #
function(root)
{
   b = getNodeSet(root, ".//div/div/div[contains(., 'Actions/Services')]/..")
   els = c(b[[1]], getNodeSet(b[[1]], ".//following-sibling::div"))
}

getPageNode =
    #
function(node)
{
    getNodeSet(node, "./ancestor-or-self::div[@data-page-no]")[[1]]
}


getPageTable =
    #
    # Not used now.
    #
function(page, ncols = 4)
{
   els = page[[1]][-(1:5)]  # page 10 of 55, Board Approved, Revised ...
    # 5th one is <div class="t m0 x1 h2 y10 ff1 fs0 fc0 sc0 ls1 ws1"> </div> 

   m = matrix(sapply(els, function(x) paste(xmlSApply(x, xmlValue), collapse = " ")),  , 4, byrow = TRUE)
}

