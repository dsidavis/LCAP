
library(XML)

source("~/DSIProjects/pdftohtml/plot.R")

f = "MillValley_LCAP_2015.2016.xml"

getGoal1 =
function(filename, doc = xmlParse(filename), flatten = FALSE)
{
   if(flatten)
      doc = flattenPages(doc)

     # Find the GOAL 1.  Need to be more flexible later
   g1 = getNodeSet(doc, "//text[starts-with(., 'GOAL 1:')]")
     # Find all the text nodes in this page and the following pages
     # We will find the second GOAL and then use all the nodes from here to that for Goal 1
   txtNodes = getNodeSet(g1[[1]], "./following-sibling::text | ../following-sibling::page/text")
   txt = sapply(txtNodes, xmlValue)
   i = grep("GOAL", txt)
   nodes = c(g1[[1]], txt[1:(i[1] - 1)])

   # now let's find the rectangles on this page - assuming they are the same for the entire table.
   #  There is a strange vertical line in the middle of the page
   # We are looking for vertical lines that start at the same approx. height as the first row of the table
   # 
   header = c("Actions/Services", "Scope of", "Pupils to be served within identified scope of service", "Budgeted")
   h = lapply(header, function(phrase)  getNodeSet(pg, sprintf("./text[starts-with(., '%s')]", phrase))[[1]])
   names(h) = header
   bb = getBBox2(h)

    # Find the vertical "lines"     
   rects = getNodeSet(pg, "./rect")
   vertRects = getVerticalRects(rects)

    # Find the vertical lines neear these nodes in h

   sapply(1:nrow(bb), findNearestVerticaLine)

       
   vertRects[,1]
}

findNearestVerticalLine =
function(txtPos, linePos)
{
     abs(txtPos[2] - vertRects[,2]) < 10
}
    

getBBox2 =
    # For text, not rect nodes.
function(nodes)
{
   ats = c("left", "top", "width", "height")
   m = do.call(rbind, lapply(nodes, function(x) as.integer(xmlAttrs(x)[ats])))
   colnames(m) = ats
   m
}


getVerticalRects =
function(nodes, threshold = 5)
{
   bb = getBBox(nodes)
   w = (bb[,3] - bb[,1]) < threshold
   bb[w,]
}


getHorizRects =
function(nodes, threshold = 5)
{
   bb = getBBox(nodes)
   w = (bb[,4] - bb[,2]) < threshold
   bb[w,]
}

getCrossPageLines =
function(rects, horiz = getHorizRects(rects), width = getPageWidth(xmlParent(rects[[1]])),
          threshold = .85)
{
  browser()
  w = (horiz[,3] - horiz[,1])/width > threshold
  horiz[w,]
}

getPageWidth =
function(page)
{
   as.integer( xmlGetAttr(page, "width") )
}


flattenPages =
function(doc)
{
  pgs = getNodeSet(doc, "//page")
  lapply(pgs, replaceNodeWithChildren)
  doc
}


