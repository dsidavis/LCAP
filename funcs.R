# We should look at the CVRead package for some ideas and possible shared code.

library(XML)

# Without flattening 
# doc = htmlParse("Winton_LCAP_2015.2018.html")
# g = getPageNode(getTable2Root(doc))
# zz = getTable2Cells(g)

# With flattening
# doc = htmlParse("Winton_LCAP_2015.2018.html")
# g = getTable2Root(doc)
# The parent is the container of all the nodes across all the pages.
# So we have html/body/div[id=page-container]/<all nodes>
# So we can then get the nodes we want.
#
#
#
#



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
    
    if(dropFirstPage) {
       p = pageNodes[-1]
       removeNodes(p)
    }
    
    if(removeImages)
      removeNodes(getNodeSet(doc, "//div[@data-page-no]/div[1]/img"))


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
    p = getNodeSet(doc, "//div[@data-page-no]")

    p = getNodeSet(doc, "//div[@data-page-no]")
    removeNodes(sapply(p, `[[`, 2))

    img = getNodeSet(doc, "//div[@data-page-no]/div/img")
    removeNodes(img)

    p = getNodeSet(doc, "//div[@data-page-no]")

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
    # Given the cells from the different pages, assemble them into a data frame
    #
    #
function(els, ncol = 4)
{
  h = els[1:ncol]
  els = els[-(1:ncol)]
  m = matrix(sapply(els, function(x) paste(xmlSApply(x, xmlValue), collapse = " ")),  , ncol, byrow = TRUE)
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

