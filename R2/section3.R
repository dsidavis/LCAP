getSupplementalTotal =
    #
    # numPages is how many pages isScanned2() checks. Don't need to do them all.
    #
function(file, doc = readPDFXML(file), numPages = 5, scanned = isScanned2(doc, numPages = numPages), within = 12)
{
      # isScanned(doc) || isScanned2()
    if(scanned)
        return(NA)
    
    # Use of Supplemental and Concentration Grant funds and Proportionality
#   sec3 = getNodeSet(doc, "//text[starts-with(., 'Section 3: Use of')]")
#browser()
    total = getNodeSet(doc, "//text[starts-with(lower-case(normalize-space(.)), 'total amount of supplemental and concentration') and not(contains(., '....'))]")

    if(length(total)) {

        # Perhaps we need to find the box containing the total
        # node and then the text to the right of this
        # See DelNorte_DelNorteUnified
        # For now just any text with a $ and within vertical pts from
        # the total node.
        node = getNodeNear(total[[1]], within)
        return(mkTotalValue( xmlValue(node)))
    }

    findSupplementalTotal(doc, within)
}

getNodeNear =
function(node, within = 12, page = xmlParent(node))
{
#browser()    
    top = as.integer(xmlGetAttr(node, "top"))
    pos = sum(as.integer(xmlAttrs(node)[c("left", "width")]))
        #        return(getTotal(total[[1]],, top))
        # and contains(., '$')
    nodes = getNodeSet(page, sprintf(".//text[abs(@top - %d) < %d and @left > %d ]", top, within, pos - 20))
    if(length(nodes) == 0) {
            # e.g. DelNorte_D...
            # The $ amount is one line down because the text of the box on the left spans two lines.
          within = within * 1.5
          nodes = getNodeSet(page, sprintf(".//text[abs(@top - %d) < %d and @left > %d ]", top, within, pos - 20))
    }

    return(nodes)
}

getTotal =
function(node, page = xmlParent(node),
         top = as.integer(xmlGetAttr(node, "top")))
{
    bb = getBBox(getNodeSet(page, ".//rect | .//line"))
    o = order(abs(bb[, 2] - top))
    r = unique(bb[o,2])[1:2]
    tmp = getNodeSet(page, sprintf(".//text[@top >= %f and @top < %f]", r[1], r[2]))
    mkTotalValue( xmlValue(tmp))
}

normalizeSpace =
function(x)
{
  trim( gsub("([[:space:]])[[:space:]]+", "\\1", x))
}

findSupplementalTotal =
function(doc, within = 12,
         pagesByLine = lapply(doc, function(x) nodesByLine(getNodeSet(x, ".//text"), asNodes = TRUE)))
{
browser()    
    w = lapply(pagesByLine,
               function(x)
                  x[grepl('total amount of supplemental( and concentration)?', normalizeSpace(names(x)),
                          ignore.case = TRUE)])
    
    i = sapply(w, length) > 0
    if(!any(i))
       return(NA)

    nodes = w[i][[1]][[1]][[1]]
    node = getNodeNear(nodes, within)
    mkTotalValue(xmlValue(node))
}

mkTotalValue =
function(str)    
{
    ans = gsub("[$,] ?", "", trim(gsub(".*calculated:", "", gsub("_", "", str))))
    ans[ans != ""]
}

###############################################################

getPctIncrease =
function(file, doc = readPDFXML(file), page = getPctPage(doc), asNodes = TRUE, followingPage = TRUE)
{
#    browser()
    if(followingPage == FALSE && is.null(page))
       return(NA)
    
    nodes = getNodeSet(page,
                       ".//text[isPct(string(.))] | .//text[isNumber(string(.))
                               and starts-with(following-sibling::text[1], '%')]",
                       xpathFuns = list(isPct = isPctString, isNumber = isNumber))

    if(length(nodes) == 0)
        nodes = getNodeSet(page,
                       ".//text[hasPctEnd(string(.))]",
                        xpathFuns = list(hasPctEnd = endsWithPct))


    if(length(nodes) > 1) {
        bb = getBBox2(nodes)
        box = getPctBox(page)
        if(is.null(box)) {
           if(followingPage)
              return(getPctIncrease(page = getSibling(page), followingPage = FALSE, asNodes = asNodes))
           else
              return(NULL)
        }
        #        ok = bb[,1] > box[1] & bb[,1] + bb[,3] < box[3] & bb[, 2] > box[2] & bb[,2] + bb[,4] < box[4]
        if(FALSE) {
#          ok = cbind(left = bb[,1] < box[1],  right = bb[,1] + bb[,3] < box[3] , top = bb[, 2] >= box[2] - 1, bottom = bb[,2] + bb[,4] <= box[4]+ 5)
          ok = bb[,1] < box[1] & bb[,1] + bb[,3] < box[3] & bb[, 2] >= box[2] -1 & bb[,2] + bb[,4] <= box[4]+ 5
          nodes = nodes[ok]
        } else {
          q = sprintf("@left < %f and (@left + @width) < %f and @top >= %f and (@top + @height) <= %f", box[1], box[3], box[2] - 1, box[4] + 5)
          nodes = getNodeSet(page, sprintf(".//text[%s]", q))
        }

    } else if(length(nodes) == 0)
         return(getPctIncrease(page = getSibling(page), asNodes = asNodes, followingPage = FALSE))

    if(!asNodes) 
       return(mkNum(paste(xmlValue(nodes), collapse = "")))

    nodes
}


getPctBox =
    #XXX!!!!  When 2 colored boxes, we end up with a huge box.
    #  So really need these to be broken into 2 or more.
function(page)
{
    nodes = getNodeSet(page, ".//rect | .//line")
    bx = getBBox(nodes)
    col = sapply(nodes, xmlGetAttr, "fill.color")
    colored = (col != "0,0,0")
    if(any(colored)) {

        tmp = bx[colored, , drop = FALSE]
#browser()    
        wd = tmp[,3] - tmp[,1]
        tmp  = tmp[wd > 0, , drop = FALSE]
        wd = wd[wd > 0]
        i = which.min(wd)
        el = tmp[i,]
        if(el[2] > el[4])
            el[c(2,4)] = el[c(4,2)]
        return(el)
        ok = bx[,2] >= el[2] & bx[,4] <= el[4] & !colored
        return ( bx[bx[,1] < el[1] & ok ,, drop = FALSE] )
    }
    
    cb = c(range(tmp[, c(1,3)]), range(tmp[, c(1,3)+1]))
    wd = bx[,3] - bx[,1]
    ht = bx[,4] - bx[,2]

    hi = bx[,2] >= cb[3] & bx[,2] <= cb[4]
    wi = bx[,3] <= cb[1]

    
    z = bx[hi & wi, , drop = FALSE]

    return(c(x0 = min(z[,1]), y0 = min(z[,2]), x1 = max(z[,3]), y1= max(z[, 4])))
#    return( matrix(c(min(z[,1]), min(z[,2]), max(z[,3]), max(z[, 4])), 1, 4) )
    i = wd > 20 & wd < 500 & ht > 10 & ht < 40
    bx[i,, drop = FALSE]
}


getPctPage =
function(doc)
{
  ll = getNodeSet(doc, "//text[ contains(normalize-space(.), 'Consistent with the requirements of 5 CCR 15496') ]")
  if(length(ll) == 0)
      ll = getNodeSet(doc, "//text[contains(., 'Consistent')]")
  if(length(ll) == 0)
      ll = getNodeSet(doc, "//text[contains(., 'In the box below, identify the percentage')]")
  
  if(length(ll))
      xmlParent(ll[[1]])
  else
     stop("cannot identify Percent Page")
}


isPctString =
function(str)
  grepl("^[[:space:]]*[0-9]*\\.[0-9]{1,3}[[:space:]]*%?[[:space:]]*$", str)

isNumber =
    #XXX This will generate warnings when it returns false.
function(x)
{    
    # !is.na(as.numeric(x))
    grepl("^[[:space:]]*[0-9]*(\\.[0-9]*)?[[:space:]]*$", x)
}



mkNum =
function(x)
{
  as.numeric(gsub("%", "", x))
}

endsWithPct =
function(x)
   grepl(":[[:space:]]+[0-9]+\\.[0-9]+%[[:space:]]*$", x)
