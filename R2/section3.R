getSupplementalTotal =
function(file, doc = readPDFXML(file))
{
    if(isScanned2(doc))
       return(NA)
    
    # Use of Supplemental and Concentration Grant funds and Proportionality
#   sec3 = getNodeSet(doc, "//text[starts-with(., 'Section 3: Use of')]")

    total = getNodeSet(doc, "//text[starts-with(lower-case(normalize-space(.)), 'total amount of supplemental and concentration')]")

    if(length(total)) {
        top = as.integer(xmlGetAttr(total[[1]], "top"))
        node = getNodeSet(xmlParent(total[[1]]), sprintf(".//text[abs(@top - %d) < 7 and contains(., '$')]", top))
        return(trim(gsub(".*calculated:", "", xmlValue(node))))
    }

    findSupplementalTotal(doc)
}

normalizeSpace =
function(x)
{
  trim( gsub("([[:space:]])[[:space:]]+", "\\1", x))
}

findSupplementalTotal =
function(doc,
         pagesByLine = lapply(doc, function(x) nodesByLine(getNodeSet(x, ".//text"), asNodes = TRUE)))
{
    w = lapply(pagesByLine,
               function(x)
                  x[grepl('total amount of supplemental and concentration', normalizeSpace(names(x)),
                          ignore.case = TRUE)])
    
    i = sapply(w, length) > 0
    nodes = w[i][[1]]
    trim(gsub(".*calculated:", "", names(nodes)))
}


###############################################################

getPctIncrease =
function(file, doc = readPDFXML(file), page = getPctPage(doc), asNodes = TRUE)
{
    nodes = getNodeSet(page,
                       ".//text[isPct(string(.))] | .//text[isNumber(string(.))
                               and starts-with(following-sibling::text[1], '%')]",
                       xpathFuns = list(isPct = isPctString, isNumber = isNumber))

  if(length(nodes) > 1) {
      bb = getBBox2(nodes)
      box = getPctBox(page)
      if(is.null(box))
         return(getPctIncrease(page = getSibling(page)))
      
      ok = bb[,1] > box[1] & bb[,1] + bb[,3] < box[3] & bb[, 2] > box[2] & bb[,2] + bb[,4] < box[4]
      nodes = nodes[ok]
  }

    if(!asNodes) 
        return(mkNum(paste(xmlValue(nodes), collapse = "")))


    nodes
}

isPctString =
function(str)
  grepl("^[[:space:]]*[0-9]+\\.[0-9]{1,2} *%? *$", str)

isNumber =
    #XXX This will generate warnings when it returns false.
function(x)
{    
    # !is.na(as.numeric(x))
    grepl("^[[:space:]]*[0-9]*(\\.[0-9]*)?[[:space:]]*$", x)
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
    if(!any(colored))
       return(NULL)
    
    tmp = bx[colored, , drop = FALSE]
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
  xmlParent(ll[[1]])    
}



mkNum =
function(x)
{
  as.numeric(gsub("%", "", x))
}
