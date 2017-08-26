getSupplementalTotal =
function(file, doc = readPDFXML(file))
{
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
    w = lapply(pagesByLine, function(x) x[grepl('total amount of supplemental and concentration', normalizeSpace(names(x)), ignore.case = TRUE)])
    i = sapply(w, length) > 0
    nodes = w[i][[1]]
    trim(gsub(".*calculated:", "", names(nodes)))
}

getPctIncrease =
function(file, doc = readPDFXML(file))
{
  ll = getNodeSet(doc, "//text[ contains(normalize-space(.), 'Consistent with the requirements of 5 CCR 15496') ]")
  p = xmlParent(ll[[1]])
  getNodeSet(p, ".//text[isPct(string(.))]", xpathFuns = list(isPct = isPctString))
}

isPctString =
function(str)
    grepl("^[0-9]+\\.[0-9]{2} *%? *$", str)
