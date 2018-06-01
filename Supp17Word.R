library(XML)
library(RWordXML)

WordNS = c("w" = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

supp17Word =
function(docx, doc = docx[["word/document.xml"]])
{
    docx = as(docx, "WordArchive")
    labels = c(amount = "Estimated", percent = 'Percentage to Increase')
    sapply(labels, getLabelValue, doc)
}

getLabelValue =
function(label, doc)    
{
    t  = getNodeSet(doc, sprintf("//w:tc[.//w:t[starts-with(., '%s')]]", label), WordNS)
    x = getSibling(t[[1]])
    xmlValue(x)
}
