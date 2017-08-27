# Scanned:
#  See README for more information on which files are scanned.
#  BishopUnified_LCAP_2015.2016.xml
#  BigPineUnified_LCAP_2015.2016.xml
#  BigOakFlat-GrovelandUnified_LCAP_2015.2016.xml  (and rotated)

library(ReadPDF); library(XML)
invisible(lapply(list.files("../ReadPDF/R", pattern = "\\.R$", full = TRUE), source))

source("R2/section3.R")
pdfs = list.files("../Jacob/Data", pattern = "2015.2016.pdf$", full = TRUE)
xml = gsub("pdf$", "xml", pdfs)
x = xml[file.exists(xml)]

totals = lapply(x, function(x) try(getSupplementalTotal(x)))

updateTotal =
function(x, totals)
{
    f = setdiff(x, names(totals))
    tmp = lapply(x, function(x) try(getSupplementalTotal(x)))
    totals[ f ] = tmp
    invisible(totals)
}

w = sapply(totals, length) == 0
x[w]

sapply(x[w], function(x) try(isScanned2(x)))
