library(ReadPDF); library(XML)
invisible(lapply(list.files("../ReadPDF/R", pattern = "\\.R$", full = TRUE), source))

pdfs = list.files("../Jacob/Data", pattern = "2015.2016.pdf$", full = TRUE)
xml = gsub("pdf$", "xml", pdfs)
x = xml[file.exists(xml)]





table(sapply(pct, is, 'try-error'))

