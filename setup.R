pdfs = list.files("../Jacob/Data", pattern = "2015.2016.pdf$", full = TRUE)
xml = gsub("pdf$", "xml", pdfs)
x = xml[file.exists(xml)]

