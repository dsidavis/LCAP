xml = list.files("2017_18", pattern = "xml$")
pdf = list.files("2017_18", pattern = "pdf$")
length(xml)
length(pdf)

m = setdiff(gsub("pdf$", "xml", pdf), xml)

mm = file.path("2017_18", gsub("xml$", "pdf", m))
i = file.info(mm)

sum(i$size == 0)

fileType = sapply(sprintf("file '2017_18/%s'", pdf), function(cmd) system(cmd, intern = TRUE))
names(ty) = sprintf("2017_18/%s", pdf)

ty = gsub(".*: ([^ ]+) .*", "\\1", fileType)

table(ty)
#  193204,      HTML Microsoft       PDF       Zip 
#        1       110         2       799         1 

html = names(ty)[ty == "HTML"]
