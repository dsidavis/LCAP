# remove all but the first four pages.
doc = htmlParse("Winton_LCAP_2015.2018.html")
p = getNodeSet(doc, "//div[@data-page-no]")
length(p)
removeNodes(p[-c(9:20)])

p = getNodeSet(doc, "//div[@data-page-no]")
removeNodes(sapply(p, `[[`, 2))

img = getNodeSet(doc, "//div[@data-page-no]/div/img")
removeNodes(img)

p = getNodeSet(doc, "//div[@data-page-no]")

invisible(sapply(p, function(x) replaceNodeWithChildren(x[[1]])))

invisible(sapply(p, replaceNodeWithChildren))

#
b = xmlRoot(doc)[["body"]]
xmlSize(b)
names(b)
xmlSApply(b, xmlSize)
# 4th element - second div is the big one.

topdiv = b[[4]]
#removeNodes(b[ names(b) == "text"])

removeNodes(topdiv[ names(topdiv) == "text"])

# This is now flat.
