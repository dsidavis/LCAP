#
# getTable.R
#


library(XML)
doc = htmlParse("Winton_LCAP_2015.2018.html")

pages = getNodeSet(doc, "//div[@data-page-no]")

pages[[10]]

p11 = pages[[11]]




# o = getNodeSet(doc, "//text()[contains(., 'Maintain the Teacher')]/../..")
