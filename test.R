source("funcs.R")
doc = htmlParse(ff[3])
doc = flattenPages(doc)
nodes = getGoal1Nodes(doc)
css = getCSS(doc)

a = processGoal1(nodes, css)





doc = htmlParse("pdf2htmlEX/Winton_LCAP_2015.2018.html")
g = getPageNode(getTable2Root(doc))
zz = getTable2Cells(g)

