
getOutcomes =
    #
    # For each page, grab the
    #
    # This won't get all of them if the outcomes span two pages, i.e. start on one and end on another.
    # Let's see if this arises.
    # We can see if the Actions/Services are on the same page. If not, then we need to go to the next page, etc.
    # and find it.
    #
#function(page, text = getAllText(xmlChildren(page)))
    # Using the xpathSApply( ".//text") we split
function(page, text = paste(xpathSApply(page, ".//text", xmlValue), collapse = "\n"))
{
  text = strsplit(text, "\\n")[[1]]
  i = grep("^Expected Annual|Actions/Services", text)
  if(length(i) < 2)
      return(character())

  ctnt = text[ (i[1] + 1) : (i[2] - 1) ]
    # In some cases, the newline won't appear after the LCAP..... So remove it.
    # But if we use xpathSApply to define text, it will.
  ctnt = gsub("LCAP Year [0-9]+: [0-9]{4}-[0-9][0-9]", "", ctnt)

  ctnt = ctnt[ ctnt != ""]
  ctnt = ctnt [ - grep("^(Measurable |Outcomes:|LCAP Year |Page [0-9]+ of )", ctnt) ]

  cleanOutcomes(ctnt)
}

cleanOutcomes =
    #
    # Clean up the 1.2.a etc. at the beginning of the outcomes. Set these as names.
    #
function(txt)
{
  nums = gsub("^([0-9]+\\.[0-9]+\\.([a-z]+\\.)?) .*", "\\1", txt)
  vals = gsub("^[0-9]+\\.[0-9]+\\.([a-z]+\\.)? ", "", txt)
  names(vals) = nums
  vals
}
