library(XML)
library(ReadPDF)
invisible(sapply(list.files("~/DSIProjects/ReadPDF/R", pattern = "\\.R$", full = TRUE), source))

suppl17 = 
function(doc = "2017_18/Palo_Alto_Unified.xml")
{    
    #    doc = readPDFXML(f)
    doc = as(doc, "PDFToXMLDoc")

    z = getNodeSet(doc, "//text[starts-with(., 'Estimated Supplemental')]")
# For Palo Alto Unified    
# 2 instances, one ends in : The second (page 146) is instructions.
# We want the one ending in :

    w = grepl(":$", sapply(z, function(x) xmlValue(x)))
    if(!any(w)) {
        
        if(length(z))
            w = 1  # or can find the one on a page that is not the instruction page.
        else
            stop("can't find the text 'Estimated Supplemental'")
    }


    bb = getBBox2(z[w], TRUE)
    p = pageOf(z[w], TRUE)[[1]]


    # Rotation can be on the page or on the <text>
    rot = xmlGetAttr(z[w][[1]], "rotation", 0)
    rot.p = xmlGetAttr(p, "rotation", 0)
    
    pos = getBBox2(getNodeSet(p, ".//text"), TRUE)

rot = 0 # temporary.


    # We can find the colored box that "level" with the text.
#browser()    
    if(rot == 0)
        w = abs(pos$top - bb$top) < bb$height
    else {
            # 14 comes from Delair doc where the width of the rotated text is 0!
        w = abs(pos$left - bb$left) < max(bb$width, 14) 

    }
    
    i = grep("[0-9]", pos$text[w])
    pos$text[w][i]
}

if(FALSE) {
    xx = list.files("2017_18", pattern = "xml$", full = TRUE)
    xx.ans = sapply(xx, function(f) try(suppl17(f)))
    b = sapply(xx.ans, is, 'try-error')
    table(b)
#FALSE  TRUE 
#  735    59 

    scan = sapply(xx[b], function(x) tryCatch(isScanned(x), error = function(...) NA))
    table(scan, useNA = "always")
    # The ones we fail on that are not scanned and not a pdftohtml error (NA) are the wrong format
    # from 2016.
#FALSE  TRUE  <NA> 
#   30    27     2     


    # Now for the ones we got something. How many  values did we get.
    nvals = sapply(xx.ans[!b], length)
    table(nvals)
#  0   1   2   3   4   5 
# 17   3 707   5   2   1 

    ans = xx.ans[!b]
    amt = XML:::trim(sapply(ans[nvals == 2], `[`, 1))
    table(grepl("^\\$", amt))
    # 1 not right.
    amt[!(grepl("^\\$", amt))]  #

    cur = as(amt, "Currency")  # doesn't convert $2.1 million
    amt[is.na(cur)]
# Clean up ', ', million $Supplemental: 
    
    pct = XML:::trim(sapply(ans[nvals == 2], `[`, 2))
    table(grepl("%", pct))
    pct[!grepl("%", pct)] # no %
    pct = as(pct, "Percent")

    
     # 5 did not give us 2 values.
    names(ans)[nvals != 2]
#[1] "2017_18/Alpine_County_Unified_and_County_Office_of_Education.xml"
#[2] "2017_18/Bangor_Union_Elementary.xml"                             
#[3] "2017_18/Borrego_Springs_Unified.xml"                             
#[4] "2017_18/Cloverdale_Unified.xml"                                  
#[5] "2017_18/Denair_Unified.xml"

#[6] "2017_18/Durham _Unified.xml"                                     
#[7] "2017_18/Franklin-McKinley_Elementary.xml"                        
#[8] "2017_18/Gridley _Unified.xml"                                    
#[9] "2017_18/Jamul-Dulzura_Union_Elementary.xml"                      
#10] "2017_18/Kashia_Elementary.xml"                                   
#11] "2017_18/Laguna_Beach_Unified.xml"                                
#12] "2017_18/Larkspur-Corte_Madera.xml"                            rotated
#13] "2017_18/Los_Alamitos_Unified.xml"                       narrow text means on a different line than value.

    # Alpine Values in box are on 2 lines. Mid point rather than top.
    # Bangor - page is narrower and the text is taller. Need to work with mid point, not top.
    # Borrego - rotated (p43). But we cannot find the whole string in a text. So will have to recombine them.
    # Cloverdale - right of page is cropped and value is not present.
    # Denair - page 55 is rotated. Actually, it is each text element. Handled now.
#2017_18/Alpine_County_Unified_and_County_Office_of_Education.xml 
#                                                              4 
#                            2017_18/Bangor_Union_Elementary.xml 
#                                                              0 
#                            2017_18/Borrego_Springs_Unified.xml 
#                                                              0 
#                                 2017_18/Cloverdale_Unified.xml 
#                                                              1 
#                                     2017_18/Denair_Unified.xml 
#                                                              3     


    # Overall
status =    c(num = length(xx),
      numFailed = sum(b),
      numScanned = sum(!is.na(scan) & scan),
      numProc = length(nvals),
      pctProc = length(nvals)/length(xx),
      ok = sum(nvals == 2),
      notOk = sum(nvals != 2),
      pctOk = sum(nvals == 2)/length(nvals)      ,
      overallOk = sum(nvals == 2)/(length(xx) - sum(!is.na(scan) & scan))
     )
# 92%
}
