# LCAP Scraper

This is a collection of scripts for extracting data from LCAPs.

## DSI Task List

* [x] Convert all PDFs to XML with pdftohtml.
* [ ] Extract Section 2 tables (from "Section 2" to "Annual Update")
  + [x] Locate Section 2
  + [x] Convert rects to lines
  + [ ] Detect cells formed by lines
  + [ ] Parse text in cells

## Questions

* Should we implement a new PDF scraper by generating R bindings to poppler, or
  should we continue to use pdftohtml?

## Examples

Row with 3 blank columns in actions table (p. 30):
```
data/Alameda_BerkeleyUnified_LCAP_2015.2018.pdf
```

LCAPs that do not have "Annual Update Instructions" after the first goals table
in section 2:
```
../xml/ButtevilleUnionElementary_LCAP_2015.2016.xml"
../xml/ContraCosta_MartinezUnified_LCAP_2015.2016.xml
../xml/DosPalosOroLomaJointUnified_LCAP_2015.2016.xml
../xml/ElDorado_MotherLodeUnionElementary_LCAP_2015.2018.xml
../xml/EvergreenUnion_LCAP_2015.2016.xml
../xml/FresnoUnified_LCAP_2015.2016.xml
../xml/HamiltonUnified_LCAP_2015.2016.xml
../xml/HowellMountainElementary_LCAP_2015.2016.xml
../xml/Jamula-DalzuraUnionElementary_LCAP_2015.2016.xml
../xml/KentfieldElementary_LCAP_2015.2016.xml
../xml/Mt.ShastaUnionElementary_LCAP_2015.2016.xml
../xml/NewJerusalemElementary_LCAP_2015.2016.xml
../xml/PortervilleUnified_LCAP_2015.2016.xml
../xml/RoundValleyUnified_LCAP_2015.2016.xml
../xml/SanLorenzoValleyUnified_LCAP_2015.2016.xml
../xml/SaugusUnion_LCAP_2015.2016.xml
../xml/SouthBayUnion_SanDiego_LCAP_2015.2016.xml
../xml/WeedUnionElementary_LCAP_2015.2016.xml
```
These are addressed by also searching for "Original" or "Section 3: Use of
Supplemental".

Section 2 of this LCAP can't be located with "State and/or Local Priorities"
because of an extra space:
```
../xml/MagnoliaElementary_LCAP_2015.2016.xml
```
This is addressed by also searching for "State and /or Local Priorities".

## Notes (Nick)

* Jacob and Sean confirmed that 2015 is higher priority and they are fine with
  cleaning the extracted text themselves.

## Notes (Duncan)

Currently
```r
source("funcs.R")
ff = list.files("pdf2htmlEX", full = TRUE)
z = getGoal1(ff[3])
```
This attempts to extract all the information for the first Goal in the
document. This returns
```r
names(z)
```
```
[1] "goal"       "priorities" "need"       "appliesTo"  "eamo"       "actions"   
```
This is:
* statement of the goal
* priorities from the top right corner
* "Identified Need"
* "Goal Applies to" divided into "Schools", "Applicable Pupil Subgroups"
* EAMO for each year
* actions/services for each year

The first 4 of these are currently hard-coded and will need some finesse.

We also need to capture the actual year in the LCAP Year row.


```r
ff = list.files("pdf2htmlEX", full = TRUE)
a = lapply(ff, function(f) tryCatch(getGoal1(f)))
```
The pdf2htmlEX/ClovisUnified_LCAP_2014.2017.html has a totally different format so this function fails.
Same for many. Note that these are 2014. Check with Jacob whether we want these?


Problems
~/DSIProjects/Jacob/Data/HTML/MillValley_LCAP_2015.2016.html  - 
  Misses the first line of the Actions table.
  Using the first row as the header.  Not enough space between these.
  Thinks there are 5 columns. One is all NAs.
  No EAMO picked up.
  Identified Need an Goal Applies to are wrong.

"~/DSIProjects/Jacob/Data/HTML/Larkspur-CorteMadera_LCAP_2015.2016.html"
a = getGoal1("~/DSIProjects/Jacob/Data/HTML/Larkspur-CorteMadera_LCAP_2015.2016.html")





Nick (Ulle) suggested using pdf2htmlEX as it gives more structure than the
current content of pdftohtml. (However, CVRead takes care of a lot of this.) If
pdf2htmlEX works, great. However, it is focused on presentation of the material
in the Web browser and not in providing the information for programmatic
access. As a result, the locations of the content boxes is not immediately
accessible to us. However, it is in CSS material within the document.

Nick and I did some exploration and it appears that this is close, but not
general enough for entirely general recovery of information. However, for
Jacob's documents, or for specific parts of them, it may be simple than a more
generic approach.


The package RCSS (git@github-omg:omegahat/RCSS.git) can help us recover the
locations.

For `HoriconElementary_LCAP_2015.2016.html`, the 3rd cell in the final row of
the first Actions/Services table (p15) has a line under  X All and the _ Low
income ....  pdf2htmlEx breaks this into two separate divs.  As a result, we
don't have 4 divs per row and now we have to figure out which go together.

The two div nodes are 
```html
<div class="c x1f y184 w14 h3a">
  <div class="t m0 x8 h3 y16b ff1 fs1 fc0 sc0 ls0 ws0">X <span class="ls11">All</span> </div>
</div> 


<div class="c x1f y184 w17 h39">
  <div class="t m0 x8 h3 y12c ff1 fs1 fc0 sc0 ls0 ws0">OR:<span class="fc1">-------</span> </div>
  <div class="t m0 x8 h3 y185 ff1 fs1 fc0 sc0 lsf ws0">  <span class="ls0"> Low Incom<span class="_ _0"/>e pupils </span></div>
  <div class="t m0 x8 h3 y12e ff1 fs1 fc0 sc0 lsf ws0">  <span class="ls0"> English Learners<span class="_ _0"/> </span></div>
  <div class="t m0 x8 h3 y12f ff1 fs1 fc0 sc0 lsf ws0">  <span class="ls0"> Foster Youth </span></div>
  <div class="t m0 x8 h3 y130 ff1 fs1 fc0 sc0 lsf ws0">  <span class="ls0"> Redesignated f<span class="_ _0"/>luent </span></div>
  <div class="t m0 x8 h3 y131 ff1 fs1 fc0 sc0 ls0 ws0">English proficient<span class="_ _0"/> </div>
  <div class="t m0 x8 h3 y13a ff1 fs1 fc0 sc0 lsf ws0">  <span class="ls0"> Other Subgroups: </span></div>
  <div class="t m0 x8 h3 y104 ff1 fs1 fc0 sc0 ls0 ws0">(Specify) </div>
  <div class="t m0 x8 h3 y103 ff1 fs1 fc0 sc0 ls0 ws0"> </div>
</div> 
```

We could use heuristic rules that say if a div node starts with OR:, then
combine it with its previous sibling.


Alternatively, we can use the class information to find the CSS:
```r
css  = getNodeSet(doc, "//head/style[@type = 'text/css']")
```
We can determine that the 3rd of these CSS nodes is the one with the
location/position information for the nodes. We have the RCSS package that use
libcroco to parse CSS files.

```r
library(RCSS)
css = readCSS(xmlValue(css[[3]]), asText = TRUE)
```

There are 1473 entries in this CSS.

See the `RCSS/inst/doc/guide.html` for an introduction to using RCSS.

Let's find the CSS for the first node
```r
node = getNodeSet(doc, "//div[@class = 'c x1f y184 w14 h3a']")
```
We use getCSSRules on this node:
```r
els = getCSSRules(node[[1]], css)
```
```
[[1]]
An object of class "RULESET"
Slot "ref":
<pointer: 0x7fd7487a30d0>


[[2]]
An object of class "RULESET"
Slot "ref":
<pointer: 0x7fd7494a0400>


[[3]]
An object of class "RULESET"
Slot "ref":
<pointer: 0x7fd745a26de0>


[[4]]
An object of class "RULESET"
Slot "ref":
<pointer: 0x7fd7494bd7e0>
```
We get 4 not 5 - the number of terms in the class. The reason for this is (most
likely) that the CSS element for the class c (.c) is in one of the other 2 CSS
blocks in the HTML file. We can resolve these.

We can convert each of these  CSS elements to  an R description with
asCSSObject()
```r
r = lapply(els, asCSSObject)
```
```
[[1]]
An object of class "CSSRuleset"
Slot "declarations":
$bottom
An object of class "CSSDeclaration"
Slot "important":
[1] FALSE

Slot "property":
[1] "bottom"

Slot "value":
[[1]]
LENGTH_PX 
   396.07 

Slot "selectors":
[[1]]
An object of class "CSSComplexSelector"
Slot "selectors":
[[1]]
An object of class "CSSClassSelector"
Slot "name":
[1] "y184"

Slot "is_case_sensitive":
[1] FALSE

Slot "name":
character(0)

Slot "specificity":
[1] 1000

[[2]]
An object of class "CSSRuleset"
Slot "declarations":
$height
An object of class "CSSDeclaration"
Slot "important":
[1] FALSE

Slot "property":
[1] "height"

Slot "value":
[[1]]
LENGTH_PX 
   116.54 

Slot "selectors":
[[1]]
An object of class "CSSComplexSelector"
Slot "selectors":
[[1]]
An object of class "CSSClassSelector"
Slot "name":
[1] "h3a"

Slot "is_case_sensitive":
[1] FALSE

Slot "name":
character(0)

Slot "specificity":
[1] 1000

[[3]]
An object of class "CSSRuleset"
Slot "declarations":
$width
An object of class "CSSDeclaration"
Slot "important":
[1] FALSE

Slot "property":
[1] "width"

Slot "value":
[[1]]
LENGTH_PX 
   118.94 

Slot "selectors":
[[1]]
An object of class "CSSComplexSelector"
Slot "selectors":
[[1]]
An object of class "CSSClassSelector"
Slot "name":
[1] "w14"

Slot "is_case_sensitive":
[1] FALSE

Slot "name":
character(0)

Slot "specificity":
[1] 1000

[[4]]
An object of class "CSSRuleset"
Slot "declarations":
$left
An object of class "CSSDeclaration"
Slot "important":
[1] FALSE

Slot "property":
[1] "left"

Slot "value":
[[1]]
LENGTH_PX 
   348.29 

Slot "selectors":
[[1]]
An object of class "CSSComplexSelector"
Slot "selectors":
[[1]]
An object of class "CSSClassSelector"
Slot "name":
[1] "x1f"

Slot "is_case_sensitive":
[1] FALSE

Slot "name":
character(0)

Slot "specificity":
[1] 1000
```

The default method of displaying this is not very convenient, but all the
information is there. Importantly, the c does resolve to absolute positioning.
And the 4 elements we resolved provide the bounding box coordinates:
```r
sapply(r, function(x) names(x@declarations))
```
```
[1] "bottom" "height" "width"  "left"  
```
So we have the locations of the div:
```r
pos1 = sapply(r, function(x) sapply(x@declarations, slot, "value"))
```
```
$bottom
LENGTH_PX 
   396.07 

$height
LENGTH_PX 
   116.54 

$width
LENGTH_PX 
   118.94 

$left
LENGTH_PX 
   348.29 
```
We know these are in pixels (`_PX`).

Now let's look at the next node. We'll get it manually:
```r
n2 = getNodeSet(doc, "//div[@class = 'c x1f y184 w17 h39']")
```
We resolve its CSS elements
```r
css2 = getCSSRules(n2[[1]], css)
```
and get the locations
```r
pos2 = sapply(css2, function(x) sapply(asCSSObject(x)@declarations, slot, "value"))
```
```
$bottom
LENGTH_PX 
   396.07 

$height
LENGTH_PX 
   116.53 

$width
LENGTH_PX 
   118.95 

$left
LENGTH_PX 
   348.29 
```

Since we have collapsed the pages, we now have a problem with the coordinates.

`HoriconElementary_LCAP_2015.2016.html` illustrates a row that spans 2 pages.
1.10 and also 1.14


To enhance pdftohtml's output, we need to find all the lines.

The ops from page 10 of Winto:
```r
sort(table(gsub("op = ", "", ops)))
```
```
 gs   d   j   l   m   S   w   G  Tc BDC EMC  Tf  f*   g  BT  ET  TJ  Tm   n  W*  re  cm  Do   q   Q 
  2  10  10  10  10  10  10  11  30  97  97 100 142 218 220 220 220 220 223 223 365 384 384 617 617 

(See the pdf_reference....pdf)
Operators
l   append straight line segment     opLineTo
S   stroke                           opStroke
f*  fill path using even-odd rule    opEOFill
re  append rectangle to path         opRectangle

Do  Invoke named object.             opXObject
W*  set clipping path                opEOClip


d, j, w, G, g - setting the line styles.
```

css unhandled conversion
```r
o = invisible(lapply(
  getCSSRules(
    getNodeSet(doc, "//div[starts-with(.,'Complete a copy')]")[[1]], css
  ), asCSSObject))
```

## Older Notes

867 different school districts and so that many documents.

Initially looking for the Annual outcomes. These are in the tables with the row
header Annual Outcomes.

It takes some time to convert a pdf document to XML, but only needs to be done
once. We can do these in parallel. So on a machine with 8 cores, we might be
able to do 

For pdf2txt, I did one 100 page document that was dense in text (a full text
report, not a table) in 1.5 minutes. So 108 of these would take almost 3 hours,
but all 867 would take about the same time in total when done in parallel. So
probably no more than 4 hours. If dones sequentially, 22 hours.

We also have to add on time to combine the individual symbols iinto
sentences/phrases and dos some of the computations to merge the contents. For
the 157 page Davis report, this took 11 seconds. So all 867 would be about 3
hours.

Extracting the Annual Outcomes is essentially no time, i.e. .2 of a second for
157 pages.

