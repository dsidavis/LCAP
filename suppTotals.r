source("R2/section3.R")
library(ReadPDF)
library(XML)
xml = list.files("../Jacob/Data", pattern = ".*2015\\.2016\\.xml$", full = TRUE)
tm1 = system.time({ totals = lapply(xml,  function(x) {print(x); try(getSupplementalTotal(x))}) })


