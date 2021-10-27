#install required packages
# install.packages(c("ggplot2", "readxl", "data.table", "extrafont", "devtools", "equivalence"))

#loading installed packages
library(extrafont); library(ggplot2); library(readxl); library(data.table)
library("BiophaRm")

#set a working directory
# setwd("D:\\BiosimilaR example\\")


bioplot.jitter(filename="tier2 example data.xlsx",   #load excel data
            start=2,                                 #set a no. of sheet to start plotting
            end=2,                                   #set a no. of sheet to end plotting
            ref.name="Reference")                    #set a sample to calculate a quality range

bioplot.box(filename="tier2 example data.xlsx",
         start=2,
         end=2,
         ref.name="Reference")


biotest.tost(filename="experiment data.xlsx",
      sheet.no=1,
      test.name="biosimilar",
      ref.name="reference",
      method.name = "in vivo assay",
      ci=0.90,
      paired=FALSE)


