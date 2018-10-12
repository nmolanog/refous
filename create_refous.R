rm(list=ls())
library(devtools)
library(roxygen2)
library(here)
#setwd("/home/nicolas/Desktop")
#create("refous")
###insert file with functions in R folder
setwd("./refous")
document()

setwd("..")
install("refous")
library(refous)
