# Packages ----------------------------------------------------------------


list.of.packages <- c("shiny"
    , "shinythemes" 
    , "rhandsontable"
    , "readr"
    , "knitr"
    , "summarytools"
    , "ggplot2"
    , "dplyr"
    , "smbinning"
    , "tidyr"
    , "car"
    , "shinyjqui"
    , "shinyhelper"
    , "pbapply"
    , "parallel"
    )

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(shiny)
library(shinythemes)
library(rhandsontable)
library(readr)
library(knitr)
library(summarytools)
library(ggplot2)
library(dplyr)
library(smbinning)
library(tidyr)
library(car)
library(shinyjqui)
library(shinyhelper)
library(pbapply)
library(parallel)

# Options -----------------------------------------------------------------

options(shiny.maxRequestSize = Inf)
options(shiny.minified = TRUE)
