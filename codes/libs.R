# Packages ----------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinythemes, rhandsontable, readr, knitr, summarytools, ggplot2, dplyr, smbinning, tidyr, car)

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

# Options -----------------------------------------------------------------

options(shiny.maxRequestSize = Inf)
options(shiny.minified = TRUE)
