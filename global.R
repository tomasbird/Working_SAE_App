library(dplyr)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(sp)
library(rgdal)

## Sample spatial data
clusters=readOGR("R/Shapefiles/NPGE61FL.shp")
adm0=readOGR("R/Shapefiles/npl_l02_1998.shp")
#subreg=readOGR("R/Shapefiles/npl_l04_2003_FPsubregion2011.shp")
#districts=readOGR("R/Shapefiles/npl_l05_2003_FPdistrict2011.shp")

## load DHS
DHS_vars=read.csv(file="R/data/DHS_vars.csv")
DHS_ind=read.csv(file="R/data/DHS_Ind.csv")

# load census
census_vars=read.csv(file="R/data/census_vars.csv")
census_ind=read.csv(file="R/data/census_Ind.csv")


datvars <- c(
  "Adm1" = "adm1",
  "Adm2" = "adm2",
  "EAs" = "eas")

