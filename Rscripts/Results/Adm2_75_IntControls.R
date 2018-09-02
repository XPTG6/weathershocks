
## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
rm(list = ls())
`%nin%` <- function(a, b){! a %in% b}
ProportionNA <- function(x){x[, lapply(.SD, function(y){sum(is.na(y))/length(y)})]}
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "grid", "gridExtra",
              "lemon", "SPEI", "RcppRoll",
              "lfe", "rworldmap", "sp", "RCurl", "grid", "foreign", "xlsx", "stargazer",
              "maptools", "maps", "shinydashboard", "DT", "reshape2", "plotly", "lubridate",
              "devtools", "rNOMADS", "shinyjs", "dendextend", "ggdendro",
              "raster", "ncdf4", "chron", "lubridate", "rasterVis", "rgdal", "geomerge",
              "Rcpp", "countrycode", "snow", "cleangeo", "ggmap"#, "mapview",
)
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)
# note sf fails to load unless it's done first in a new session

detach('package:shinyjs') # to avoid conflict with raster::print

## Load Panel (Adm2 0.75 res) ----

load("/data/Data/Adm2_75/Panel75_Final")

## Preparation ----

which.pop <- "Pop1990"
source("~/Rscripts/Prep/Adm2_75_PanelPrep.R")

## ____ ----

## SPEI4 regressions ----


# Conflict events
reg2.A <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:Pre.Conflict + SPEI4.lag:Pre.Conflict|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
# Deaths
reg2.B <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Pre.Deaths + SPEI4.lag:Pre.Deaths|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)
# Institutional veto players
reg2.C <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Pre.Checks + SPEI4.lag:Pre.Checks |year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)
# Agricultural area
reg2.D <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Pre.agri.irrigated.cap + SPEI4.lag:Pre.agri.irrigated.cap +
                 SPEI4:Pre.agri.rainfed.cap + SPEI4.lag:Pre.agri.rainfed.cap|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)
# Country GDP
reg2.E <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Pre.Lights.Country + SPEI4.lag:Pre.Lights.Country |year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)
# Ethnic group
reg2.F <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Pre.Group.Size + SPEI4.lag:Pre.Group.Size |year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)
# LR temp & rain
reg2.G <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Temp.LRmean + SPEI4.lag:Temp.LRmean +
                 SPEI4:Rain.LRmean + SPEI4.lag:Rain.LRmean|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)

# All
reg2.H <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6 +
                 SPEI4:Pre.Deaths + SPEI4.lag:Pre.Deaths +
                 SPEI4:Pre.Checks + SPEI4.lag:Pre.Checks +
                 SPEI4:Pre.agri.irrigated.cap + SPEI4.lag:Pre.agri.irrigated.cap +
                 SPEI4:Pre.agri.rainfed.cap + SPEI4.lag:Pre.agri.rainfed.cap +
                 SPEI4:Pre.Lights.Country + SPEI4.lag:Pre.Lights.Country +
                 SPEI4:Pre.Group.Size + SPEI4.lag:Pre.Group.Size +
                 SPEI4:Temp.LRmean + SPEI4.lag:Temp.LRmean +
                 SPEI4:Rain.LRmean + SPEI4.lag:Rain.LRmean|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
               Panel75)


stargazer(reg2.A, reg2.B, reg2.C, reg2.D, reg2.E, reg2.F, reg2.G, reg2.H,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_IntControls.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 9)),
                           c("Year FE", rep("Yes", 9)),
                           c("Admin. 2 trends", rep("Yes", 9)),
                           c("Inter. Controls", "Conf. Ev.", "Deaths", "Inst.", "Agri.",
                             "GDP", "Group size", "LR Clim.", "All" )),
          omit        = c("Pre"),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")







## ____ ----

## Inter. FEs ----

Panel75[, country.f := relevel(country.f, "Kenya")]

# Country level
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag +
               SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:year.f + 
               SPEI4.lag:year.f  +
               country.f:SPEI4 + country.f:SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag +
               SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:year.f + 
               SPEI4.lag:year.f +
               country.f:SPEI4 + country.f:SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
# Admin 1
reg3 <- felm(Light.growth ~ SPEI4 + SPEI4.lag +
               SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:year.f + 
               SPEI4.lag:year.f  +
               admin1.f:SPEI4 + admin1.f:SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg4 <- felm(Light.growth ~ SPEI4 + SPEI4.lag +
               SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:year.f + 
               SPEI4.lag:year.f +
               admin1.f:SPEI4 + admin1.f:SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
# Country w. time trends
reg5 <- felm(Light.growth ~ SPEI4 + SPEI4.lag +
               SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:year.f + 
               SPEI4.lag:year.f  +
               country.f:SPEI4 + country.f:SPEI4.lag +
               country.f:time.trend:SPEI4 + country.f:time.trend:SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg6 <- felm(Light.growth ~ SPEI4 + SPEI4.lag +
               SPEI4:status6 + SPEI4.lag:status6 +
               SPEI4:year.f + 
               SPEI4.lag:year.f +
               country.f:SPEI4 + country.f:SPEI4.lag +
               country.f:time.trend:SPEI4 + country.f:time.trend:SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_InterFE.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 6)),
                           c("Year FE", rep("Yes", 6)),
                           c("Admin. 2 trends", "Yes", "No", "Yes", "No", "Yes", "No"),
                           c("Inter. Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Inter. Country FE", "Yes", "Yes", "No", "No", "Yes", "Yes"),
                           c("Inter. Admin. 1 FE", "No", "No", "Yes", "Yes", "No", "No"),
                           c("Inter. Country trends", "No", "No", "No", "No", "Yes", "Yes")),
          omit        = c("country", "admin1", "year"),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

