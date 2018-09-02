
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


## Preparation ----
which.pop <- "Pop1990"
load("/data/Data/Adm2_75/Panel75_Final")
source("~/Rscripts/Prep/Adm2_75_PanelPrep.R")

## Robust to gas flares ----
Panel.nogas <- copy(Panel75[NAME_0 %nin% c("Equatorial Guinea", "Nigeria", "Angola", "Gabon")])
regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel.nogas)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.nogas)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel.nogas)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.nogas)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_NoGas.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_NoGas.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status\n[1990 population, exc. Nigeria, Angola, Gabon, Eq. Guinea]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")



## Robust to Rwanda ----
Panel.noRwa <- copy(Panel75[NAME_0 %nin% c("Rwanda")])
regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel.noRwa)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.noRwa)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel.noRwa)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.noRwa)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_NoRwanda.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_NoRwanda.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status\n[1990 population, exc. Rwanda]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")


## Robust to South Africa ----
Panel.noSA <- copy(Panel75[NAME_0 %nin% c("South Africa")])
regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel.noSA)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.noSA)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel.noSA)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.noSA)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_NoSouthAfrica.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_NoSouthAfrica.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status\n[1990 population, exc. South Africa]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")