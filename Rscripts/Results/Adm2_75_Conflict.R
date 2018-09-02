
## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
rm(list = ls())
gc()
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


## Rain & Temp regs ----

regA <- felm(Conf.events ~ Temp.shock + Temp.shock.lag + Rain.shock + Rain.shock.lag|year.f + unit.name.f|0|admin2.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
regB <- felm(Conf.events ~ Temp.shock + Temp.shock.lag + Rain.shock + Rain.shock.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Conf.events ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag |year.f + unit.name.f|0|admin2.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Conf.events ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Conf.events ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + unit.name.f|0|admin2.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg4 <- felm(Conf.events ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])


stargazer(regA, regB, reg1, reg2, reg3, reg4,
          type = "html", out = "/data/Data/Output/Adm2_75/raintemp_anomaly_status6_Conf.events.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 6)),
                           c("Year FE", rep("Yes", 6)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 3))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")


## SPEI4 regressions ----

regA <- felm(Conf.events ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel75)
regB <- felm(Conf.events ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
reg1 <- felm(Conf.events ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg2 <- felm(Conf.events ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)

stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_Conf.events.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Conf.events"
ind.var <- "SPEI4"
yaxis <- c(-0.06, 0.06)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_Conf.events.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on the number of conflict events, by ethnic group status")
notes.label <- "The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level."
source("~/Rscripts/Results/MEfigure.R")


## SPEI6 & SPEI12 regressions ----

regA <- felm(Conf.events ~ SPEI6 + SPEI6.lag|year.f + unit.name.f|0|admin2.f,
             Panel75)
regB <- felm(Conf.events ~ SPEI6 + SPEI6.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
reg1 <- felm(Conf.events ~ SPEI6 + SPEI6.lag + SPEI6:status6 + SPEI6.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg2 <- felm(Conf.events ~ SPEI6 + SPEI6.lag + SPEI6:status6 + SPEI6.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)

stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI6_status6_Conf.events.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Conf.events"
ind.var <- "SPEI6"
yaxis <- c(-0.06, 0.06)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_Conf.events.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on the number of conflict events, by ethnic group status")
notes.label <- "The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level."
source("~/Rscripts/Results/MEfigure.R")

regA <- felm(Conf.events ~ SPEI12 + SPEI12.lag|year.f + unit.name.f|0|admin2.f,
             Panel75)
regB <- felm(Conf.events ~ SPEI12 + SPEI12.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
reg1 <- felm(Conf.events ~ SPEI12 + SPEI12.lag + SPEI12:status6 + SPEI12.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg2 <- felm(Conf.events ~ SPEI12 + SPEI12.lag + SPEI12:status6 + SPEI12.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)

stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI12_status6_Conf.events.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Conf.events"
ind.var <- "SPEI12"
yaxis <- c(-0.06, 0.06)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_Conf.events.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on the number of conflict events, by ethnic group status")
notes.label <- "The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level."
source("~/Rscripts/Results/MEfigure.R")



## ____ ----


## Other conflict variables ----
names(Panel75)

reg1 <- felm(Conflict ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
reg2 <- felm(Any.Deaths ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
reg3 <- felm(Deaths.max ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)
reg4 <- felm(Deaths.best ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)

stargazer(reg1, reg2, reg3, reg4,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_OtherConfVars.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")


