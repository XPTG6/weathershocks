
## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
rm(list = ls())
`%nin%` <- function(a, b){! a %in% b}
ProportionNA <- function(x){x[, lapply(.SD, function(y){sum(is.na(y))/length(y)})]}
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "gridExtra", "SPEI", "RcppRoll",
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


## Load Panel (Adm1 .75 res) ----

load("/data/Data/Adm1_75/Panel75_Final")

## ____ ---- 

## Preparation ----

setkey(Panel75, unit.name, year)

## Fix unit.name of unit exchanging country
x <- Panel75[, length(unique(NAME_0))>1, by = unit.name][V1 == T, unit.name]
# View(Panel75[unit.name %in% x])
Panel75[unit.name %in% x, unit.name := paste0(unit.name, "_", NAME_0)]


Panel75[, Rain.km := Rain/areasqkm]
Panel75[, Light.cap := Light/Pop1990]

Panel75[, unit.name.f := as.factor(unit.name)]
Panel75[, year.f := as.factor(year)]
Panel75[, country.f := as.factor(NAME_0)]
Panel75[, admin1.f := as.factor(NAME_1)]
Panel75[, year.country.f := interaction(year.f, country.f)]

Panel75[year >1992, time.trend := as.double(seq_len(length(year))), by = unit.name]

# Pre-treatment status:
Panel75[year == 1992 & status %nin% c("STATE COLLAPSE", "IRRELEVANT", "SELF-EXCLUSION"), status6 := status]
Panel75[, status6 := na.locf(status6, na.rm = F), by = unit.name]
Panel75[status6 %in% c("JUNIOR PARTNER", "SENIOR PARTNER"),
        `:=` (status2 = "included",
              status3 = "shared")]
Panel75[status6 %in% c("DOMINANT", "MONOPOLY"),
        `:=` (status2 = "included",
              status3 = "dominant")]
Panel75[status6 %in% c("POWERLESS", "DISCRIMINATED"),
        `:=` (status2 = "excluded",
              status3 = "excluded")]
# Irrelevant, state collapse, and self-exclusion (none in sample) as NA

# Light growth:
light.shift <- Panel75[Light.cap>0, min(Light.cap)]
Panel75[, Light.cap.adj := Light.cap + light.shift]
Panel75[,Light.growth := log(Light.cap.adj) - log(data.table::shift(Light.cap.adj,
                                                                    type = "lag")),
        by = unit.name]

# View(Panel75[year>1992 & is.na(Light.growth)]) # first year of new units (etnhnoadmins)
# Panel75[year>1992 & !is.na(Light.growth), quantile(Light.growth, seq(0, 1, 0.05))]


## Weather anomaly measures
Panel75[, Rain.shock := (Rain.km-Rain.LRmean)/Rain.LRsd]
Panel75[, Temp.shock := (Temp-Temp.LRmean)/Temp.LRsd]


## DEPRECATED b/c distribution looks OK after dealing with LR values properly.
# # Set outliers in Temp.shock and Rain.shock to NA
# Panel75[!is.na(Temp.shock), `:=` (Temp.shock.Q99 = quantile(Temp.shock, .99),
#                                   Temp.shock.Q01 = quantile(Temp.shock, .01))]
# Panel75[!is.na(Rain.shock), `:=` (Rain.shock.Q99 = quantile(Rain.shock, .99),
#                                   Rain.shock.Q01 = quantile(Rain.shock, .01))]
# 
# Panel75[Temp.shock < Temp.shock.Q01 | Temp.shock > Temp.shock.Q99,
#         `:=` (Temp.shock = NA,
#               Rain.shock = NA)]
# Panel75[Rain.shock < Rain.shock.Q01 | Rain.shock > Rain.shock.Q99,
#         `:=` (Temp.shock = NA,
#               Rain.shock = NA)]


## Lags
Panel75[, Temp.shock.lag := data.table::shift(Temp.shock, 1, type = "lag"), by = unit.name]
Panel75[, Rain.shock.lag := data.table::shift(Rain.shock, 1, type = "lag"), by = unit.name]
Panel75[, SPEI4.lag := data.table::shift(SPEI4, 1, type = "lag"), by = unit.name]
Panel75[, SPEI6.lag := data.table::shift(SPEI6, 1, type = "lag"), by = unit.name]
Panel75[, SPEI12.lag := data.table::shift(SPEI12, 1, type = "lag"), by = unit.name]

## Leads
Panel75[, Temp.shock.lead := data.table::shift(Temp.shock, 1, type = "lead"), by = unit.name]
Panel75[, Rain.shock.lead := data.table::shift(Rain.shock, 1, type = "lead"), by = unit.name]
Panel75[, SPEI4.lead := data.table::shift(SPEI4, 1, type = "lead"), by = unit.name]
Panel75[, SPEI6.lead := data.table::shift(SPEI6, 1, type = "lead"), by = unit.name]
Panel75[, SPEI12.lead := data.table::shift(SPEI12, 1, type = "lead"), by = unit.name]

## Exclude Northern Africa:
Panel75 <- Panel75[!NAME_0 %in% c("Morocco", "Tunisia", "Egypt", "Algeria", "Libya")]

# Exclude pre-1992:
Panel75 <- Panel75[year > 1992 & !is.na(Light.growth)]

# Exclude NAs in status6 (irrelevant, state collapse):
Panel75 <- Panel75[!is.na(status6)]


Panel75[, status6 := relevel(status6, "JUNIOR PARTNER")]
Panel75[, year := as.double(year)]

## ____ ----

## Rain & Temp regs ----

regA <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
regB <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag |year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg4 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm1_75/rain_anomaly_status6.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")

stargazer(regA, regB, reg3, reg4,
          type = "html", out = "/data/Data/Output/Adm1_75/temp_anomaly_status6.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")




regA <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
regB <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status3 + Rain.shock.lag:status3 + Temp.shock + Temp.shock.lag |year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status3 + Rain.shock.lag:status3 + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status3 + Temp.shock.lag:status3 + Rain.shock + Rain.shock.lag|year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg4 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status3 + Temp.shock.lag:status3 + Rain.shock + Rain.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])


stargazer(regA, regB, reg1, reg2,reg3, reg4,
          type = "html", out = "/data/Data/Output/Adm1_75/rain_anomaly_status3.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 6)),
                           c("Year FE", rep("Yes", 6)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 3))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")

regA <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
regB <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status2 + Rain.shock.lag:status2 + Temp.shock + Temp.shock.lag |year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status2 + Rain.shock.lag:status2 + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status2 + Temp.shock.lag:status2 + Rain.shock + Rain.shock.lag|year.f + unit.name.f|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg4 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status2 + Temp.shock.lag:status2 + Rain.shock + Rain.shock.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)])


stargazer(regA, regB, reg1, reg2, reg3, reg4,
          type = "html", out = "/data/Data/Output/Adm1_75/rain_anomaly_status2.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 6)),
                           c("Year FE", rep("Yes", 6)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 3))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")


## SPEI4 regressions ----

regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin1.f,
             Panel75)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin1.f,
             Panel75)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm1_75/SPEI4_status6.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")


regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin1.f,
             Panel75)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status3 + SPEI4.lag:status3|year.f + unit.name.f|0|admin1.f,
             Panel75)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status3 + SPEI4.lag:status3|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm1_75/SPEI4_status3.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")


regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin1.f,
             Panel75)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status2 + SPEI4.lag:status2|year.f + unit.name.f|0|admin1.f,
             Panel75)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status2 + SPEI4.lag:status2|year.f + unit.name.f + admin1.f : time.trend|0|admin1.f,
             Panel75)

stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm1_75/SPEI4_status2.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 1 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-1 administrative areas.")
