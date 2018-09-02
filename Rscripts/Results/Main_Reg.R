
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


## Load Panel (Adm2 Interpolated) ----

load("/data/Data/Robjects/Panel_Final")

## ____ ---- 

## Preparation ----

setkey(Panel, unit.name, year)

Panel[, Rain.km := Rain/areasqkm]
Panel[, Light.cap := Light/Pop1990]

Panel[, year.f := as.factor(year)]
Panel[, country.f := as.factor(NAME_0)]
Panel[, admin2.f := as.factor(NAME_2)]
Panel[, year.country.f := interaction(year.f, country.f)]

# Pre-treatment status:
Panel[year == 1992 & status %nin% c("STATE COLLAPSE", "IRRELEVANT", "SELF-EXCLUSION"), status6 := status]
Panel[, status6 := na.locf(status6, na.rm = F), by = unit.name]
Panel[status6 %in% c("JUNIOR PARTNER", "SENIOR PARTNER"),
      `:=` (status2 = "included",
            status3 = "shared")]
Panel[status6 %in% c("DOMINANT", "MONOPOLY"),
      `:=` (status2 = "included",
            status3 = "dominant")]
Panel[status6 %in% c("POWERLESS", "DISCRIMINATED"),
      `:=` (status2 = "excluded",
            status3 = "excluded")]
# Irrelevant, state collapse, and self-exclusion (none in sample) as NA

# Light growth:
light.shift <- Panel[Light.cap>0, min(Light.cap)]
Panel[, Light.cap.adj := Light.cap + light.shift]
Panel[,Light.growth := log(Light.cap.adj) - log(data.table::shift(Light.cap.adj,
                                                       type = "lag")),
      by = unit.name]

# View(Panel[year>1992 & is.na(Light.growth)]) # first year of new units (etnhnoadmins)
# Panel[year>1992 & !is.na(Light.growth), quantile(Light.growth, seq(0, 1, 0.05))]

# Long run (pre-study) weather means:
Panel[year < 1992, Rain.LRmean := mean(Rain.km, na.rm = T), by = unit.name]
Panel[, Rain.LRmean := na.locf(Rain.LRmean, na.rm = F), by = unit.name]
Panel[year < 1992, Temp.LRmean := mean(Temp, na.rm = T), by = unit.name]
Panel[, Temp.LRmean := na.locf(Temp.LRmean, na.rm = F), by = unit.name]

# Long run SDs:
Panel[year < 1992, Rain.LRsd := sd(Rain.km, na.rm = T), by = unit.name]
Panel[, Rain.LRsd := na.locf(Rain.LRsd, na.rm = F), by = unit.name]
Panel[year < 1992, Temp.LRsd := sd(Temp, na.rm = T), by = unit.name]
Panel[, Temp.LRsd := na.locf(Temp.LRsd, na.rm = F), by = unit.name]

if(nrow(Panel[is.infinite(Temp.LRmean) |
              is.infinite(Temp.LRsd) |
              is.infinite(Rain.LRmean) |
              is.infinite(Rain.LRsd)]) > 0)stop("Infinite long-run values")

## Weather anomaly measures
Panel[, Rain.shock := (Rain.km-Rain.LRmean)/Rain.LRsd]
Panel[, Temp.shock := (Temp-Temp.LRmean)/Temp.LRsd]

# Set outliers in Temp.shock and Rain.shock to NA
Panel[!is.na(Temp.shock), `:=` (Temp.shock.Q99 = quantile(Temp.shock, .99),
                                  Temp.shock.Q01 = quantile(Temp.shock, .01))]
Panel[!is.na(Rain.shock), `:=` (Rain.shock.Q99 = quantile(Rain.shock, .99),
                                  Rain.shock.Q01 = quantile(Rain.shock, .01))]

Panel[Temp.shock < Temp.shock.Q01 | Temp.shock > Temp.shock.Q99,
        `:=` (Temp.shock = NA,
              Rain.shock = NA)]
Panel[Rain.shock < Rain.shock.Q01 | Rain.shock > Rain.shock.Q99,
        `:=` (Temp.shock = NA,
              Rain.shock = NA)]

## Lags
Panel[, Temp.shock.lag := data.table::shift(Temp.shock, 1, type = "lag"), by = unit.name]
Panel[, Rain.shock.lag := data.table::shift(Rain.shock, 1, type = "lag"), by = unit.name]

## Leads
Panel[, Temp.shock.lead := data.table::shift(Temp.shock, 1, type = "lead"), by = unit.name]
Panel[, Rain.shock.lead := data.table::shift(Rain.shock, 1, type = "lead"), by = unit.name]

## Exclude Northern Africa:
Panel <- Panel[!NAME_0 %in% c("Morocco", "Tunisia", "Egypt", "Algeria", "Libya")]

# Exclude pre-1992:
Panel <- Panel[year > 1992 & !is.na(Light.growth)]

# Exclude NAs in status6 (irrelevant, state collapse):
Panel <- Panel[!is.na(status6)]

Panel <- Panel[, .(unit.name,
                   year, year.f, country.f, admin2.f, year.country.f,
                   Light.growth,
                   Rain.shock, Temp.shock,
                   Rain.shock.lag, Temp.shock.lag,
                   Light.cap,
                   status6, status2, status3)]

Panel[, status6 := relevel(status6, "JUNIOR PARTNER")]
Panel[, year := as.double(year)]

## Rain regressions ----

# senior partner effect visible if using rain.km, disappears with shock measure
reg0 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + admin2.f + year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag |year.f + admin2.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag|year.f + admin2.f + country.f : year.f|0|admin2.f,
              Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])

stargazer(reg0, reg1, reg2, reg3,
          type = "html", out = "/data/Data/Output/Main/rain_anomaly_status6.html")

reg0 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + admin2.f + year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status3 + Rain.shock.lag:status3 + Temp.shock + Temp.shock.lag |year.f + admin2.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status3 + Rain.shock.lag:status3 + Temp.shock + Temp.shock.lag|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status3 + Rain.shock.lag:status3 + Temp.shock + Temp.shock.lag|year.f + admin2.f + country.f : year.f|0|admin2.f,
              Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])

stargazer(reg0, reg1, reg2, reg3,
          type = "html", out = "/data/Data/Output/Main/rain_anomaly_status3.html")

reg0 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + admin2.f + year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status2 + Rain.shock.lag:status2 + Temp.shock + Temp.shock.lag |year.f + admin2.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status2 + Rain.shock.lag:status2 + Temp.shock + Temp.shock.lag|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status2 + Rain.shock.lag:status2 + Temp.shock + Temp.shock.lag|year.f + admin2.f + country.f : year.f|0|admin2.f,
              Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])

stargazer(reg0, reg1, reg2, reg3,
          type = "html", out = "/data/Data/Output/Main/rain_anomaly_status2.html")

## Temp regressions ----

reg0 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Rain.shock + Rain.shock.lag|year.f + admin2.f + year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + admin2.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + admin2.f + country.f : year.f|0|admin2.f,
              Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])

stargazer(reg0, reg1, reg2, reg3,
          type = "html", out = "/data/Data/Output/Main/temp_anomaly_status6.html")


reg0 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Rain.shock + Rain.shock.lag|year.f + admin2.f + year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status3 + Temp.shock.lag:status3 + Rain.shock + Rain.shock.lag|year.f + admin2.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status3 + Temp.shock.lag:status3 + Rain.shock + Rain.shock.lag|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status3 + Temp.shock.lag:status3 + Rain.shock + Rain.shock.lag|year.f + admin2.f + country.f : year.f|0|admin2.f,
              Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])

stargazer(reg0, reg1, reg2, reg3,
          type = "html", out = "/data/Data/Output/Main/temp_anomaly_status3.html")


reg0 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Rain.shock + Rain.shock.lag|year.f + admin2.f + year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status2 + Temp.shock.lag:status2 + Rain.shock + Rain.shock.lag|year.f + admin2.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status2 + Temp.shock.lag:status2 + Rain.shock + Rain.shock.lag|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status2 + Temp.shock.lag:status2 + Rain.shock + Rain.shock.lag|year.f + admin2.f + country.f : year.f|0|admin2.f,
              Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])

stargazer(reg0, reg1, reg2, reg3,
          type = "html", out = "/data/Data/Output/Main/temp_anomaly_status2.html")

