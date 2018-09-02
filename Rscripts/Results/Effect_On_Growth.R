
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


## SPEI4 regressions ----

reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel75)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel75)


dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, ".png")
manual.title <- F
source("~/Rscripts/Results/MEfigure.R")

## Calculating effect on growth differential
growth.diff <- plot.table[Reg == 2, .(Status, ME.tot)]
growth.diff <- merge(growth.diff,
                     Panel75[, .(Mean.Growth = mean(Light.growth)),
                             by = .(Status = status6)],
                     by = "Status")
growth.diff <- merge(growth.diff,
                     Panel75[, .(SD.SPEI4 = sd(SPEI4)),
                             by = .(Status = status6)],
                     by = "Status")
growth.diff <- merge(growth.diff, 
                     Panel75[, .(SD.SPEI4 = sd(SPEI4)),
                             by = .(Status = status6, unit.name)][
                               !is.na(SD.SPEI4),
                               .(Temporal.SD.SPEI4 = mean(SD.SPEI4)),# This is more like the temporal standard deviation of the mean subdivision
                               by = Status],
                     by = "Status")
growth.diff[, Change.Growth := (-SD.SPEI4)*ME.tot]
growth.diff[, Change.Growth.TSDO := (-Temporal.SD.SPEI4)*ME.tot]

# Monopoly & discriminated
growth.diff[, Mean.Growth[Status=="MONOPOLY"] - Mean.Growth[Status=="DISCRIMINATED"]] # Old growth difference
growth.diff[, (Mean.Growth[Status=="MONOPOLY"] + Change.Growth[Status=="MONOPOLY"]) -
              (Mean.Growth[Status=="DISCRIMINATED"] + Change.Growth[Status=="DISCRIMINATED"])] # New growth difference
growth.diff[, Change.Growth[Status=="MONOPOLY"] - Change.Growth[Status=="DISCRIMINATED"]] # Difference btw the two

# Monopoly & powerless
growth.diff[, Mean.Growth[Status=="MONOPOLY"] - Mean.Growth[Status=="POWERLESS"]] # Old growth difference
growth.diff[, (Mean.Growth[Status=="MONOPOLY"] + Change.Growth[Status=="MONOPOLY"]) -
              (Mean.Growth[Status=="POWERLESS"] + Change.Growth[Status=="POWERLESS"])] # New growth difference
growth.diff[, Change.Growth[Status=="MONOPOLY"] - Change.Growth[Status=="POWERLESS"]] # Difference btw the two


## But that includes geographical sd... what if only temporal sd?
# Monopoly & discriminated
growth.diff[, Mean.Growth[Status=="MONOPOLY"] - Mean.Growth[Status=="DISCRIMINATED"]] # Old growth difference
growth.diff[, (Mean.Growth[Status=="MONOPOLY"] + Change.Growth.TSDO[Status=="MONOPOLY"]) -
              (Mean.Growth[Status=="DISCRIMINATED"] + Change.Growth.TSDO[Status=="DISCRIMINATED"])] # New growth difference
growth.diff[, Change.Growth.TSDO[Status=="MONOPOLY"] - Change.Growth.TSDO[Status=="DISCRIMINATED"]] # Difference btw the two

# Monopoly & powerless
growth.diff[, Mean.Growth[Status=="MONOPOLY"] - Mean.Growth[Status=="POWERLESS"]] # Old growth difference
growth.diff[, (Mean.Growth[Status=="MONOPOLY"] + Change.Growth.TSDO[Status=="MONOPOLY"]) -
              (Mean.Growth[Status=="POWERLESS"] + Change.Growth.TSDO[Status=="POWERLESS"])] # New growth difference
growth.diff[, Change.Growth.TSDO[Status=="MONOPOLY"] - Change.Growth.TSDO[Status=="POWERLESS"]] # Difference btw the two

## negligible difference!

