
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
              "Rcpp", "sf", "countrycode", "snow", "cleangeo", "ggmap"#, "mapview",
)
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)
# note sf fails to load unless it's done first in a new session

detach('package:shinyjs') # to avoid conflict with raster::print



## Load Panel (Adm2 0.75 res) ----

load("/data/Data/Adm2_75/Panel75_Final")

## Checking polynomials ----

ggplot(Panel75) +
  geom_smooth(aes(y = Pop, x = year), method = "glm", formula = y~x) +
  geom_smooth(aes(y = Pop, x = year), method = "glm", formula = y~x+I(x^2), col = alpha("red", 0.6))

stargazer(lm(Pop ~ year, Panel75),
          lm(Pop ~ year + I(year^2), Panel75),
          type = "text")

## Creating table (+ example) ----

Pop.Table <- data.table(expand.grid(unit.name = Panel75[, unique(unit.name)],
                         year = seq(1990, 2015, 5)))
setkey(Pop.Table, unit.name, year)
Pop.Table <- merge(Pop.Table, Panel75[, .(Pop2015 = Pop2015[.N]), by = unit.name], by = "unit.name")
Pop.Table <- merge(Pop.Table, Panel75[, .(unit.name, year, Pop)], by = c("unit.name", "year"), all.x = T)
Pop.Table[year == 2015, Pop := Pop2015]
Pop.Table <- Pop.Table[!is.na(Pop)]
Pop.Table[year >= 2000, Post.2000 := 1]
Pop.Table[is.na(Post.2000), Post.2000 := 0]

## Example: linear prediction with dummy
example <- merge(data.table(Pred.Pop = predict(lm(Pop ~ year + Post.2000 + year:Post.2000, Pop.Table[unit.name == "AGO.1.1_1.Bakongo"]),
                                    data.table(year = c(1990:2015), Post.2000 = c(0,0,1,1,1,1))),
                 data.table(year = c(1990:2015))),
Pop.Table[unit.name == "AGO.1.1_1.Bakongo", .(year, Pop)],
by = "year",
all.x = T)

## EX2: comparing linear interpolation with linear prediction 
example2 <- merge(data.table(Pred.Pop = predict(lm(Pop ~ year, Pop.Table[unit.name == "AGO.1.1_1.Bakongo"]),
                                               data.table(year = c(1990:2015))),
                            data.table(year = c(1990:2015))),
                 Pop.Table[unit.name == "AGO.1.1_1.Bakongo", .(year, Pop)],
                 by = "year",
                 all.x = T)
example2[, Approx.Pop := na.approx(Pop, x = year)]

ggplot(example2) +
  geom_point(aes(x = year, y = Pop)) +
  geom_point(aes(x=year, y = Pred.Pop), colour = alpha("blue", 0.4)) +
  geom_point(aes(x=year, y = Approx.Pop), colour = alpha("red", 0.4))
## ---> Use linear interpolation (linear prediction will largely be captured by admin-specific time trends)

## Run model by unit.name and store in Pop.Table ----

PredPop.Table <- Pop.Table[, as.list(predict(lm(Pop ~ year + Post.2000 + year:Post.2000),
                                             data.table(year = c(1990:2015), Post.2000 = c(0,0,1,1,1,1)))),
                           by = unit.name]

setnames(PredPop.Table, as.character(1:26), paste0("Pred.Pop.", 1990:2015))

Panel75 <- merge(Panel75, PredPop.Table, by = "unit.name")

for(i in 1990:2013){
  Panel75[year == i, Pred.Pop := eval(parse(text = paste0("Pred.Pop.", i)))]
}

Panel75[, c(paste0("Pred.Pop.", 1990:2015)) := NULL]

## ISSUE: linear prediction will (largely) be captured by admin-specific time trends
## ---> use na.approx linear interpolation instead

