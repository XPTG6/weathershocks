## Preparing dataset for thesis

## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data/Adm2_75")
rm(list = ls())
ProportionNA <- function(x){x[, lapply(.SD, function(y){sum(is.na(y))/length(y)})]}
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "gridExtra", "SPEI", "RcppRoll",
              "lfe", "rworldmap", "sp", "RCurl", "grid", "foreign", "xlsx",
              "maptools", "maps", "shinydashboard", "DT", "reshape2", "plotly", "lubridate",
              "devtools", "rNOMADS", "shinyjs", "dendextend", "ggdendro",
              "raster", "ncdf4", "chron", "lubridate", "rasterVis", "rgdal", "geomerge",
              "Rcpp", "sf", "countrycode", "snow", "cleangeo", "ggmap"#, "mapview"
)
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)
# note sf fails to load unless it's done first in a new session

detach('package:shinyjs') # to avoid conflict with raster::print

devtools::install_github("andrie/rrd")
library(rrd)

## ____ ----

## Rainfall ----

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_rain_", x),
                                        stack(paste0("/data/Data/era_rain75/era_rain75_", x, ".nc")))})
years <- lapply(stack.list,
                function(x){as.integer(substr(names(x), 2, 5))})
years <- unlist(years)
ERArain75 <- stack(stack.list)
indices <- which(years<=2013)
ERArain75 <- subset(ERArain75, indices)
new_z <- unique(years[years<=2013])
indices <- rep(seq_len(nlayers(ERArain75)/2), each=2) # aggregate each month (2 daily fcs)
ERArain75 <- stackApply(ERArain75, indices, sum)
indices <- rep(seq_len(nlayers(ERArain75)/12), each=12)
ERArain75 <- stackApply(ERArain75, indices, mean) # yearly mean
ERArain75 <- calc(ERArain75, function(x){x*1000}) # convert to mm
names(ERArain75) <- as.character(new_z)
ERArain75 <- setZ(ERArain75, new_z)
writeRaster(ERArain75, "/data/Data/era_rain75/ERArain75", overwrite=TRUE)

## Temperature ----

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_temp_", x),
                                        stack(paste0("/data/Data/era_temp75/era_temp75_", x, ".nc")))})
years <- lapply(stack.list,
                function(x){as.integer(substr(names(x), 2, 5))})
years <- unlist(years)
ERAtemp75 <- stack(stack.list)
indices <- which(years<=2013)
ERAtemp75 <- subset(ERAtemp75, indices)
new_z <- unique(years[years<=2013])
indices <- rep(seq_len(nlayers(ERAtemp75)/12), each=12)
ERAtemp75 <- stackApply(ERAtemp75, indices, mean)
ERAtemp75 <- calc(ERAtemp75, function(x){x-273.15}) # convert to °C
names(ERAtemp75) <- as.character(new_z)
ERAtemp75 <- setZ(ERAtemp75, new_z)
writeRaster(ERAtemp75, "/data/Data/era_temp75/ERAtemp75")

## SPEI ----


for(i in c(4, 6, 12)){
  assign(paste0("SPEI", i),
         stack(paste0("/data/Data/SPEI/SPEI", i)))
  years <- as.integer(substr(names(eval(parse(text = paste0("SPEI", i)))), 2, 5))
  years <- unlist(years)
  indices <- which(years<=2013)
  assign(paste0("SPEI", i),
         subset(eval(parse(text = paste0("SPEI", i))), indices))
  new_z <- unique(years[years<=2013])
  indices <- rep(seq_len(nlayers(eval(parse(text = paste0("SPEI", i))))/12), each=12)
  assign(paste0("SPEI", i),
         stackApply(eval(parse(text = paste0("SPEI", i))), indices, mean))
  assign("SPEI_temp",
         eval(parse(text = paste0("SPEI", i))))
  names(SPEI_temp) <- as.character(new_z)
  assign(paste0("SPEI", i), SPEI_temp)
  rm(SPEI_temp)
  assign(paste0("SPEI", i),
         setZ(eval(parse(text = paste0("SPEI", i))), new_z))
  writeRaster(eval(parse(text = paste0("SPEI", i))),
              paste0("/data/Data/SPEI/SPEI", i, "ymeans"),
              overwrite=TRUE)
}


## ____ ----

## Load ethnadmin (+ conflict) ----

## Load
ethnicities.in.admin2 <- shapefile("/data/Data/Shapefiles/ethnadmin2_wconflict.shp")
load("/data/Data/Robjects/admin_areas_2")
# add proj info
proj4string(ethnicities.in.admin2) <- admin_areas_2@proj4string 


## ____ ----

## Extract rain sums for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(ERArain75, ethnicities.in.admin2[i, ],
                       weights = TRUE, normalizeWeights = F)
  print(paste0("Layer ", i, ": extraction completed."))
  cells.table <- as.data.table(allmyvals)
  cols.nw <- setdiff(names(cells.table), "weight")
  if(nrow(cells.table) == 0){
    j = j + 1
    warning(paste0("Layer ", i, " is null."))
    null.polygons[[j]] <- i
  }else{
    cols <- names(cells.table)
    if(sum(is.na(cells.table))>0)warning(paste0(sum(is.na(cells.table)),
                                                "NAs in cells.table for polygon ", i, "."))
    if(i == 1){
      polygon.means <- cells.table[,
                                   lapply(.SD,
                                          function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                                na.rm = T)/sum(weight,
                                                                                               na.rm = T)}),
                                   .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.means data table."))
    }else{
      polygon.means <- rbind(polygon.means,
                             cells.table[,
                                         lapply(.SD,
                                                function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)/sum(weight,
                                                                                                               na.rm = T)}),
                                         .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.rain75 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.rain75, file = "/data/Data/Adm2_75/polygon.means.rain75")
# load("/data/Data/Adm2_75/polygon.means.rain75")


## Extract temp means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(ERAtemp75, ethnicities.in.admin2[i, ],
                       weights = TRUE, normalizeWeights = F)
  print(paste0("Layer ", i, ": extraction completed."))
  cells.table <- as.data.table(allmyvals)
  cols.nw <- setdiff(names(cells.table), "weight")
  if(nrow(cells.table) == 0){
    j = j + 1
    warning(paste0("Layer ", i, " is null."))
    null.polygons[[j]] <- i
  }else{
    cols <- names(cells.table)
    if(sum(is.na(cells.table))>0)warning(paste0(sum(is.na(cells.table)),
                                                "NAs in cells.table for polygon ", i, "."))
    if(i == 1){
      polygon.means <- cells.table[,
                                   lapply(.SD,
                                          function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                                na.rm = T)/sum(weight,
                                                                                               na.rm = T)}),
                                   .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.means data table."))
    }else{
      polygon.means <- rbind(polygon.means,
                             cells.table[,
                                         lapply(.SD,
                                                function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)/sum(weight,
                                                                                                               na.rm = T)}),
                                         .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.temp75 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.temp75, file = "/data/Data/Adm2_75/polygon.means.temp75")
# load("/data/Data/Adm2_75/polygon.means.temp75")


## Extract SPEI4 means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(SPEI4, ethnicities.in.admin2[i, ],
                       weights = TRUE, normalizeWeights = F)
  print(paste0("Layer ", i, ": extraction completed."))
  cells.table <- as.data.table(allmyvals)
  cols.nw <- setdiff(names(cells.table), "weight")
  if(nrow(cells.table) == 0){
    j = j + 1
    warning(paste0("Layer ", i, " is null."))
    null.polygons[[j]] <- i
  }else{
    cols <- names(cells.table)
    if(sum(is.na(cells.table))>0)warning(paste0(sum(is.na(cells.table)),
                                                "NAs in cells.table for polygon ", i, "."))
    if(i == 1){
      polygon.means <- cells.table[,
                                   lapply(.SD,
                                          function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                                na.rm = T)/sum(weight,
                                                                                               na.rm = T)}),
                                   .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.means data table."))
    }else{
      polygon.means <- rbind(polygon.means,
                             cells.table[,
                                         lapply(.SD,
                                                function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)/sum(weight,
                                                                                                               na.rm = T)}),
                                         .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.SPEI4 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.SPEI4, file = "/data/Data/Adm2_75/polygon.means.SPEI4")
# load("/data/Data/Adm2_75/polygon.means.SPEI4")


## Extract SPEI6 means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(SPEI6, ethnicities.in.admin2[i, ],
                       weights = TRUE, normalizeWeights = F)
  print(paste0("Layer ", i, ": extraction completed."))
  cells.table <- as.data.table(allmyvals)
  cols.nw <- setdiff(names(cells.table), "weight")
  if(nrow(cells.table) == 0){
    j = j + 1
    warning(paste0("Layer ", i, " is null."))
    null.polygons[[j]] <- i
  }else{
    cols <- names(cells.table)
    if(sum(is.na(cells.table))>0)warning(paste0(sum(is.na(cells.table)),
                                                "NAs in cells.table for polygon ", i, "."))
    if(i == 1){
      polygon.means <- cells.table[,
                                   lapply(.SD,
                                          function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                                na.rm = T)/sum(weight,
                                                                                               na.rm = T)}),
                                   .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.means data table."))
    }else{
      polygon.means <- rbind(polygon.means,
                             cells.table[,
                                         lapply(.SD,
                                                function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)/sum(weight,
                                                                                                               na.rm = T)}),
                                         .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.SPEI6 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.SPEI6, file = "/data/Data/Adm2_75/polygon.means.SPEI6")
# load("/data/Data/Adm2_75/polygon.means.SPEI6")


## Extract SPEI12 means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(SPEI12, ethnicities.in.admin2[i, ],
                       weights = TRUE, normalizeWeights = F)
  print(paste0("Layer ", i, ": extraction completed."))
  cells.table <- as.data.table(allmyvals)
  cols.nw <- setdiff(names(cells.table), "weight")
  if(nrow(cells.table) == 0){
    j = j + 1
    warning(paste0("Layer ", i, " is null."))
    null.polygons[[j]] <- i
  }else{
    cols <- names(cells.table)
    if(sum(is.na(cells.table))>0)warning(paste0(sum(is.na(cells.table)),
                                                "NAs in cells.table for polygon ", i, "."))
    if(i == 1){
      polygon.means <- cells.table[,
                                   lapply(.SD,
                                          function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                                na.rm = T)/sum(weight,
                                                                                               na.rm = T)}),
                                   .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.means data table."))
    }else{
      polygon.means <- rbind(polygon.means,
                             cells.table[,
                                         lapply(.SD,
                                                function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)/sum(weight,
                                                                                                               na.rm = T)}),
                                         .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.SPEI12 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.SPEI12, file = "/data/Data/Adm2_75/polygon.means.SPEI12")
# load("/data/Data/Adm2_75/polygon.means.SPEI12")



## Load light sums for admin ----

load("/data/Data/Robjects/polygon.sums.lights")


## Load pop sums for admin ----

load("/data/Data/Robjects/polygon.sums.pop")

## Load agri sums for admin ----

load("/data/Data/Robjects/polygon.sums.agri")

## Merge to polygons ----

# Bind rain
setnames(polygon.means.rain75, names(polygon.means.rain75), paste0("Rain", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.rain75)

# Bind temp
setnames(polygon.means.temp75, names(polygon.means.temp75), paste0("Temp", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.temp75)

# Bind light
setnames(polygon.sums.lights, names(polygon.sums.lights), paste0("Light", 1992:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.lights)

# Bind pop
setnames(polygon.sums.pop, names(polygon.sums.pop), paste0("Pop", c(1990, 1995, 2000, 2005, 2010, 2015)))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.pop)

# Bind agri
setnames(polygon.sums.agri, names(polygon.sums.agri), c("agri.irrigated", "agri.rainfed"))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.agri)


# Bind SPEI4
setnames(polygon.means.SPEI4, names(polygon.means.SPEI4), paste0("SPEI4", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.SPEI4)

# Bind SPEI6
setnames(polygon.means.SPEI6, names(polygon.means.SPEI6), paste0("SPEI6", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.SPEI6)


# Bind SPEI12
setnames(polygon.means.SPEI12, names(polygon.means.SPEI12), paste0("SPEI12", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.SPEI12)

# Save
save(ethnicities.in.admin2, file = "/data/Data/Adm2_75/ethnicities.in.admin2_75_withrasterdata")
# load("/data/Data/Adm2_75/ethnicities.in.admin2_75_withrasterdata")
writeSpatialShape(ethnicities.in.admin2, "/data/Data/Adm2_75/ethnadmin2_75_wraster")

# Load
ethnicities.in.admin2 <- shapefile("/data/Data/Adm2_75/ethnadmin2_75_wraster")
load("/data/Data/Robjects/admin_areas_2")
proj4string(ethnicities.in.admin2) <- admin_areas_2@proj4string 


## ____ ----

## Prepare data ----

ethnicities.in.admin2@data$RowIndex <- seq.int(nrow(ethnicities.in.admin2@data))
regression.table <- ethnicities.in.admin2@data
regression.table <- data.table(regression.table)
regression.table[statename == "Cote d'Ivoire", statename := "Côte d'Ivoire"]
regression.table[statename == "Congo", statename := "Republic of Congo"]
regression.table[statename == "Congo, DRC", statename := "Democratic Republic of the Congo"]
regression.table[statename == "The Gambia", statename := "Gambia"]
regression.table[statename == "Libya (Tripolitania, Cyrenaica, Fezzan)", statename := "Libya"]
regression.table[NAME_0 == "Western Sahara", NAME_0 := "Morocco"]
regression.table[statename == "Sudan" & NAME_0 == "South Sudan", NAME_0 := "Sudan"]
## Sudan: because admin dataset is as of today - these are ethnic territories pre-2011 (+ border incongruencies)
regression.table[statename == "Ethiopia" & NAME_0 == "Eritrea" & to <= 1993, NAME_0 := "Ethiopia"]
# same here for Eritrea and Ethiopia

## DIAGNOSTIC code for cross-border groups in ethnadmin_crossborder.R

# Exclude areas picked up in different countries
regression.table <- regression.table[NAME_0 == statename]
## Note conflicted / transferred areas, aside from south sudan, cannot be reliably assigned based on data
## --> cannot distinguish from other inconsistencies
## See email by Luc Girardin

# Exclude Pop1990 < 1000
regression.table[, quantile(Pop1990, seq(0, 1, 0.05))]
regression.table <- regression.table[Pop1990 >= 1000]

# Exclude areasqkm < 10
regression.table[, quantile(areasqkm, seq(0, 1, 0.05))]
regression.table <- regression.table[areasqkm >= 10]

save(regression.table, file = "/data/Data/Adm2_75/regression.table.75")
load("/data/Data/Adm2_75/regression.table.75")

## Create panel ----
setnames(regression.table, c("agri_irrig", "agri_rainf"), c("agri.irrigated", "agri.rainfed"))

regression.table[is.na(GID_2), unique(NAME_0)] # Morocco and Libya lack GID_2, use GID_1 instead
regression.table[is.na(GID_2), GID_2 := GID_1]

regression.table[, unit.name := paste0(GID_2, ".", group)]
setkey(regression.table, unit.name)

panel <- expand.grid(unit.name = regression.table[, unique(unit.name)],
                     year = 1979:2015) # Adapt years
panel <- data.table(panel)
setkey(panel, unit.name, year)
panel <- merge(panel,
               regression.table[, c("unit.name", "NAME_0", "NAME_1", "NAME_2", "GID_1", "GID_2",
                                    "areasqkm",
                                    "group", "gwgroupid",
                                    "from", "to",
                                    paste0("Rain", 1979:2013), 
                                    paste0("Temp", 1979:2013), 
                                    paste0("SPEI4", 1979:2013), 
                                    paste0("SPEI6", 1979:2013), 
                                    paste0("SPEI12", 1979:2013),
                                    paste0("DB_", 1989:2013),
                                    paste0("DM_", 1989:2013),
                                    paste0("CE_", 1989:2013),
                                    paste0("Light", 1992:2013),
                                    paste0("Pop", seq(1990, 2015, 5)),
                                    "agri.irrigated", "agri.rainfed")],
               by = "unit.name",
               all.x = T,
               allow.cartesian=TRUE)

for(i in 1989:2013){
  panel[year == i, Deaths.best := eval(parse(text = paste0("DB_", i)))]
  panel[year == i, Deaths.max := eval(parse(text = paste0("DM_", i)))]
  panel[year == i, Conf.events := eval(parse(text = paste0("CE_", i)))]
}
for(i in 1992:2013){
  panel[year == i, Light := eval(parse(text = paste0("Light", i)))]
  panel[year == i, Rain := eval(parse(text = paste0("Rain", i)))]
  panel[year == i, Temp := eval(parse(text = paste0("Temp", i)))]
  panel[year == i, SPEI4 := eval(parse(text = paste0("SPEI4", i)))]
  panel[year == i, SPEI6 := eval(parse(text = paste0("SPEI6", i)))]
  panel[year == i, SPEI12 := eval(parse(text = paste0("SPEI12", i)))]
}
for(i in 1979:1991){
  panel[year == i, Rain := eval(parse(text = paste0("Rain", i)))]
  panel[year == i, Temp := eval(parse(text = paste0("Temp", i)))]
  panel[year == i, SPEI4 := eval(parse(text = paste0("SPEI4", i)))]
  panel[year == i, SPEI6 := eval(parse(text = paste0("SPEI6", i)))]
  panel[year == i, SPEI12 := eval(parse(text = paste0("SPEI12", i)))]
}
for(i in seq(1990, 2015, 5)){
  panel[year == i, Pop := eval(parse(text = paste0("Pop", i)))]
}

setkey(panel, unit.name, year)
panel[, Pop.filled := na.locf(Pop, na.rm = F), by = unit.name]

panel[, c(paste0("Rain", 1979:2013),
          paste0("Temp", 1979:2013),
          paste0("Light", 1992:2013),
          paste0("SPEI4", 1979:2013),
          paste0("SPEI6", 1979:2013),
          paste0("SPEI12", 1979:2013),
          paste0("DB_", 1989:2013),
          paste0("DM_", 1989:2013),
          paste0("CE_", 1989:2013)) := NULL]

setkey(panel, unit.name, year)

# Long run (pre-study) weather means:
panel[year < 1992, Rain.LRmean := mean(Rain, na.rm = T), by = unit.name]
panel[, Rain.LRmean := na.locf(Rain.LRmean, na.rm = F), by = unit.name]
panel[year < 1992, Temp.LRmean := mean(Temp, na.rm = T), by = unit.name]
panel[, Temp.LRmean := na.locf(Temp.LRmean, na.rm = F), by = unit.name]

# Long run SDs:
panel[year < 1992, Rain.LRsd := sd(Rain, na.rm = T), by = unit.name]
panel[, Rain.LRsd := na.locf(Rain.LRsd, na.rm = F), by = unit.name]
panel[year < 1992, Temp.LRsd := sd(Temp, na.rm = T), by = unit.name]
panel[, Temp.LRsd := na.locf(Temp.LRsd, na.rm = F), by = unit.name]

if(nrow(panel[is.infinite(Temp.LRmean) |
                is.infinite(Temp.LRsd) |
                is.infinite(Rain.LRmean) |
                is.infinite(Rain.LRsd)]) > 0)stop("Infinite long-run values")
ProportionNA(panel)

## Approx Pop ----
setkey(panel, unit.name, year)
panel[, Pop.Approx := as.double(na.approx(Pop, x = year, na.rm = F)), by = unit.name]
# View(panel[1:1000, .(unit.name, year, Pop, Pop.Approx)])
panel <- panel[year<= 2013]

## Remove out of range ----

panel <- panel[year >= from & year <= to] 

## Save panel raw ----

Panel75 <- copy(panel)
save(Panel75, file = "/data/Data/Adm2_75/Panel75_raw")
load("/data/Data/Adm2_75/Panel75_raw")


## Add ethn power ----

EPR_core <- read.csv("/data/Data/EPR-2018.csv")
EPR_core <- data.table(EPR_core)

## Shona (Zimbabwe) fix:
# EPR_core[group == "Shona"]
EPR_core[group == "Shona (minus Ndau)", `:=`(group = "Shona", gwgroupid = 55201200)]
EPR_core[group == "Shona (minus Manyika & Ndau)", `:=`(group = "Shona", gwgroupid = 55201200)]
####

EPR_core <- EPR_core[, .(gwgroupid, status, EPR_core_size = size, reg_aut, status.from = from, status.to = to)]
EPR_core[, gwgroupid := as.character(gwgroupid)]
Panel75[, gwgroupid := as.character(gwgroupid)]
Panel75[, year.b := year]
ProportionNA(EPR_core)

setkey(EPR_core, gwgroupid, status.from, status.to)
setkey(Panel75, gwgroupid, from, to)

pre.length <- nrow(Panel75)
Panel75 <- foverlaps(Panel75, EPR_core,
                   by.x = c("gwgroupid", "year", "year.b"),
                   by.y = c("gwgroupid", "status.from", "status.to"))
if(pre.length!=nrow(Panel75))stop("Output of foverlaps has different number of rows")


## Myene (Gabon) fix:
Panel75[group == "Myene" & year %in% 2001:2005, status := "JUNIOR PARTNER"]
Panel75[group == "Myene" & year %in% 2001:2005, reg_aut := "false"]

## Africans (Zimbabwe) fix:
Panel75 <- Panel75[group != "Africans"]

# Check NAs:
ProportionNA(Panel75)
## Note an issue with Pop & Pop.filled is that ethnic areas starting mid sample may lack early years
## --> consider filling backwards with na.locf


## Add institutions ----
DPI <- read.dta13("/data/Data/DPI2017/DPI2017.dta")
DPI <- as.data.table(DPI)

DPI[countryname == "Eq. Guinea", countryname := "Equatorial Guinea"]
DPI[countryname == "Congo (DRC)", countryname := "Democratic Republic of the Congo"]
DPI[countryname == "Congo", countryname := "Republic of Congo"]
DPI[countryname == "S. Africa", countryname := "South Africa"]
DPI[countryname == "Cent. Af. Rep.", countryname := "Central African Republic"]
DPI[countryname == "Cote d'Ivoire", countryname := "Côte d'Ivoire"]

pre.length <- nrow(Panel75)
Panel75 <- merge(Panel75,
               DPI[, .(NAME_0 = countryname, year, checks, stabs)],
               by = c("NAME_0", "year"),
               all.x = T)
if(pre.length!=nrow(Panel75))stop("Merge changed number of Panel75 rows")

ProportionNA(Panel75)

## Rm South Sudan & Eritrea duplicates ----

x <- Panel75[, sum(duplicated(year)), by = unit.name][, unit.name[V1 > 0]] 
length(x) # 64 south sudan units + 1 Eritrea
# View(Panel75[unit.name %in% x]) # Assigned to both Sudan and South Sudan in 2011
## Official independence on 9th July, but referendum in January - so remove Sudan duplicates
pre.length <- nrow(Panel75)
Panel75 <- Panel75[!(unit.name %in% x & NAME_0 == "Sudan" & year == 2011)]
if((pre.length - nrow(Panel75))>64)warning("Removing too many observations!")
# ERI.3.1_1.Afar to both Ethiopia and Eritrea in 1993 - assign to Eritrea
View(Panel75[unit.name == "ERI.3.1_1.Afar"])
pre.length <- nrow(Panel75)
Panel75 <- Panel75[!(unit.name %in% x & NAME_0 == "Ethiopia" & year == 1993)]
if((pre.length - nrow(Panel75))>1)warning("Removing too many observations!")

## Save Panel75 ----
save(Panel75, file = "/data/Data/Adm2_75/Panel75_Final")
load("/data/Data/Adm2_75/Panel75_Final")



## ____ ----
