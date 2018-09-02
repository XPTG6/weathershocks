## Main work for thesis

## Setup ----
setwd("/data/Data")
rm(list = ls())
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "gridExtra", "SPEI", "RcppRoll",
              "lfe", "rworldmap", "sp", "RCurl", "grid", "foreign",
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

## Rainfall ----

ERArain <- stack("/data/Data/ERA_Interim_rainfall.nc")
year <- as.integer(substr(names(ERArain), 2, 5))
indices <- which(year<=2013)
ERArain <- subset(ERArain, indices)
new_z <- unique(year[year<=2013])
indices <- rep(seq_len(nlayers(ERArain)/24), each=24)
ERArain <- stackApply(ERArain, indices, sum)
names(ERArain) <- as.character(new_z)
ERArain <- setZ(ERArain, new_z)

## Temperature ----

ERAtemp <- stack("/data/Data/ERA_Interim_temp.nc")
year <- as.integer(substr(names(ERAtemp), 2, 5))
indices <- which(year<=2013)
ERAtemp <- subset(ERAtemp, indices)
new_z <- unique(year[year<=2013])
indices <- rep(seq_len(nlayers(ERAtemp)/12), each=12)
ERAtemp <- stackApply(ERAtemp, indices, mean)
names(ERAtemp) <- as.character(new_z)
ERAtemp <- setZ(ERAtemp, new_z)

## Nightlights ----

for (i in c('F101992', 'F101993', 'F121994', 'F121995', 'F121996',
            'F141997', 'F141998', 'F141999', 'F152000', 'F152001',
            'F152002', 'F152003', 'F162004', 'F162005', 'F162006',
            'F162007', 'F162008', 'F162009', 'F182010', 'F182011',
            'F182012', 'F182013')){
  year <- substr(i, 4, 7)
  
  if(length(grep(".tif", list.files(paste0("/data/Data/Nightlights/",
                                           i,
                                           ".v4")))) != 1){
    stop("Wrong number of .tif files in ", year, " directory.")}
  
  assign(paste0("lights.", year),
         raster(paste0("/data/Data/Nightlights/",
                       i,
                       ".v4/",
                       grep(".tif", list.files(paste0("/data/Data/Nightlights/",
                                                      i,
                                                      ".v4")), value = T))))
  
  assign(paste0("lights.", year),
         setZ(eval(parse(text = paste0("lights.", year))),
              as.Date(paste0(year, "-01-01")), name='time'))
}

lights.list <- mget(paste0("lights.", 1992:2013))
lights.stack <- stack(lights.list)

# Africa only
e <- extent(-20.2, 60.2, -40.2, 40.2)
lights.stack.c <- crop(lights.stack, e)

writeRaster(lights.stack.c, "/data/Data/Robjects/lights.stack.c.grd")
# lights.stack.c <- stack("/data/Data/Robjects/lights.stack.c.grd")

names(lights.stack.c) <- as.character(1992:2013)

# Aggregate to 10x lower resolution
lights.stack.c <- stack(lights.stack.c)
lights.stack.ca10 <- aggregate(lights.stack.c, 10, fun = function(x, ...)if(is.null(x)){NA}else{sum(x, na.rm = T)})

writeRaster(lights.stack.ca10, "/data/Data/Robjects/lights.stack.ca10.grd")
# lights.stack.ca10 <- stack("/data/Data/Robjects/lights.stack.ca10.grd")


rm(list = paste0("lights.", 1992:2013))
rm(lights.list, lights.stack, e)


# Plotting:
l1992 <- raster(lights.stack.ca10, "X1992")
l2013 <- raster(lights.stack.ca10, "X2013")
lightsTheme <- rasterTheme(region = c("black", "white"))
a <- levelplot(l1992, margin = F,
               #at=cutpts,
               par.settings = lightsTheme,
               pretty=TRUE)
b <- levelplot(l2013, margin = F,
               #at=cutpts,
               par.settings = lightsTheme,
               pretty=TRUE)
grid.arrange(a, b, ncol = 2)


## Administrative boundaries ----

admin_areas_0 <- shapefile("/data/Data/GADM/gadm36_0.shp")

# Subset Africa
african_countrycodes <- data.table(continent = codelist$continent, iso = codelist$iso3c)
african_countrycodes <- african_countrycodes[continent == "Africa"]
admin_areas_0 <- admin_areas_0[admin_areas_0@data$GID_0 %in% african_countrycodes$iso, ]
#map(admin_areas_0)

for (i in 1:5){
  assign(paste0("admin_areas_", i), shapefile(paste0("/data/Data/GADM/gadm36_", i, ".shp")))
  assign(paste0("admin_areas_", i),
         eval(parse(text = paste0("admin_areas_",
                                  i)))[eval(parse(text = paste0("admin_areas_",
                                                                i)))@data$GID_0 %in% african_countrycodes$iso, ])
  
}

save(admin_areas_1, file = "/data/Data/Robjects/admin_areas_1")
#load("/data/Data/Robjects/admin_areas_1")

## QUESTION: at what admin level? lowest by country? But 5 may be too low (and only Rwanda)
map(admin_areas_1)
length(unique(admin_areas_5$NAME_0))
length(unique(admin_areas_4$NAME_0))
length(unique(admin_areas_3$NAME_0))
length(unique(admin_areas_2$NAME_0))
length(unique(admin_areas_1$NAME_0))
length(unique(admin_areas_0$NAME_0))

noleveltwo <- unique(admin_areas_1$NAME_0)[!unique(admin_areas_1$NAME_0) %in% unique(admin_areas_2$NAME_0)]

## level 2 or 1 if two missing
admin_areas_2 <- bind(admin_areas_2,
                      admin_areas_1[admin_areas_1@data$NAME_0 %in% noleveltwo, ])

save(admin_areas_2, file = "/data/Data/Robjects/admin_areas_2")
load("/data/Data/Robjects/admin_areas_2")

## Check projection
readOGR("/data/Data/GADM/gadm36_1.shp")
# Output: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
ogrInfo(dsn="/data/Data/GADM", layer="gadm36_1") #faster



## GeoEPR ----

GeoEPR <- shapefile("/data/Data/GeoEPR-2018/GeoEPR.shp")

GeoEPR@data$iso <- countrycode(GeoEPR@data$statename, "country.name", "iso3c")
GeoEPR@data[is.na(GeoEPR@data$iso),] # nothing to worry about

GeoEPR <- GeoEPR[GeoEPR@data$iso %in% unique(admin_areas_2@data$GID_0),]

# # check .csv for 'null geometries' from warning message above
# x <- read.csv("/data/Data/GeoEPR-2018.csv")
# indices <- c(10, 11, 66, 95, 102, 103, 107, 114, 119, 120, 121, 127, 131, 143, 152, 153, 154,
#              167, 173, 174, 175, 176, 180, 183, 210, 216, 217, 224, 230, 233, 234, 243, 244,
#              246, 253, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 369, 370, 371, 372,
#              373, 468, 469, 470, 472, 473, 476, 509, 518, 573, 667, 672, 764, 775, 794, 806,
#              807, 821, 822, 823, 825, 826, 827, 828, 853, 909, 910, 911, 912, 919, 920, 928,
#              954, 955, 956, 957, 973, 1006, 1007, 1023, 1028, 1036, 1039, 1040, 1041, 1043,
#              1049, 1050, 1054, 1058, 1059, 1060, 1061, 1062, 1137, 1140, 1142, 1221, 1226,
#              1227, 1257, 1258, 1259, 1262, 1279, 1285, 1290, 1302, 1317, 1341, 1342, 1343,
#              1344, 1345, 1349, 1457, 1458, 1462, 1464, 1465)
# View(x[indices,]) 
# # See codebook: dispersed, nomad, and urban groups. (shouldn't be a problem)


# Check countries not in Geo-EPR
length(unique(GeoEPR@data$statename)); length(unique(admin_areas_2@data$GID_0))
admin_areas_2@data$NAME_0[admin_areas_2@data$GID_0 %in% setdiff(unique(admin_areas_2@data$GID_0), GeoEPR@data$iso)]
###
# Tunisia: ethnically homogeneous (except tiny & politically irrelevant)
# Seychelles: not in dataset
# Swaziland: argue Zulu and White politically irrelevant
# São Tomé and Príncipe: not in dataset
# Somalia: ethnically homogeneous, ethnicity irrelevant politically, clans ≠ ethnic groups
# Saint Helena: not in dataset
# Mayotte: not in dataset
# Reunion: not in dataset
# Lesotho: ethnicity not politically relevant
# Western Sahara: included as Morocco? [!!!]
# Burkina Faso: ethnically very heterogeneous, but not politically relevant [!!!]


## Intersect GeoEPR and admin ----
GeoEPR@proj4string
admin_areas_2@proj4string # same
# proj4string(GeoEPR) <- admin_areas_2@proj4string # copy admin info to GeoEPR

GeoEPR <- GeoEPR[GeoEPR@data$to >= 1992, ]

## Cleaning a self-intersecting polygon in admin_areas_2
admin_areas_2 <- clgeo_Clean(admin_areas_2, strategy = "BUFFER")
sum(gIsValid(admin_areas_2, byid=TRUE)==FALSE)

if(sum(!gIsValid(admin_areas_2, byid = T))>0)warning(paste(sum(!gIsValid(admin_areas_2, byid = T)),
                                                           "self-intersecting geometries in admin_areas_2"))
if(sum(!gIsValid(GeoEPR, byid = T))>0)warning(paste(sum(!gIsValid(GeoEPR, byid = T)),
                                                    "self-intersecting geometries in GeoEPR"))

# Intersect admin areas with ethnic homelands
ethnicities.in.admin2 <- raster::intersect(admin_areas_2, GeoEPR)

## ISSUE: above could also be done with sf, but crashes
# GeoEPR <- st_as_sf(GeoEPR)
# admin_areas_2 <- st_as_sf(admin_areas_2)
# ethnicities.in.admin2 <- st_intersects(admin_areas_2, GeoEPR)

## Save and load
writeSpatialShape(ethnicities.in.admin2, "/data/Data/Shapefiles/ethnadmin2")
# ethnicities.in.admin2 <- shapefile("/data/Data/Shapefiles/ethnadmin2.shp")
# load("/data/Data/Robjects/admin_areas_2")

# add proj info
proj4string(ethnicities.in.admin2) <- admin_areas_2@proj4string 

ethnicities.in.admin2@data$area <- area(ethnicities.in.admin2)
ethnicities.in.admin2@data$areasqkm <- ethnicities.in.admin2@data$area / 1000000
ethnicities.in.admin2@data$area <- NULL
summary(ethnicities.in.admin2@data$areasqkm)

#Excluding archipelagos with primarily touristic economy
ethnicities.in.admin2 <- ethnicities.in.admin2[!ethnicities.in.admin2@data$NAME_0 %in% c("Mauritius", "Comoros"),]
#Note Seychelles and Sao Tome already excluded from GeoEPR so don't appear here (same for French and British islands)

## Extract rain sums for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(ERArain, ethnicities.in.admin2[i, ],
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
      polygon.sums <- cells.table[,
                                  lapply(.SD,
                                         function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                               na.rm = T)}),
                                  .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.sums data table."))
    }else{
      polygon.sums <- rbind(polygon.sums,
                            cells.table[,
                                        lapply(.SD,
                                               function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)}),
                                        .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.sums data table."))
    }
  }
}

polygon.sums.rain <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.rain, file = "/data/Data/Robjects/polygon.sums.rain")
# load("/data/Data/Robjects/polygon.sums.rain")

# ## Alternative code using extract on all polygons and lapply
# ### Seems about same speed / slower and gives no indication of progress 
# weights <- extract(ERArain, ethnicities.in.admin2,
#                    weights = TRUE, normalizeWeights = F)
# 
# save(weights, file = "/data/Data/Robjects/weights")
# # load("/data/Data/Robjects/weights")
# 
# weights <- lapply(weights, as.data.table)
# sums <- lapply(weights, function(x){
#   cols <- setdiff(names(x), "weight")
#   x[, lapply(.SD, function(x){sum(x*weight)}), .SDcols = cols]})
# 
# rain.sums <- rbindlist(sums)
# save(rain.sums, file = "/data/Data/Robjects/rain.sums")


## Extract temp means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(ERAtemp, ethnicities.in.admin2[i, ],
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
polygon.means.temp <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.temp, file = "/data/Data/Robjects/polygon.means.temp")
# load("/data/Data/Robjects/polygon.means.temp")


## Extract light sums for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(lights.stack.ca10, ethnicities.in.admin2[i, ],
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
      polygon.sums <- cells.table[,
                                  lapply(.SD,
                                         function(x)if(is.null(x)){NA}else{sum(x*weight,
                                                                               na.rm = T)}),
                                  .SDcols = cols.nw]
      print(paste0("Polygon ", i, ": created polygon.sums data table."))
    }else{
      polygon.sums <- rbind(polygon.sums,
                            cells.table[,
                                        lapply(.SD,
                                               function(x)if(is.null(x)){NA}else{sum(x*weight, na.rm = T)}),
                                        .SDcols = cols.nw])
      print(paste0("Polygon ", i, ": added to polygon.sums data table."))
    }
  }
}

polygon.sums.lights <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.lights, file = "/data/Data/Robjects/polygon.sums.lights")
# load("/data/Data/Robjects/polygon.sums.lights")


## Merge to polygons ----

# Bind rain
setnames(polygon.sums.rain, names(polygon.sums.rain), paste0("Rain", 1990:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.rain)

# Bind temp
setnames(polygon.means.temp, names(polygon.means.temp), paste0("Temp", 1990:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.temp)

# Bind light
setnames(polygon.sums.lights, names(polygon.sums.lights), paste0("Light", 1992:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.lights)

save(ethnicities.in.admin2, file = "/data/Data/Robjects/ethnicities.in.admin2_withrasterdata")
# load("/data/Data/Robjects/ethnicities.in.admin2_withrasterdata")

## Add population ----

pop90 <- raster("/data/Data/SEDAC-bil/afup90ag.bil")

e <- extent(-20.2, 60.2, -40.2, 40.2)
pop90 <- crop(pop90, e)
# Aggregate to 10x lower resolution
pop90.a <- aggregate(pop90, 10, fun = function(x, ...)if(is.null(x)){NA}else{sum(x, na.rm = T)})
writeRaster(pop90.a, "/data/Data/pop90_a", format = "raster", overwrite=TRUE)


popcells.by.poly <- extract(pop90.a, ethnicities.in.admin2)
pop.by.poly <- lapply(popcells.by.poly, function(x)if(is.null(x)){NA}else{sum(x, na.rm = T)})

ethnicities.in.admin2@data$population <- unlist(pop.by.poly)

save(ethnicities.in.admin2, file = "/data/Data/Robjects/ethnicities.in.admin2.withpop")
# load("/data/Data/Robjects/ethnicities.in.admin2.withpop")
writeSpatialShape(ethnicities.in.admin2, "/data/Data/Shapefiles/ethnadmin2_wpop")


## Prepare data ----

# exclude pre-1994 (at least two years)
ethnicities.in.admin2 <- ethnicities.in.admin2[ethnicities.in.admin2@data$to >= 1994, ]

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
# View(regression.table[NAME_0 != statename, .(NAME_0, statename)])
# x <- regression.table[NAME_0 != statename,]
# setkey(x, areasqkm)
# x

# diagnostic plot, small areas
plot(ethnicities.in.admin2[ethnicities.in.admin2@data$RowIndex == 7982, ])
map(admin_areas_0,add=T,fill=F, col="red", lwd = 0.2)
# clearly just approximation error in drawing boundaries

# unclear about larger areas... checked google for border changes but doesn't seem to fit time period
map(admin_areas_0[admin_areas_0@data$NAME_0 %in% c("Morocco", "Algeria"),],fill=F,col="red", lwd = 0.2)
plot(ethnicities.in.admin2[ethnicities.in.admin2@data$RowIndex %in% c(4152, 4182), ], add=T)

# Exclude areas picked up in different countries
regression.table <- regression.table[NAME_0 == statename]
## ISSUE: here there are also areas that have been annexed in war / territorial changes
## need more rigorous approach!

# Exclude pop < 1000
regression.table[, summary(population)]
regression.table <- regression.table[population >= 1000]

save(regression.table, file = "/data/Data/Robjects/regression.table")
load("/data/Data/Robjects/regression.table")

## Add ethn power ----

EPR_core <- read.csv("/data/Data/EPR-2018.csv")
EPR_core <- data.table(EPR_core)
EPR_core <- EPR_core[, .(gwgroupid, status, from, to)]
EPR_core[, gwgroupid := as.character(gwgroupid)]
regression.table[, gwgroupid := as.character(gwgroupid)]

setkey(EPR_core, gwgroupid, from, to)
setkey(regression.table, gwgroupid, from, to)
regression.table <- foverlaps(regression.table, EPR_core,
                              by.x = c("gwgroupid", "from", "to"),
                              by.y = c("gwgroupid", "from", "to"))
# View(regression.table[1:1000])

## Create panel ----

regression.table[, unit.name := paste0(GID_2, ".", group)]
setkey(regression.table, unit.name)

panel <- expand.grid(unit.name = regression.table[, unique(unit.name)],
                     year = 1990:2013)
panel <- data.table(panel)
setkey(panel, unit.name, year)
panel <- merge(panel,
               regression.table[, c("unit.name", "NAME_0", "NAME_1", "NAME_2",
                                    "areasqkm", "population",
                                    "from", "to", "i.from", "i.to",
                                    "status",
                                    paste0("Rain", 1990:2013),
                                    paste0("Temp", 1990:2013),
                                    paste0("Light", 1992:2013))],
               by = "unit.name",
               all.x = T,
               allow.cartesian=TRUE)
panel <- panel[year >= from & year <= to]
panel <- panel[year >= i.from & year <= i.to]
names(regression.table)

for(i in 1992:2013){
  panel[year == i, Light := eval(parse(text = paste0("Light", i)))]
  panel[year == i, Rain := eval(parse(text = paste0("Rain", i)))]
  panel[year == i, Temp := eval(parse(text = paste0("Temp", i)))]
}
for(i in 1990:1991){
  panel[year == i, Rain := eval(parse(text = paste0("Rain", i)))]
  panel[year == i, Temp := eval(parse(text = paste0("Temp", i)))]
}

panel[, c(paste0("Rain", 1990:2013),
          paste0("Temp", 1990:2013),
          paste0("Light", 1992:2013)) := NULL]

Panel <- copy(panel)
save(Panel, file = "/data/Data/Robjects/Panel")
load("/data/Data/Robjects/Panel")


## Run regression ----

Panel[, Rain.km := Rain/areasqkm]
Panel[, Light.cap := Light/population]
Panel[, year.f := as.factor(year)]
Panel[, state.f := as.factor(NAME_0)]
Panel[, admin2.f := as.factor(NAME_2)]

Panel[, year.state.f := interaction(year.f, state.f)]

# Panel[year == 1990, Rain.km.1990 := Rain.km]
# Panel[, Rain.km.1990 := max(Rain.km.1990, na.rm = T), by = unit.name]
# Panel <- Panel[!is.infinite(Rain.km.1990)] #ISSUE: arbitrary fix!
setkey(Panel, unit.name, year)
Panel[, Rain.km.lag := data.table::shift(Rain.km, 1, type = "lag"), by = unit.name]

Panel[, Rain.km.mean := mean(Rain.km), by = unit.name]
Panel[, Rain.km.d := Rain.km - Rain.km.mean]
Panel[, Rain.km.lag.d := Rain.km.lag - Rain.km.mean]

setkey(Panel, unit.name, year)
Panel[, Rain.rmean3 := roll_meanr(Rain.km, n = 3, fill = NA), by = unit.name]
Panel[, Rain.shock := (Rain.km-Rain.rmean3)/Rain.rmean3]

Panel[, Temp.lag := data.table::shift(Temp, 1, type = "lag"), by = unit.name]
Panel[, Temp.rmean3 := roll_meanr(Temp, n = 3, fill = NA), by = unit.name]
Panel[, Temp.shock := (Temp-Temp.rmean3)/Temp.rmean3]

Panel <- Panel[, .(unit.name,
                   year, year.f, state.f, admin2.f, year.state.f,
                   Rain.km, Rain.km.lag, Rain.km.d, Rain.km.lag.d,
                   Rain.shock,
                   Temp, Temp.lag,
                   Light.cap,
                   status)]
Panel <- Panel[!is.na(Light.cap)]

Panel[, status := relevel(status, "JUNIOR PARTNER")]
Panel[, year := as.double(year)]

# senior partner effect visible if using rain.km, disappears with shock measure
reg0 <- felm(Light.cap ~ Rain.shock|year.f + admin2.f + year|0|admin2.f,
             Panel)
reg1 <- felm(Light.cap ~ Rain.shock + Rain.shock*status + status|year.f + admin2.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.cap ~ Rain.shock + Rain.shock*status + status|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel)
reg2b <- felm(Light.cap ~ Rain.shock + Rain.shock*status + status|year.f + admin2.f + state.f : year.f|0|admin2.f,
             Panel)

summary(reg0)
summary(reg1)
summary(reg2)
summary(reg2b)

treg0 <- felm(Light.cap ~ Temp|year.f + admin2.f + year|0|admin2.f,
             Panel)
treg1 <- felm(Light.cap ~ Temp + Temp*status + status|year.f + admin2.f|0|admin2.f,
             Panel)
treg2 <- felm(Light.cap ~ Temp + Temp*status + status|year.f + admin2.f + admin2.f : year|0|admin2.f,
             Panel)
treg2b <- felm(Light.cap ~ Temp + Temp*status + status|year.f + admin2.f + state.f : year.f|0|admin2.f,
              Panel)

summary(treg0)
summary(treg1)
summary(treg2)
summary(treg2b)

Panel[, length(unit.name), by = status]
Panel
