
## DEPRECATED (see notes) ----

## Still used to derive some variables for Adm2_75_Prep, but the Panel should not be used.
## Climate data processing is deprecated (rainfall needs to follow Adm2_75_Prep)

## ____ ----

## Preparing dataset for thesis

## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
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
                                        stack(paste0("/data/Data/era_rain/era_rain_", x, ".nc")))})
years <- lapply(stack.list,
                function(x){as.integer(substr(names(x), 2, 5))})
years <- unlist(years)
ERArain <- stack(stack.list)
indices <- which(years<=2013)
ERArain <- subset(ERArain, indices)
new_z <- unique(years[years<=2013])
indices <- rep(seq_len(nlayers(ERArain)/24), each=24) # aggregate to year (2 monthly fcs)
ERArain <- stackApply(ERArain, indices, sum)
ERArain <- calc(ERArain, function(x){x*1000}) # convert to mm
ERArain <- reclassify(ERArain, matrix(c(-1, 0, 0), ncol=3), right = F) # convert neg. interpolations to zero
names(ERArain) <- as.character(new_z)
ERArain <- setZ(ERArain, new_z)
writeRaster(ERArain, "/data/Data/era_rain/ERArain", overwrite = T)
# ERArain <- stack("/data/Data/era_rain/ERArain")

## Temperature ----

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_temp_", x),
                                        stack(paste0("/data/Data/era_temp/era_temp_", x, ".nc")))})
years <- lapply(stack.list,
                function(x){as.integer(substr(names(x), 2, 5))})
years <- unlist(years)
ERAtemp <- stack(stack.list)
indices <- which(years<=2013)
ERAtemp <- subset(ERAtemp, indices)
new_z <- unique(years[years<=2013])
indices <- rep(seq_len(nlayers(ERAtemp)/12), each=12)
ERAtemp <- stackApply(ERAtemp, indices, mean)
ERAtemp <- calc(ERAtemp, function(x){x-273.15}) # convert to °C
names(ERAtemp) <- as.character(new_z)
ERAtemp <- setZ(ERAtemp, new_z)
writeRaster(ERAtemp, "/data/Data/era_temp/ERAtemp")

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

writeRaster(lights.stack.c, "/data/Data/Robjects/lights.stack.c.grd", overwrite = T)
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


## Population ----

e <- extent(-20, 60, -40, 40)
pop90 <- raster("/data/Data/GPW3/1990/afp90ag.bil")
pop90 <- crop(pop90, e)
pop95 <- raster("/data/Data/GPW3/1995/afp95ag.bil")
pop95 <- crop(pop95, e)
pop00 <- stack("/data/Data/GPW4/gpw_v4_une_atotpopbt_cntm_2pt5_min.nc")
# plot(pop00)
pop00 <- pop00[[1:4]] # extract estimates for 00, 05, 10, 15 (other layers inc. data quality, see .csv contents file)
pop00 <- crop(pop00, e)
listpop <- c(pop90, pop95, pop00)
Pop <- stack(listpop)
names(Pop) <- c("1990", "1995", "2000", "2005", "2010", "2015")
crs(Pop) <- crs(pop00)
 
writeRaster(Pop, "/data/Data/R_Rasters/Pop", format = "raster")


## Agriculture ----

e <- extent(-20, 60, -40, 40)

agri.irrigated <- raster("/data/Data/MIRCA_harvested_area_grids/annual_area_harvested_irrigated_allcrops_ha.asc")
agri.irrigated <- crop(agri.irrigated, e)
crs(agri.irrigated) <- crs(Pop)
setZ(agri.irrigated, "Irrigated", name = "type")

agri.rainfed <- raster("/data/Data/MIRCA_harvested_area_grids/annual_area_harvested_rainfed_allcrops_ha.asc")
agri.rainfed <- crop(agri.rainfed, e)
crs(agri.rainfed) <- crs(Pop)
setZ(agri.rainfed, "Rainfed", name = "type")

Agri <- stack(agri.irrigated, agri.rainfed)
writeRaster(Agri, "/data/Data/R_Rasters/Agri", format = "raster")


## ____ ----

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

## level 2 or 1 if two missing
admin_areas_2 <- bind(admin_areas_2,
                      admin_areas_1[admin_areas_1@data$NAME_0 %in% noleveltwo, ])

save(admin_areas_2, file = "/data/Data/Robjects/admin_areas_2")
load("/data/Data/Robjects/admin_areas_2")


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


## Add conflict events ----

Conflict <- shapefile("/data/Data/UCDP_GED/ged181")
proj4string(Conflict) <- admin_areas_2@proj4string 
e <- extent(-20, 60, -40, 40)
Conflict <- crop(Conflict, e)

for(i in unique(Conflict@data$year)){
  assign(paste0("Conflict.", i),
         Conflict[Conflict$year == i,])
  assign(paste0("pointsinpolygons", i),
         over(ethnicities.in.admin2,
              eval(parse(text = paste0("Conflict.", i))),
              returnList = TRUE))
  
  assign(paste0("deathsinpolygons", i), 
         unlist(lapply(eval(parse(text = paste0("pointsinpolygons", i))),
                       function(x){sum(x$best)})))
  assign(paste0("deathsinpolygons", i), 
         data.table(eval(parse(text = paste0("deathsinpolygons", i)))))
  setnames(eval(parse(text = paste0("deathsinpolygons", i))),
           "V1",
           paste0("DB.", i))
  ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data,
                                      eval(parse(text = paste0("deathsinpolygons", i))))
  
  
  assign(paste0("maxdeathsinpolygons", i),
         unlist(lapply(eval(parse(text = paste0("pointsinpolygons", i))),
                       function(x){sum(x$high)})))
  assign(paste0("maxdeathsinpolygons", i), 
         data.table(eval(parse(text = paste0("maxdeathsinpolygons", i)))))
  setnames(eval(parse(text = paste0("maxdeathsinpolygons", i))),
           "V1",
           paste0("DM.", i))
  ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data,
                                      eval(parse(text = paste0("maxdeathsinpolygons", i))))
  
  
  assign(paste0("eventsinpolygons", i),
         unlist(lapply(eval(parse(text = paste0("pointsinpolygons", i))),
                       function(x){nrow(x)})))
  assign(paste0("eventsinpolygons", i), 
         data.table(eval(parse(text = paste0("eventsinpolygons", i)))))
  setnames(eval(parse(text = paste0("eventsinpolygons", i))),
           "V1",
           paste0("CE.", i))
  ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data,
                                      eval(parse(text = paste0("eventsinpolygons", i))))
  print(paste0("Added conflict data for year ", i))
}



## Save and load
writeSpatialShape(ethnicities.in.admin2, "/data/Data/Shapefiles/ethnadmin2_wconflict")
# ethnicities.in.admin2 <- shapefile("/data/Data/Shapefiles/ethnadmin2_wconflict.shp")
# load("/data/Data/Robjects/admin_areas_2")
# add proj info
proj4string(ethnicities.in.admin2) <- admin_areas_2@proj4string 


## ____ ----

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


## Extract pop sums for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(Pop, ethnicities.in.admin2[i, ],
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

polygon.sums.pop <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.pop, file = "/data/Data/Robjects/polygon.sums.pop")
# load("/data/Data/Robjects/polygon.sums.pop")

## Extract agri sums for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin2)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(Agri, ethnicities.in.admin2[i, ],
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

polygon.sums.agri <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.agri, file = "/data/Data/Robjects/polygon.sums.agri")
# load("/data/Data/Robjects/polygon.sums.agri")

## Merge to polygons ----

# Bind rain
setnames(polygon.sums.rain, names(polygon.sums.rain), paste0("Rain", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.rain)

# Bind temp
setnames(polygon.means.temp, names(polygon.means.temp), paste0("Temp", 1979:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.means.temp)

# Bind light
setnames(polygon.sums.lights, names(polygon.sums.lights), paste0("Light", 1992:2013))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.lights)

# Bind pop
setnames(polygon.sums.pop, names(polygon.sums.pop), paste0("Pop", c(1990, 1995, 2000, 2005, 2010, 2015)))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.pop)

# Bind agri
setnames(polygon.sums.agri, names(polygon.sums.agri), c("agri.irrigated", "agri.rainfed"))
ethnicities.in.admin2@data <- cbind(ethnicities.in.admin2@data, polygon.sums.agri)


# Save
save(ethnicities.in.admin2, file = "/data/Data/Robjects/ethnicities.in.admin2_withrasterdata")
# load("/data/Data/Robjects/ethnicities.in.admin2_withrasterdata")
writeSpatialShape(ethnicities.in.admin2, "/data/Data/Shapefiles/ethnadmin2_wraster")

# Load
ethnicities.in.admin2 <- shapefile("/data/Data/Shapefiles/ethnadmin2_wraster")
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

save(regression.table, file = "/data/Data/Robjects/regression.table")
load("/data/Data/Robjects/regression.table")

## Create panel ----
setnames(regression.table, c("agri_irrig", "agri_rainf"), c("agri.irrigated", "agri.rainfed"))

regression.table[is.na(GID_2), unique(NAME_0)] # Morocco and Libya lack GID_2, use GID_1 instead
regression.table[is.na(GID_2), GID_2 := GID_1]

regression.table[, unit.name := paste0(GID_2, ".", group)]
setkey(regression.table, unit.name)

panel <- expand.grid(unit.name = regression.table[, unique(unit.name)],
                     year = 1979:2013) # Adapt years
panel <- data.table(panel)
setkey(panel, unit.name, year)
panel <- merge(panel,
               regression.table[, c("unit.name", "NAME_0", "NAME_1", "NAME_2",
                                    "areasqkm",
                                    "group", "gwgroupid",
                                    "from", "to",
                                    paste0("Rain", 1979:2013), 
                                    paste0("Temp", 1979:2013), 
                                    paste0("Light", 1992:2013),
                                    paste0("DB_", 1989:2013),
                                    paste0("DM_", 1989:2013),
                                    paste0("CE_", 1989:2013),
                                    paste0("Pop", seq(1990, 2015, 5)),
                                    "agri.irrigated", "agri.rainfed")],
               by = "unit.name",
               all.x = T,
               allow.cartesian=TRUE)
panel <- panel[year >= from & year <= to]

for(i in 1989:2013){
  panel[year == i, Deaths.best := eval(parse(text = paste0("DB_", i)))]
  panel[year == i, Deaths.max := eval(parse(text = paste0("DM_", i)))]
  panel[year == i, Conf.events := eval(parse(text = paste0("CE_", i)))]
}
for(i in 1992:2013){
  panel[year == i, Light := eval(parse(text = paste0("Light", i)))]
  panel[year == i, Rain := eval(parse(text = paste0("Rain", i)))]
  panel[year == i, Temp := eval(parse(text = paste0("Temp", i)))]
}
for(i in 1979:1991){
  panel[year == i, Rain := eval(parse(text = paste0("Rain", i)))]
  panel[year == i, Temp := eval(parse(text = paste0("Temp", i)))]
}
for(i in seq(1990, 2015, 5)){
  panel[year == i, Pop := eval(parse(text = paste0("Pop", i)))]
}

panel[, Pop.filled := na.locf(Pop, na.rm = F), by = unit.name]

panel[, c(paste0("Rain", 1979:2013),
          paste0("Temp", 1979:2013),
          paste0("Light", 1992:2013),
          paste0("DB_", 1989:2013),
          paste0("DM_", 1989:2013),
          paste0("CE_", 1989:2013)) := NULL]

Panel <- copy(panel)
save(Panel, file = "/data/Data/Robjects/Panel_raw")
load("/data/Data/Robjects/Panel_raw")


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
Panel[, gwgroupid := as.character(gwgroupid)]
Panel[, year.b := year]
ProportionNA(EPR_core)

setkey(EPR_core, gwgroupid, status.from, status.to)
setkey(Panel, gwgroupid, from, to)

pre.length <- nrow(Panel)
Panel <- foverlaps(Panel, EPR_core,
                              by.x = c("gwgroupid", "year", "year.b"),
                              by.y = c("gwgroupid", "status.from", "status.to"))
if(pre.length!=nrow(Panel))stop("Output of foverlaps has different number of rows")


## Myene (Gabon) fix:
Panel[group == "Myene" & year %in% 2001:2005, status := "JUNIOR PARTNER"]
Panel[group == "Myene" & year %in% 2001:2005, reg_aut := "false"]

## Africans (Zimbabwe) fix:
Panel <- Panel[group != "Africans"]

# Check NAs:
ProportionNA(Panel)
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

pre.length <- nrow(Panel)
Panel <- merge(Panel,
               DPI[, .(NAME_0 = countryname, year, checks, stabs)],
               by = c("NAME_0", "year"),
               all.x = T)
if(pre.length!=nrow(Panel))stop("Merge changed number of Panel rows")

ProportionNA(Panel)
# Panel[year>1990, sum(is.na(checks))/length(checks), by = NAME_0] # not too bad
# Panel[year>1990, sum(is.na(checks))/length(checks), by = year] 

## Check for different country names
# pcy <- Panel[, .(countryname = unique(NAME_0)), by = year]
# x <- merge(pcy, DPI, by = c("countryname", "year"))
# pcy[!countryname %in% x[, unique(countryname)], unique(countryname)]


## Rm South Sudan duplicates ----

x <- Panel[, sum(duplicated(year)), by = unit.name][, unit.name[V1 > 0]] 
length(x) # 64 south sudan units
# View(Panel75[unit.name %in% x]) # Assigned to both Sudan and South Sudan in 2011
## Official independence on 9th July, but referendum in January - so remove Sudan duplicates
pre.length <- nrow(Panel)
Panel <- Panel[!(unit.name %in% x & NAME_0 == "Sudan" & year == 2011)]
pre.length - nrow(Panel)


## Save panel ----
save(Panel, file = "/data/Data/Robjects/Panel_Final")
load("/data/Data/Robjects/Panel_Final")


## ____ ----
