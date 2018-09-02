## Preparing dataset for thesis

## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data/Adm1_75")
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


## Load admin 1 ----

load("/data/Data/Robjects/admin_areas_1")


## GeoEPR ----

GeoEPR <- shapefile("/data/Data/GeoEPR-2018/GeoEPR.shp")

GeoEPR@data$iso <- countrycode(GeoEPR@data$statename, "country.name", "iso3c")
GeoEPR@data[is.na(GeoEPR@data$iso),] # nothing to worry about

GeoEPR <- GeoEPR[GeoEPR@data$iso %in% unique(admin_areas_1@data$GID_0),]

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
length(unique(GeoEPR@data$statename)); length(unique(admin_areas_1@data$GID_0))
admin_areas_1@data$NAME_0[admin_areas_1@data$GID_0 %in% setdiff(unique(admin_areas_1@data$GID_0), GeoEPR@data$iso)]
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
admin_areas_1@proj4string # same
# proj4string(GeoEPR) <- admin_areas_1@proj4string # copy admin info to GeoEPR

GeoEPR <- GeoEPR[GeoEPR@data$to >= 1992, ]

## Cleaning a self-intersecting polygon in admin_areas_1
admin_areas_1 <- clgeo_Clean(admin_areas_1, strategy = "BUFFER")
sum(gIsValid(admin_areas_1, byid=TRUE)==FALSE)

if(sum(!gIsValid(admin_areas_1, byid = T))>0)warning(paste(sum(!gIsValid(admin_areas_1, byid = T)),
                                                           "self-intersecting geometries in admin_areas_1"))
if(sum(!gIsValid(GeoEPR, byid = T))>0)warning(paste(sum(!gIsValid(GeoEPR, byid = T)),
                                                    "self-intersecting geometries in GeoEPR"))

# Intersect admin areas with ethnic homelands
ethnicities.in.admin1 <- raster::intersect(admin_areas_1, GeoEPR)

## Save and load
writeSpatialShape(ethnicities.in.admin1, "/data/Data/Shapefiles/ethnadmin1")
# ethnicities.in.admin1 <- shapefile("/data/Data/Shapefiles/ethnadmin1.shp")
# load("/data/Data/Robjects/admin_areas_1")

# add proj info
proj4string(ethnicities.in.admin1) <- admin_areas_1@proj4string 

ethnicities.in.admin1@data$area <- area(ethnicities.in.admin1)
ethnicities.in.admin1@data$areasqkm <- ethnicities.in.admin1@data$area / 1000000
ethnicities.in.admin1@data$area <- NULL
summary(ethnicities.in.admin1@data$areasqkm)

#Excluding archipelagos with primarily touristic economy
ethnicities.in.admin1 <- ethnicities.in.admin1[!ethnicities.in.admin1@data$NAME_0 %in% c("Mauritius", "Comoros"),]
#Note Seychelles and Sao Tome already excluded from GeoEPR so don't appear here (same for French and British islands)


## Add conflict events ----

Conflict <- shapefile("/data/Data/UCDP_GED/ged181")
proj4string(Conflict) <- admin_areas_1@proj4string 
e <- extent(-20, 60, -40, 40)
Conflict <- crop(Conflict, e)

for(i in unique(Conflict@data$year)){
  assign(paste0("Conflict.", i),
         Conflict[Conflict$year == i,])
  assign(paste0("pointsinpolygons", i),
         over(ethnicities.in.admin1,
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
  ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data,
                                      eval(parse(text = paste0("deathsinpolygons", i))))
  
  
  assign(paste0("maxdeathsinpolygons", i),
         unlist(lapply(eval(parse(text = paste0("pointsinpolygons", i))),
                       function(x){sum(x$high)})))
  assign(paste0("maxdeathsinpolygons", i), 
         data.table(eval(parse(text = paste0("maxdeathsinpolygons", i)))))
  setnames(eval(parse(text = paste0("maxdeathsinpolygons", i))),
           "V1",
           paste0("DM.", i))
  ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data,
                                      eval(parse(text = paste0("maxdeathsinpolygons", i))))
  
  
  assign(paste0("eventsinpolygons", i),
         unlist(lapply(eval(parse(text = paste0("pointsinpolygons", i))),
                       function(x){nrow(x)})))
  assign(paste0("eventsinpolygons", i), 
         data.table(eval(parse(text = paste0("eventsinpolygons", i)))))
  setnames(eval(parse(text = paste0("eventsinpolygons", i))),
           "V1",
           paste0("CE.", i))
  ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data,
                                      eval(parse(text = paste0("eventsinpolygons", i))))
  print(paste0("Added conflict data for year ", i))
}


## Save and load
writeSpatialShape(ethnicities.in.admin1, "/data/Data/Shapefiles/ethnadmin1_wconflict")
# ethnicities.in.admin1 <- shapefile("/data/Data/Shapefiles/ethnadmin1_wconflict.shp")
# load("/data/Data/Robjects/admin_areas_1")
# add proj info
proj4string(ethnicities.in.admin1) <- admin_areas_1@proj4string 


## ____ ----


## Load raster stacks (8) ----

ERArain75 <- stack("/data/Data/era_rain75/ERArain75")
ERAtemp75 <- stack("/data/Data/era_temp75/ERAtemp75")
SPEI4 <- stack("/data/Data/SPEI/SPEI4ymeans")
SPEI6 <- stack("/data/Data/SPEI/SPEI6ymeans")
SPEI12 <- stack("/data/Data/SPEI/SPEI12ymeans")

lights.stack.ca10 <- stack("/data/Data/Robjects/lights.stack.ca10.grd")
Pop <- stack("/data/Data/R_Rasters/Pop.grd")
Agri <- stack("/data/Data/R_Rasters/Agri.grd")


## ____ ----



## Extract rain means for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(ERArain75, ethnicities.in.admin1[i, ],
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
save(polygon.means.rain75, file = "/data/Data/Adm1_75/polygon.means.rain75")
# load("/data/Data/Adm1_75/polygon.means.rain75")


## Extract temp means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(ERAtemp75, ethnicities.in.admin1[i, ],
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
save(polygon.means.temp75, file = "/data/Data/Adm1_75/polygon.means.temp75")
# load("/data/Data/Adm1_75/polygon.means.temp75")


## Extract SPEI4 means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  if(i %% 500 == 0)print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(SPEI4, ethnicities.in.admin1[i, ],
                       weights = TRUE, normalizeWeights = F)
  if(i %% 500 == 0)print(paste0("Layer ", i, ": extraction completed."))
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
      if(i %% 500 == 0){print(paste0("Polygon ", i, ": added to polygon.means data table."))}
    }
  }
}
polygon.means.SPEI4 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.SPEI4, file = "/data/Data/Adm1_75/polygon.means.SPEI4")
# load("/data/Data/Adm1_75/polygon.means.SPEI4")


## Extract SPEI6 means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  if(i %% 500 == 0)print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(SPEI6, ethnicities.in.admin1[i, ],
                       weights = TRUE, normalizeWeights = F)
  if(i %% 500 == 0)print(paste0("Layer ", i, ": extraction completed."))
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
      if(i %% 500 == 0)print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.SPEI6 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.SPEI6, file = "/data/Data/Adm1_75/polygon.means.SPEI6")
# load("/data/Data/Adm1_75/polygon.means.SPEI6")


## Extract SPEI12 means for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  if(i %% 500 == 0)print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(SPEI12, ethnicities.in.admin1[i, ],
                       weights = TRUE, normalizeWeights = F)
  if(i %% 500 == 0)print(paste0("Layer ", i, ": extraction completed."))
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
      if(i %% 500 == 0)print(paste0("Polygon ", i, ": added to polygon.means data table."))
    }
  }
}
polygon.means.SPEI12 <- polygon.means
rm(polygon.means, cells.table, cols.nw, cols, allmyvals)
save(polygon.means.SPEI12, file = "/data/Data/Adm1_75/polygon.means.SPEI12")
# load("/data/Data/Adm1_75/polygon.means.SPEI12")



## Extract light sums for admin ----

j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  if(i %% 500 == 0)print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(lights.stack.ca10, ethnicities.in.admin1[i, ],
                       weights = TRUE, normalizeWeights = F)
  if(i %% 500 == 0)print(paste0("Layer ", i, ": extraction completed."))
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
      if(i %% 500 == 0)print(paste0("Polygon ", i, ": added to polygon.sums data table."))
    }
  }
}

polygon.sums.lights <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.lights, file = "/data/Data/Adm1_75/polygon.sums.lights")
# load("/data/Data/Adm1_75/polygon.sums.lights")


## Extract pop sums for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  if(i %% 500 == 0)print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(Pop, ethnicities.in.admin1[i, ],
                       weights = TRUE, normalizeWeights = F)
  if(i %% 500 == 0)print(paste0("Layer ", i, ": extraction completed."))
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
      if(i %% 500 == 0)print(paste0("Polygon ", i, ": added to polygon.sums data table."))
    }
  }
}

polygon.sums.pop <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.pop, file = "/data/Data/Adm1_75/polygon.sums.pop")
# load("/data/Data/Adm1_75/polygon.sums.pop")

## Extract agri sums for admin ----

# Verbose loop (see below for commented out alternative code)
j = 0
null.polygons <- c()

for(i in 1:length(ethnicities.in.admin1)){
  if(i %% 500 == 0)print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(Agri, ethnicities.in.admin1[i, ],
                       weights = TRUE, normalizeWeights = F)
  if(i %% 500 == 0)print(paste0("Layer ", i, ": extraction completed."))
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
      if(i %% 500 == 0)print(paste0("Polygon ", i, ": added to polygon.sums data table."))
    }
  }
}

polygon.sums.agri <- polygon.sums
rm(polygon.sums, cells.table, cols.nw, cols, allmyvals)
save(polygon.sums.agri, file = "/data/Data/Adm1_75/polygon.sums.agri")
# load("/data/Data/Adm1_75/polygon.sums.agri")

## Merge to polygons ----

# Bind rain
setnames(polygon.means.rain75, names(polygon.means.rain75), paste0("Rain", 1979:2013))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.means.rain75)

# Bind temp
setnames(polygon.means.temp75, names(polygon.means.temp75), paste0("Temp", 1979:2013))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.means.temp75)

# Bind light
setnames(polygon.sums.lights, names(polygon.sums.lights), paste0("Light", 1992:2013))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.sums.lights)

# Bind pop
setnames(polygon.sums.pop, names(polygon.sums.pop), paste0("Pop", c(1990, 1995, 2000, 2005, 2010, 2015)))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.sums.pop)

# Bind agri
setnames(polygon.sums.agri, names(polygon.sums.agri), c("agri.irrigated", "agri.rainfed"))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.sums.agri)


# Bind SPEI4
setnames(polygon.means.SPEI4, names(polygon.means.SPEI4), paste0("SPEI4", 1979:2013))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.means.SPEI4)

# Bind SPEI6
setnames(polygon.means.SPEI6, names(polygon.means.SPEI6), paste0("SPEI6", 1979:2013))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.means.SPEI6)


# Bind SPEI12
setnames(polygon.means.SPEI12, names(polygon.means.SPEI12), paste0("SPEI12", 1979:2013))
ethnicities.in.admin1@data <- cbind(ethnicities.in.admin1@data, polygon.means.SPEI12)

# Save
save(ethnicities.in.admin1, file = "/data/Data/Adm1_75/ethnicities.in.admin1_75_withrasterdata")
# load("/data/Data/Adm1_75/ethnicities.in.admin1_75_withrasterdata")
writeSpatialShape(ethnicities.in.admin1, "/data/Data/Adm1_75/ethnadmin1_75_wraster")

# Load
ethnicities.in.admin1 <- shapefile("/data/Data/Adm1_75/ethnadmin1_75_wraster")
load("/data/Data/Robjects/admin_areas_1")
proj4string(ethnicities.in.admin1) <- admin_areas_1@proj4string 

## ____ ----

## Prepare data ----


ethnicities.in.admin1@data$RowIndex <- seq.int(nrow(ethnicities.in.admin1@data))
regression.table <- ethnicities.in.admin1@data
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

save(regression.table, file = "/data/Data/Adm1_75/regression.table.75")
load("/data/Data/Adm1_75/regression.table.75")

## Create panel ----
setnames(regression.table, c("agri_rainf", "agri_irrig"), c("agri.rainfed", "agri.irrigated"))

regression.table[, unit.name := paste0(GID_1, ".", group)]
setkey(regression.table, unit.name)

regression.table[is.na(GID_1), unique(NAME_0)] # None

panel <- expand.grid(unit.name = regression.table[, unique(unit.name)],
                     year = 1979:2013) # Adapt years
panel <- data.table(panel)
setkey(panel, unit.name, year)
panel <- merge(panel,
               regression.table[, c("unit.name", "NAME_0", "NAME_1",
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

panel <- panel[year >= from & year <= to]


Panel75 <- copy(panel)
save(Panel75, file = "/data/Data/Adm1_75/Panel75_raw")
load("/data/Data/Adm1_75/Panel75_raw")


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

## Rm South Sudan duplicates ----

x <- Panel75[, sum(duplicated(year)), by = unit.name][, unit.name[V1 > 0]] 
length(x) # 22 south sudan units
# View(Panel75[unit.name %in% x]) # Assigned to both Sudan and South Sudan in 2011
## Official independence on 9th July, but referendum in January - so remove Sudan duplicates
pre.length <- nrow(Panel75)
Panel75 <- Panel75[!(unit.name %in% x & NAME_0 == "Sudan" & year == 2011)]
pre.length - nrow(Panel75)
pre.length <- nrow(Panel75)
Panel75 <- Panel75[!(unit.name %in% x & NAME_0 == "Ethiopia" & year == 1993)]
pre.length - nrow(Panel75)

## Save Panel75 ----
save(Panel75, file = "/data/Data/Adm1_75/Panel75_Final")
load("/data/Data/Adm1_75/Panel75_Final")


## ____ ----
