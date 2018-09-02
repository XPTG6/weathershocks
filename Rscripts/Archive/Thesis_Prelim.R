## Preliminary work for thesis

## Setup ----
setwd("/data/Data")
rm(list = ls())
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "gridExtra", "SPEI",
              "lfe", "rworldmap", "sp", "RCurl", "grid", "foreign",
              "maptools", "maps", "shinydashboard", "DT", "reshape2", "plotly", "lubridate",
              "devtools", "rNOMADS", "shinyjs", "dendextend", "ggdendro",
              "raster", "ncdf4", "chron", "lubridate", "rasterVis", "rgdal", "geomerge",
              "Rcpp", "sf", "countrycode", "snow", "cleangeo"#, "mapview"
)
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)
# note sf fails to load unless it's done first in a new session

detach('package:shinyjs') # to avoid conflict with raster::print

devtools::install_github("andrie/rrd")
library(rrd)

## Rainfall using raster ----

ERArain <- stack("/data/Data/ERA_Interim_rainfall.nc")
year <- as.integer(substr(names(ERArain), 2, 5))
indices <- which(year<=2013)
ERArain <- subset(ERArain, indices)
new_z <- unique(year[year<=2013])
indices <- rep(seq_len(nlayers(ERArain)/24), each=24)
ERArain <- stackApply(ERArain, indices, sum)
names(ERArain) <- as.character(new_z)
ERArain <- setZ(ERArain, new_z)




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


## Check projection
readOGR("/data/Data/GADM/gadm36_1.shp")
# Output: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
ogrInfo(dsn="/data/Data/GADM", layer="gadm36_1") #faster


## Extract rain sums for admin ----

weights <- extract(ERArain, admin_areas_1,
                   weights = TRUE, normalizeWeights = F)

save(weights, file = "/data/Data/Robjects/weights")
# load("/data/Data/Robjects/weights")

weights <- lapply(weights, as.data.table)
sums <- lapply(weights, function(x){
  cols <- setdiff(names(x), "weight")
  x[, lapply(.SD, function(x){sum(x*weight)}), .SDcols = cols]})

rain.sums <- rbindlist(sums)
save(rain.sums, file = "/data/Data/Robjects/rain.sums")
# load("/data/Data/Robjects/rain.sums")

## Extract light sums for admin ----
j = 0
null.polygons <- c()

for(i in 1:length(admin_areas_1)){
  print(paste0("Starting polygon ", i, "..."))
  allmyvals <- extract(lights.stack.ca10, admin_areas_1[i, ])
  print(paste0("Layer ", i, ": extraction completed."))
  cells.table <- as.data.table(allmyvals)
  if(nrow(cells.table) == 0){
    j = j + 1
    warning(paste0("Layer ", i, " is null."))
    null.polygons[[1]] <- i
  }else{
  cols <- names(cells.table)
  if(sum(is.na(cells.table))>0)warning(paste0(sum(is.na(cells.table)),
                                              "NAs in cells.table for polygon ", i, "."))
  if(i == 1){
    polygon.sums <- cells.table[,
                                lapply(.SD,
                                       function(x)if(is.null(x)){NA}else{sum(x,
                                                                             na.rm = T)})]
    print(paste0("Polygon ", i, ": created polygon.sums data table."))
    }else{
      polygon.sums <- rbind(polygon.sums,
                    cells.table[,
                                lapply(.SD,
                                       function(x)if(is.null(x)){NA}else{sum(x, na.rm = T)})])
      print(paste0("Polygon ", i, ": added to polygon.sums data table."))
    }
  }
}

save(polygon.sums, file = "/data/Data/Robjects/polygon.sums.lights")
load("/data/Data/Robjects/polygon.sums.lights")
null.polygons <- c(159:180, 488) #fix null polygons above
save(null.polygons, file = "/data/Data/Robjects/null.polygons")


null_areas <- admin_areas_1[null.polygons,]
null_areas@data #excluding Cape Verde and one small island from Mauritius
admin_areas_1@data[admin_areas_1@data$NAME_0 == "Mauritius", ]

## Merge to polygons ----

# Exclude islands beyond extent
admin_areas_1 <- admin_areas_1[setdiff(1:length(admin_areas_1), null.polygons),]

# Bind rain
setnames(rain.sums, names(rain.sums), paste0("Rain", 1990:2013))
admin_areas_1@data <- cbind(admin_areas_1@data, rain.sums)

# Bind light
setnames(polygon.sums, names(polygon.sums), paste0("Light", 1992:2013))
admin_areas_1@data <- cbind(admin_areas_1@data, polygon.sums)

save(admin_areas_1, file = "/data/Data/Robjects/admin_areas_1_withrasterdata")
# load("/data/Data/Robjects/admin_areas_1_withrasterdata")


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

# #ISSUE: removing 3 polygons b/c of error ('TopologyException: side location conflict')
# g <- GeoEPR[!GeoEPR@data$groupid %in% c(9210, 2000, 4000), ] ## removing three polygons in Uganda
# # g <- clgeo_Clean(GeoEPR) # doesn't help
# # g <- GeoEPR[GeoEPR@data$groupid != 9210, ] # removing one only doesn't help
# ethnicities.in.admin1 <- over(admin_areas_1, g, returnList = T, minDimension = 2)
# # ISSUE: still matches homelands from neighbouring countries, probably as a result of shared border

ethnicities.in.admin1 <- raster::intersect(admin_areas_1, GeoEPR)

save(ethnicities.in.admin1, file = "/data/Data/Robjects/ethnicities.in.admin1")
# load("/data/Data/Robjects/ethnicities.in.admin1")
writeSpatialShape(ethnicities.in.admin1, "/data/Data/Shapefiles/ethnadmin1")

ethnicities.in.admin1@data$area <- area(ethnicities.in.admin1)
ethnicities.in.admin1@data$areasqkm <- ethnicities.in.admin1@data$area / 1000000
ethnicities.in.admin1 <- ethnicities.in.admin1[ethnicities.in.admin1@data$areasqkm >= 30,]


## ISSUE: sf crashes
# GeoEPR <- st_as_sf(GeoEPR)
# admin_areas_1 <- st_as_sf(admin_areas_1)
# ethnicities.in.admin1 <- st_intersects(admin_areas_1, GeoEPR)


## Add population ----

pop90 <- raster("/data/Data/SEDAC-bil/afup90ag.bil")

e <- extent(-20.2, 60.2, -40.2, 40.2)
pop90 <- crop(pop90, e)
# Aggregate to 10x lower resolution
pop90.a <- aggregate(pop90, 10, fun = function(x, ...)if(is.null(x)){NA}else{sum(x, na.rm = T)})
writeRaster(pop90.a, "/data/Data/pop90_a", format = "raster", overwrite=TRUE)


popcells.by.poly <- extract(pop90.a, ethnicities.in.admin1)
pop.by.poly <- lapply(popcells.by.poly, function(x)if(is.null(x)){NA}else{sum(x, na.rm = T)})

ethnicities.in.admin1@data$population <- unlist(pop.by.poly)

save(ethnicities.in.admin1, file = "/data/Data/Robjects/ethnicities.in.admin1.withpop")
# load("/data/Data/Robjects/ethnicities.in.admin1.withpop")
writeSpatialShape(ethnicities.in.admin1, "/data/Data/Shapefiles/ethnadmin1.wpop")


## Prepare data ----

# exclude pre-1994 (at least two years)
ethnicities.in.admin1 <- ethnicities.in.admin1[ethnicities.in.admin1@data$to >= 1994, ]

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
# View(regression.table[NAME_0 != statename, .(NAME_0, statename)])
# x <- regression.table[NAME_0 != statename,]
# setkey(x, areasqkm)
# x

# diagnostic plot
plot(ethnicities.in.admin1[ethnicities.in.admin1@data$RowIndex == 585, ])
map(admin_areas_0,add=T,fill=F, col="red", lwd = 0.2)
# clearly just approximation error in drawing boundaries

# Exclude areas picked up in different countries
regression.table <- regression.table[NAME_0 == statename]
## ISSUE: here there are also areas that have been annexed in war / territorial changes
## need more rigorous approach!

# Exclude pop < 1000
regression.table <- regression.table[population >= 1000]

save(regression.table, file = "/data/Data/Robjects/regression.table")
load("/data/Data/Robjects/regression.table")

## Add ethn power ----

EPR_core <- read.csv("/data/Data/EPR-2018.csv")
EPR_core <- data.table(EPR_core)
EPR_core <- EPR_core[, .(gwgroupid, status, from, to)]

setkey(EPR_core, gwgroupid, from, to)
setkey(regression.table, gwgroupid, from, to)
regression.table <- foverlaps(regression.table, EPR_core,
                              by.x = c("gwgroupid", "from", "to"),
                              by.y = c("gwgroupid", "from", "to"))
## Create panel ----

regression.table[, unit.name := paste0(GID_1, ".", group)]
setkey(regression.table, unit.name)

panel <- expand.grid(unit.name = regression.table[, unique(unit.name)],
                     year = 1990:2013)
panel <- data.table(panel)
setkey(panel, unit.name, year)
panel <- merge(panel,
               regression.table[, .(unit.name, NAME_0, NAME_1,
                                    areasqkm, population,
                                    from, to, i.from, i.to,
                                    status)],
               by = "unit.name",
               all.x = T,
               allow.cartesian=TRUE)
panel <- panel[year >= from & year <= to]
panel <- panel[year >= i.from & year <= i.to]


for(i in 1992:2013){
assign(paste0("panel", i),
       merge(panel[year == i],
             regression.table[, c("unit.name", paste0("Light", i), paste0("Rain", i)), with=FALSE],
             by = "unit.name",
             all.x = T))
  eval(parse(text = paste0("panel", i)))[, Light := eval(parse(text = paste0("Light", i)))]
  eval(parse(text = paste0("panel", i)))[, Rain := eval(parse(text = paste0("Rain", i)))]
  eval(parse(text = paste0("panel", i)))[, paste0("Light", i) := NULL]
  eval(parse(text = paste0("panel", i)))[, paste0("Rain", i) := NULL]
}

for(i in 1990:1991){
  assign(paste0("panel", i),
         merge(panel[year == i],
               regression.table[, c("unit.name", paste0("Rain", i)), with=FALSE],
               by = "unit.name",
               all.x = T))
  eval(parse(text = paste0("panel", i)))[, Rain := eval(parse(text = paste0("Rain", i)))]
  eval(parse(text = paste0("panel", i)))[, paste0("Rain", i) := NULL]
  eval(parse(text = paste0("panel", i)))[, Light := NA]
}

Panel <- rbindlist(mget(paste0("panel", 1990:2013)))

save(Panel, file = "/data/Data/Robjects/Panel")
load("/data/Data/Robjects/Panel")


## Run regression ----

Panel[, Rain.km := Rain/areasqkm]
Panel[, Light.cap := Light/population]
Panel[, year.f := as.factor(year)]
Panel[, state.f := as.factor(NAME_0)]
Panel[, admin1.f := as.factor(NAME_1)]

Panel[, year.state.f := interaction(year.f, state.f)]

Panel[year == 1990, Rain.km.1990 := Rain.km]
Panel[, Rain.km.1990 := max(Rain.km.1990, na.rm = T), by = unit.name]
Panel <- Panel[!is.infinite(Rain.km.1990)] #ISSUE: arbitrary fix!
setkey(Panel, unit.name, year)
Panel[, Rain.km.lag := data.table::shift(Rain.km, 1, type = "lag"), by = unit.name]

Panel[, Rain.km.mean := mean(Rain.km), by = unit.name]
Panel[, Rain.km.d := Rain.km - Rain.km.mean]
Panel[, Rain.km.lag.d := Rain.km.lag - Rain.km.mean]


Panel <- Panel[, .(unit.name,
                   year.f, state.f, admin1.f, year.state.f,
                   Rain.km.d, Rain.km.lag.d, Rain.km.1990,
                   Light.cap,
                   status)]
Panel <- Panel[!is.na(Light.cap)]

Panel[, status := relevel(status, "JUNIOR PARTNER")]

reg1 <- felm(Light.cap ~ Rain.km.d + Rain.km.d*status + status + Rain.km.1990|year.f + state.f + admin1.f,
             Panel)
summary(reg1)


## _____ ----

## Nightlights exploratory ----

lights2013 <- raster("/data/Data/Nightlights/F182013.v4/F182013.v4c_web.stable_lights.avg_vis.tif")
print(lights2013) # output: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
lights2013 <- setZ(lights2013, as.Date("2013-01-01"), name='time') # add time

# plotting
lightsplot <- levelplot(lights2013, margin = F,
                        #at=cutpts,
                        par.settings = lightsTheme,
                        pretty=TRUE)


## Rainfall exploratory ----

ERArain <- stack("/data/Data/ERA_Interim_rainfall.nc")
print(ERArain)
rain2013.01 <- raster(ERArain, layer = "X2013.01.01.00.00.00")
rain1992.01 <- raster(ERArain, layer = "X1992.01.01.00.00.00")

plot(rain2013.01)

# map the data
world.outlines <- map("world", plot=FALSE)
world.outlines.sp <- map2SpatialLines(world.outlines, proj4string = CRS("+proj=longlat"))

mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")))
# cutpts <- c(-2.5, -2.0, -1.5, -1, -0.5, 0, 0.5, 1.0, 1.5, 2.0, 2.5)
plt13 <- levelplot(rain2013.01, margin = F,
                   #at=cutpts,
                   cuts=11, pretty=TRUE, par.settings = mapTheme,
                   main="rainfall 2013.01")
plot2013.01 <- plt13 + layer(sp.lines(world.outlines.sp, col = "black", lwd = 0.5))
plot2013.01

plt92 <- levelplot(rain1992.01, margin = F,
                   #at=cutpts,
                   cuts=11, pretty=TRUE, par.settings = mapTheme,
                   main="rainfall 1992.01")
plot1992.01 <- plt92 + layer(sp.lines(world.outlines.sp, col = "black", lwd = 0.5))
plot1992.01

grid.arrange(plot1992.01, plot2013.01, ncol = 2)


## R Nightlights ----

devtools::install_github("chrisvwn/Rnightlights")
library(Rnightlights)

#performance enhancement if you have gdal installed
# pkgOptions(cropMaskMethod = "gdal", extractMethod = "gdal", deleteTiles = TRUE)

#Optional performance enhancement. If extractMethod="rast" you can specify the number of
#CPU cores to use in parallel
#pkgOptions(extractMethod = "rast", numCores=4)

Rnightlights::getAllNlTypes()
Rnightlights::searchAdmLevel("NGA", "")
NGA <- Rnightlights::readCtryPolyAdmLayer("NGA", "NGA_adm1") #Check country polygon at specified admin level
# plot(NGA)


#download and process monthly VIIRS stats at the highest admin level
highestAdmLevelStats <- Rnightlights::getCtryNlData(ctryCode = "NGA", 
                                                    admLevel = "highest",
                                                    nlType = "OLS.Y", 
                                                    nlPeriods = nlRange("1992", "2013"), 
                                                    nlStats = "sum",
                                                    ignoreMissing=FALSE)



#Optionally plot the data
#melt the stats into key-value format for easy multi-line plotting with ggplot2
highestAdmLevelStats <- melt(highestAdmLevelStats,
                             id.vars = grep("NL_", names(highestAdmLevelStats), 
                                            invert=TRUE), 
                             variable.name = "nlPeriod", 
                             value.name = "radiancesum")

#extract date from the NL col names
highestAdmLevelStats$nlPeriod <- substr(highestAdmLevelStats$nlPeriod, 12, 17)

#format period as date
highestAdmLevelStats$nlPeriod <- ymd(paste0(substr(highestAdmLevelStats$nlPeriod, 1,4), 
                                            "-",substr(highestAdmLevelStats$nlPeriod, 5,6), "-01"))

#plot 2nd admin level sums for the year
g <- ggplot(data = highestAdmLevelStats, 
            aes(x=nlPeriod, y=radiancesum, 
                color=highestAdmLevelStats[[2]])) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
  geom_line()+geom_point() + labs(color = names(highestAdmLevelStats)[2]) + 
  xlab("Month") + 
  ylab("Sum of Radiances") +
  ggtitle(paste0(unique(names(highestAdmLevelStats)[2]), " sum of radiances for ", "Nigeria"))

print(g)

#quick conversion to interactive map with plotly
ggplotly(g)


## NetCDF (rainfall) ----

# Total precipitation
ERArain <- nc_open("Data/ERA_Interim_rainfall.nc")
print(ERArain)

# get dimensions
lon <- ncvar_get(ERArain, "longitude")
lat <- ncvar_get(ERArain, "latitude")
time <- ncvar_get(ERArain, "time")
time_units <- ncatt_get(ERArain, "time")

# get variable
dname <- "tp"
rain.array <- ncvar_get(ERArain, dname)
dlname <- ncatt_get(ERArain, dname, "long_name")
dunits <- ncatt_get(ERArain, dname, "units")
fillvalue <- ncatt_get(ERArain, dname, "_FillValue")
dim(rain.array)

# get global attributes
title <- ncatt_get(ERArain, 0, "title")
institution <- ncatt_get(ERArain, 0, "institution")
datasource <- ncatt_get(ERArain, 0, "source")
references <- ncatt_get(ERArain, 0, "references")
history <- ncatt_get(ERArain, 0, "history")
Conventions <- ncatt_get(ERArain, 0, "Conventions")

# close NetCDF file
nc_close(ERArain)

# split the time units string into fields
time <- as.POSIXct(time*3600, origin='1900-01-01 00:00')

# NAs
rain.array[rain.array == fillvalue$value] <- NA
sum(is.na(rain.array)) # no NAs


## Walsh Nightlights package ----

devtools::install_github("walshc/nightlights")
library(nightlights)
?extractNightLights
?downloadNightLights


## Harari La Ferrara ----

Harari <- shapefile("/data/Data/HarariLaFerrara/dataset/raster_Africa.shp")

install.packages("readstata13")
library(readstata13)
Harari.data <- read.dta13("/data/Data/HarariLaFerrara/dataset/geoconflict_main.dta")

Harari@data
