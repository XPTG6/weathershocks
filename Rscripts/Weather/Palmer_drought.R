## Setup ----
setwd("/data/Data")
rm(list = ls())
packages <- c("raster", "ncdf4", "rgdal", "data.table", "devtools")

if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)

install_github("cszang/pdsi")
# (on Linux)
library(pdsi)
pdsi::build_linux_binary()

## Rainfall ----

ERArain <- stack("/data/Data/ERA_Interim_rainfall.nc")
months <- substr(names(ERArain), 2, 8)
indices <- which(as.integer(substr(months, 1, 4))<=2013)
months <- months[indices]
ERArain <- subset(ERArain, indices)
new_z <- unique(months)
indices <- rep(seq_len(nlayers(ERArain)/2), each=2)
ERArain <- stackApply(ERArain, indices, sum)
ERArain <- calc(ERArain, function(x){x*1000})
names(ERArain) <- as.character(new_z)
ERArain <- setZ(ERArain, new_z)

## Temperature ----

ERAtemp <- stack("/data/Data/ERA_Interim_temp.nc")
months <- substr(names(ERAtemp), 2, 8)
indices <- which(as.integer(substr(months, 1, 4))<=2013)
months <- months[indices]
ERAtemp <- subset(ERAtemp, indices)
ERAtemp <- calc(ERAtemp, function(x){x-273.15})


## AWC ----
AWC <- raster("ISRIC_available_water.tif")
AWC <- projectRaster(AWC, ERArain)

# plot(AWC)
# density(AWC)

## Make data table ----

ERArain <- rasterToPolygons(ERArain)
ERAtemp <- as.matrix(ERAtemp)
latitude <- coordinates(ERArain)[, 2]
awc <- as.vector(AWC)

## TAKE TOO LONG :(
# for (i in 1:dim(ERAtemp)[1]){
#   if(i == 1){
#     x <- ERAtemp[i,]
#     names <- names(x)
#     climate <- data.table(cell = rep_len(i, dim(ERAtemp)[2]),
#                           year = as.integer(substr(names, 2, 5)),
#                           month = as.integer(substr(names, 7, 8)),
#                           temp = x,
#                           rain = ERArain@data[i,])
#     cat(paste("Finished cell", i))
#     }else{
#       x <- ERAtemp[i,]
#       names <- names(x)
#       newcell <- data.table(cell = rep_len(i, dim(ERAtemp)[2]),
#                             year = as.integer(substr(names, 2, 5)),
#                             month = as.integer(substr(names, 7, 8)),
#                             temp = x,
#                             rain = ERArain@data[i,])
#   climate <- rbind(climate, newcell)
#   if(i %% 500 == 0) {
#     cat(paste("Finished cell", i))
#   }
# }}