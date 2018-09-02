## Setup ----
setwd("/data/Data")
rm(list = ls())
packages <- c("raster", "ncdf4", "rgdal", "data.table",
              "devtools", "SPEI", "zoo", "doSNOW")

if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)


## Rainfall ----

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_rain75_", x),
                                        stack(paste0("/data/Data/era_rain75/era_rain75_", x, ".nc")))})
years <- lapply(stack.list,
                function(x){as.integer(substr(names(x), 2, 5))})
years <- unlist(years)
ERArain <- stack(stack.list)
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

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_temp75_", x),
                                        stack(paste0("/data/Data/era_temp75/era_temp75_", x, ".nc")))})
years <- lapply(stack.list,
                function(x){as.integer(substr(names(x), 2, 5))})
years <- unlist(years)
ERAtemp <- stack(stack.list)
months <- substr(names(ERAtemp), 2, 8)
indices <- which(as.integer(substr(months, 1, 4))<=2013)
months <- months[indices]
ERAtemp <- subset(ERAtemp, indices)
ERAtemp <- calc(ERAtemp, function(x){x-273.15})
new_z <- unique(months)
names(ERAtemp) <- as.character(new_z)
ERAtemp <- setZ(ERAtemp, new_z)

## Evapotranspiration ----

# lat <- setValues(ERAtemp[[1]], coordinates(ERAtemp)[, "y"])
# 
thor <- Vectorize(SPEI::thornthwaite)
# 
# PET <- raster::overlay(ERAtemp, lat, fun = thor)
# writeRaster(PET, filename  = "/data/Data/PET/PET")

lat <- coordinates(ERAtemp)[, "y"]
Tave <- as.data.frame(t(as.matrix(ERAtemp)))

PET <- thornthwaite(Tave, lat)
dim(PET)

## Check that Thorntwaite is working fine: YES
# L <- rep(40.50, 11772)
# P <- as.data.frame(t(thornthwaite(Tave, L)))
# sum(P == PET); sum(P != PET) #
####


## SPEI ----
Rain <- t(as.matrix(ERArain))
dim(Rain)

CWB <- Rain - PET
dim(CWB)

SPEI4 <- spei(CWB, 4, kernel=list(type='gaussian', shift=0))
SPEI6 <- spei(CWB, 6, kernel=list(type='gaussian', shift=0))
SPEI12 <- spei(CWB, 12, kernel=list(type='gaussian', shift=0))

## Save 
save(SPEI4, file = "/data/Data/Robjects/SPEI4")
save(SPEI6, file = "/data/Data/Robjects/SPEI6")
save(SPEI12, file = "/data/Data/Robjects/SPEI12")
####


SPEI4 <- SPEI4$fitted
SPEI4 <- t(SPEI4)
SPEI4.raster <- setValues(ERAtemp, SPEI4)
writeRaster(SPEI4.raster, "/data/Data/SPEI/SPEI4")

SPEI6 <- SPEI6$fitted
SPEI6 <- t(SPEI6)
SPEI6.raster <- setValues(ERAtemp, SPEI6)
writeRaster(SPEI6.raster, "/data/Data/SPEI/SPEI6")

SPEI12 <- SPEI12$fitted
SPEI12 <- t(SPEI12)
SPEI12.raster <- setValues(ERAtemp, SPEI12)
writeRaster(SPEI12.raster, "/data/Data/SPEI/SPEI12")


## Check NAs
# sum(is.na(SPEI4))/(dim(SPEI4)[1]*dim(SPEI4)[2])
# x <- SPEI4[, !names(SPEI4) %in% c("V1", "V2", "V3")]
# sum(is.na(x)) # zero, all NAs in first three years
#####





## ____ ----

## garbage bin ----
 
## Parallelised processing: GIVES ERROR

#setting up cluster

# NumberOfCluster <- 6
# cl <- makeCluster(NumberOfCluster, outfile = "")
# registerDoSNOW(cl)
# junk <- clusterEvalQ(cl,library(raster))
# 
# #perform calculations on each raster
# months <- substr(names(ERAtemp), 2, 8)
# 
# foreach(i=1:dim(ERAtemp)[3]) %dopar% {
#   library(raster)
#   rasterOptions(tmpdir=paste0("/data2/rasttemp_", i))
# 
#   assign(paste0("PET_", months[i]),
#          raster::overlay(ERAtemp[[i]], lat[[i]], fun = thor))
# 
#   writeRaster(paste0("PET_", months[i]),
#               filename  = paste0("/data/Data/PET/PET_", months[i]))
#   print(paste0("Completed layer ", i, " of ", dim(ERAtemp)[3]))
#   rm(paste0("PET_", months[i]))
#   gc()
#   removeTmpFiles()
#   gc()
# }
# 
# #stop cluster
# 
# stopCluster(cl)

