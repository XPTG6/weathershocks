## Setup ----
setwd("/data/Data")
rm(list = ls())
packages <- c("raster", "ncdf4", "rgdal")

if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)

## Parameters ----
years <- c(1989:1989)
# ISSUE: 1980 and following give error in stackApply (or any other operation...):
## Error in ncdim_len(nc$id, "xysize") : NAs in foreign function call (arg 1)

## Raster operations ----
for (i in years){
  print(paste("Starting max temp for year", i))
  maxtemp <- stack(paste0("/data/Data/PythonScripts/era_minmaxtemp_", i, ".nc"), varname = "mx2t")

  days <- substr(names(maxtemp), 2, 11)
  days <- unique(days)
  days <- days[-length(days)]
  yearmonth <- substr(names(maxtemp), 2, 8) # extract month and year
  yearmonth <- yearmonth[-length(yearmonth)]
  new_z <- unique(yearmonth)
  
  indices <- rep(seq_len(nlayers(maxtemp)/8), each=8) # select all forecasts by day
  maxtemp <- stackApply(maxtemp, indices, max) # take daily max
  
  days <- substr(days, 1, 7) 
  indices2 <- c(0,cumsum(as.numeric(days[1:(length(days)-1)] != days[2:length(days)]))) # select all days by month
  maxtemp <- stackApply(maxtemp, indices2, mean) # take monthly mean
  
  names(maxtemp) <- as.character(new_z)
  maxtemp <- setZ(maxtemp, new_z)
  assign(paste0("maxtemp_", i), maxtemp)
  writeRaster(eval(parse(text = paste0("maxtemp_", i))), paste0("/data/Data/era_maxtemp/maxtemp_", i))
  rm(list = c(paste0("maxtemp_", i), "maxtemp"))

# do same for min values
  print(paste("Starting min temp for year", i))
  mintemp <- stack(paste0("/data/Data/PythonScripts/era_minmaxtemp_", i, ".nc"), varname = "mn2t")
  
  days <- substr(names(mintemp), 2, 11)
  days <- unique(days)
  days <- days[-length(days)]
  yearmonth <- substr(names(mintemp), 2, 8) # extract month and year
  yearmonth <- yearmonth[-length(yearmonth)]
  new_z <- unique(yearmonth)
  
  indices <- rep(seq_len(nlayers(mintemp)/8), each=8) # select all forecasts by day
  mintemp <- stackApply(mintemp, indices, min) # take daily min
  
  days <- substr(days, 1, 7) 
  indices2 <- c(0,cumsum(as.numeric(days[1:(length(days)-1)] != days[2:length(days)]))) # select all days by month
  mintemp <- stackApply(mintemp, indices2, mean) # take monthly mean
  
  names(mintemp) <- as.character(new_z)
  mintemp <- setZ(mintemp, new_z)
  assign(paste0("mintemp_", i), mintemp)
  writeRaster(eval(parse(text = paste0("mintemp_", i))), paste0("/data/Data/era_mintemp/mintemp_", i))
  rm(list = c(paste0("mintemp_", i), "mintemp"))
  print(paste("Completed year", i))
}

# file.remove("/data/Data/PythonScripts/era_minmaxtemp_1989.nc")

