## Lights ----

# NAs in lights represented by 255 
# http://darrylmcleod.com/wp-content/uploads/2016/06/Night-Lights-and-ArcGIS-A-Brief-Guide.pdf

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

lights.stack[[]] # 255 :(

## Lights Africa ----

lights.stack.c <- stack("/data/Data/Robjects/lights.stack.c.grd")
cellStats(lights.stack.c, max) # 63! :) (maximum allowed range)

## ____ ----

## Rain, interpolated ----
# NA should be 9999 from link below
# https://confluence.ecmwf.int//display/ECC/Frequently+Asked+Questions#FrequentlyAskedQuestions-WhyamInotabletosetthemissingvalueintheGRIBmessage?

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_rain_", x),
                                        stack(paste0("/data/Data/era_rain/era_rain_", x, ".nc")))})
ERArain <- stack(stack.list)
cellStats(ERArain, max) # nothing weird
cellStats(ERArain, min) # some negative values -0.000000000000000003469447


## Temp, interpolated ----
stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_temp_", x),
                                        stack(paste0("/data/Data/era_temp/era_temp_", x, ".nc")))})
ERAtemp <- stack(stack.list)
cellStats(ERAtemp, max) # nothing weird
cellStats(ERAtemp, min) # nothing weird


## Rain, 0.75° resolution ----
stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_rain75_", x),
                                        stack(paste0("/data/Data/era_rain75/era_rain75_", x, ".nc")))})
ERArain <- stack(stack.list)
cellStats(ERArain, max) # nothing weird
cellStats(ERArain, min) # mostly zeroes, some +ve values 0.000000000000000003469447
# same number turning negative in interpolated dataset!

stack.list <- lapply(c(1970, 1980, 1990, 2000, 2010),
                     function(x){assign(paste0("era_temp75_", x),
                                        stack(paste0("/data/Data/era_temp75/era_temp75_", x, ".nc")))})
ERAtemp <- stack(stack.list)
cellStats(ERAtemp, max) # nothing weird
cellStats(ERAtemp, min) # nothing weird

## ____ ----

## Conclusions ----

# Lights OK within Africa (no 255, i.e. no NAs)
# Set negative to zero in interpolated rain
# rain at 0.75° res. OK
# temp OK
