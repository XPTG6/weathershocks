
## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
rm(list = ls())
`%nin%` <- function(a, b){! a %in% b}
ProportionNA <- function(x){x[, lapply(.SD, function(y){sum(is.na(y))/length(y)})]}
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "grid", "gridExtra","classInt",
              "lemon", "SPEI", "RcppRoll", "kableExtra", "plyr", "viridis",
              "lfe", "rworldmap", "sp", "RCurl", "grid", "foreign", "xlsx", "stargazer",
              "maptools", "maps", "shinydashboard", "DT", "reshape2", "plotly", "lubridate",
              "devtools", "rNOMADS", "shinyjs", "dendextend", "ggdendro",
              "raster", "ncdf4", "chron", "lubridate", "rasterVis", "rgdal", "geomerge",
              "Rcpp", "countrycode", "snow", "cleangeo", "ggmap"#, "mapview",
)
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)
# note sf fails to load unless it's done first in a new session

detach('package:shinyjs') # to avoid conflict with raster::print

## Load Panel (Adm2 0.75 res) ----

load("/data/Data/Adm2_75/Panel75_Final")

## Preparation ----

which.pop <- "Pop1990"
source("~/Rscripts/Prep/Adm2_75_PanelPrep.R")

setdiff(unique(admin_areas_2@data$NAME_0), Panel75[, unique(NAME_0)])
setdiff(Panel75[year == 2013, unique(NAME_0)], Panel75[year == 1993, unique(NAME_0)])
setdiff(Panel75[year == 1993, unique(NAME_0)], Panel75[year == 2013, unique(NAME_0)])

## ____ ----

## Load spatial ----
ethnicities.in.admin2 <- shapefile("/data/Data/Adm2_75/ethnadmin2_75_wraster")
load("/data/Data/Robjects/admin_areas_2")
proj4string(ethnicities.in.admin2) <- admin_areas_2@proj4string 

ethnicities.in.admin2@data$unit.name <- paste0(ethnicities.in.admin2@data$GID_2,
                                               ".",
                                               ethnicities.in.admin2@data$group)

## Maps Prep 2013 ----
dev.off()

setkey(Panel75, unit.name, year)
Maps.2013 <- Panel75[year == 2013, .SD,
                      by = unit.name,
                      .SDcols = c("SPEI4", "SPEI6", "SPEI12", "Light.cap", "Light.cap.adj", "Light.growth",
                                  "Rain", "Temp", "Rain.shock", "Temp.shock")]
Maps.2013[, Log.Light := log(Light.cap.adj)]
Maps.2013[, Light.growth.symlog := if(abs(Light.growth) < 0.0000000001){0}else{sign(Light.growth)*log(1+abs(Light.growth))}]
Maps.2013[, Light.growth.f := cut(Light.growth,
                                  breaks = c(-Inf, seq(-1, 1, 0.2), Inf),
                                  labels = c("<-1", "-1,.8", "-.8,.6", "-.6,.4", "-.4,.2",
                                             "-.2,0", "0,.2", ".2,.4", ".4,.6",
                                             ".6,.8", ".8,1", ">1"))]


plotdata <- ethnicities.in.admin2[, c("unit.name", "NAME_0", "group")]
plotdata <- merge(plotdata, Maps.2013, by = "unit.name")
ProportionNA(data.table(plotdata@data))
plotdata <- plotdata[!is.na(plotdata$SPEI4),]

## Plots ----
# Panel75[, list(unique(NAME_0))]

# SPEI4
SPEI4.map <- spplot(plotdata, zcol = "SPEI4",
                    col.regions = viridis(20, begin = 0, end = 1,
                                          direction = -1, option = "inferno"),
                    lwd = 0.1)
title.grob <- textGrob(label = "SPEI4", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
SPEI4 <- arrangeGrob(SPEI4.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/SPEI4_2013.png",
       width = 9, height = 9, units = "cm",
       SPEI4, type = "cairo")

# SPEI6
SPEI6.map <- spplot(plotdata, zcol = "SPEI6",
                    col.regions = viridis(20, begin = 0, end = 1,
                                          direction = -1, option = "inferno"),
                    lwd = 0.1)
title.grob <- textGrob(label = "SPEI6", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
SPEI6 <- arrangeGrob(SPEI6.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/SPEI6_2013.png",
       width = 9, height = 9, units = "cm",
       SPEI6, type = "cairo")

# SPEI12
SPEI12.map <- spplot(plotdata, zcol = "SPEI12",
                     col.regions = viridis(20, begin = 0, end = 1,
                                           direction = -1, option = "inferno"),
                     lwd = 0.1)
title.grob <- textGrob(label = "SPEI12", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
SPEI12 <- arrangeGrob(SPEI12.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/SPEI12_2013.png",
       width = 9, height = 9, units = "cm",
       SPEI12, type = "cairo")

# Rain.shock
Rain.shock.map <- spplot(plotdata, zcol = "Rain.shock",
                         col.regions = viridis(20, begin = 0, end = 1,
                                               direction = -1, option = "inferno"),
                         lwd = 0.1)
title.grob <- textGrob(label = "Rainfall shock", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Rain.shock <- arrangeGrob(Rain.shock.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Rain.shock_2013.png",
       width = 9, height = 9, units = "cm",
       Rain.shock, type = "cairo")

# Temp.shock
Temp.shock.map <- spplot(plotdata, zcol = "Temp.shock",
                         col.regions = viridis(20, begin = 0, end = 1,
                                               direction = 1, option = "inferno"),
                         lwd = 0.1)
title.grob <- textGrob(label = "Temperature shock", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Temp.shock <- arrangeGrob(Temp.shock.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Temp.shock_2013.png",
       width = 9, height = 9, units = "cm",
       Temp.shock, type = "cairo")

# Temp
Temp.map <- spplot(plotdata, zcol = "Temp",
                         col.regions = viridis(20, begin = 0, end = 1,
                                               direction = 1, option = "inferno"),
                         lwd = 0.1)
title.grob <- textGrob(label = "Temperature (°C)", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Temp <- arrangeGrob(Temp.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Temp_2013.png",
       width = 9, height = 9, units = "cm",
       Temp, type = "cairo")

# Rain
Rain.map <- spplot(plotdata, zcol = "Rain",
                   col.regions = viridis(20, begin = 0, end = 1,
                                         direction = -1, option = "inferno"),
                   lwd = 0.1)
title.grob <- textGrob(label = "Rainfall (mm/day)", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Rain <- arrangeGrob(Rain.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Rain_2013.png",
       width = 9, height = 9, units = "cm",
       Rain, type = "cairo")


# Log.Light
colfunc <- colorRampPalette(c("black", "white"))
Log.Light.map <- spplot(plotdata, zcol = "Log.Light",
                   col.regions = colfunc(20),
                   lwd = 0.1)
title.grob <- textGrob(label = "Log light p.c.", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Log.Light <- arrangeGrob(Log.Light.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Log.Light_2013.png",
       width = 9, height = 9, units = "cm",
       Log.Light, type = "cairo")

# Light.growth
Light.growth.map <- spplot(plotdata, zcol = "Light.growth.f",
                        col.regions = colfunc(12),
                        lwd = 0.1,
                        colorkey = list(height = 1),
                        par.settings=list(fontsize=list(text=8)))
title.grob <- textGrob(label = "Growth (log ∆) light p.c.", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Light.growth <- arrangeGrob(Light.growth.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Light.growth_2013.png",
       width = 9, height = 9, units = "cm",
       Light.growth, type = "cairo")


## ____ ----

## Maps Prep 1993 ----

setkey(Panel75, unit.name, year)
Maps.1993 <- Panel75[year == 1993, .SD,
                     by = unit.name,
                     .SDcols = c("SPEI4", "SPEI6", "SPEI12", "Light.cap", "Light.cap.adj", "Light.growth",
                                 "Rain", "Temp", "Rain.shock", "Temp.shock")]
Maps.1993[, Log.Light := log(Light.cap.adj)]
Maps.1993[, Light.growth.symlog := if(abs(Light.growth) < 0.0000000001){0}else{sign(Light.growth)*log(1+abs(Light.growth))}]
Maps.1993[, Light.growth.f := cut(Light.growth,
                                  breaks = c(-Inf, seq(-1, 1, 0.2), Inf),
                                  labels = c("<-1", "-1,.8", "-.8,.6", "-.6,.4", "-.4,.2",
                                             "-.2,0", "0,.2", ".2,.4", ".4,.6",
                                             ".6,.8", ".8,1", ">1"))]


plotdata <- ethnicities.in.admin2[, c("unit.name", "NAME_0", "group")]
plotdata <- merge(plotdata, Maps.1993, by = "unit.name")
ProportionNA(data.table(plotdata@data))
plotdata <- plotdata[!is.na(plotdata$SPEI4),]


## Plots ----

# SPEI4
SPEI4.map <- spplot(plotdata, zcol = "SPEI4",
                    col.regions = viridis(20, begin = 0, end = 1,
                                          direction = -1, option = "inferno"),
                    lwd = 0.1)
title.grob <- textGrob(label = "SPEI4", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
SPEI4 <- arrangeGrob(SPEI4.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/SPEI4_1993.png",
       width = 9, height = 9, units = "cm",
       SPEI4, type = "cairo")

# SPEI6
SPEI6.map <- spplot(plotdata, zcol = "SPEI6",
                    col.regions = viridis(20, begin = 0, end = 1,
                                          direction = -1, option = "inferno"),
                    lwd = 0.1)
title.grob <- textGrob(label = "SPEI6", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
SPEI6 <- arrangeGrob(SPEI6.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/SPEI6_1993.png",
       width = 9, height = 9, units = "cm",
       SPEI6, type = "cairo")

# SPEI12
SPEI12.map <- spplot(plotdata, zcol = "SPEI12",
                     col.regions = viridis(20, begin = 0, end = 1,
                                           direction = -1, option = "inferno"),
                     lwd = 0.1)
title.grob <- textGrob(label = "SPEI12", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
SPEI12 <- arrangeGrob(SPEI12.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/SPEI12_1993.png",
       width = 9, height = 9, units = "cm",
       SPEI12, type = "cairo")

# Rain.shock
Rain.shock.map <- spplot(plotdata, zcol = "Rain.shock",
                         col.regions = viridis(20, begin = 0, end = 1,
                                               direction = -1, option = "inferno"),
                         lwd = 0.1)
title.grob <- textGrob(label = "Rainfall shock", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Rain.shock <- arrangeGrob(Rain.shock.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Rain.shock_1993.png",
       width = 9, height = 9, units = "cm",
       Rain.shock, type = "cairo")

# Temp.shock
Temp.shock.map <- spplot(plotdata, zcol = "Temp.shock",
                         col.regions = viridis(20, begin = 0, end = 1,
                                               direction = 1, option = "inferno"),
                         lwd = 0.1)
title.grob <- textGrob(label = "Temperature shock", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Temp.shock <- arrangeGrob(Temp.shock.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Temp.shock_1993.png",
       width = 9, height = 9, units = "cm",
       Temp.shock, type = "cairo")

# Temp
Temp.map <- spplot(plotdata, zcol = "Temp",
                   col.regions = viridis(20, begin = 0, end = 1,
                                         direction = 1, option = "inferno"),
                   lwd = 0.1)
title.grob <- textGrob(label = "Temperature (°C)", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Temp <- arrangeGrob(Temp.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Temp_1993.png",
       width = 9, height = 9, units = "cm",
       Temp, type = "cairo")

# Rain
Rain.map <- spplot(plotdata, zcol = "Rain",
                   col.regions = viridis(20, begin = 0, end = 1,
                                         direction = -1, option = "inferno"),
                   lwd = 0.1)
title.grob <- textGrob(label = "Rainfall (mm/day)", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Rain <- arrangeGrob(Rain.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Rain_1993.png",
       width = 9, height = 9, units = "cm",
       Rain, type = "cairo")


# Log.Light
colfunc <- colorRampPalette(c("black", "white"))
Log.Light.map <- spplot(plotdata, zcol = "Log.Light",
                        col.regions = colfunc(20),
                        lwd = 0.1)
title.grob <- textGrob(label = "Log light p.c.", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Log.Light <- arrangeGrob(Log.Light.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Log.Light_1993.png",
       width = 9, height = 9, units = "cm",
       Log.Light, type = "cairo")


# Light.growth
Light.growth.map <- spplot(plotdata, zcol = "Light.growth.f",
                           col.regions = colfunc(12),
                           lwd = 0.1,
                           colorkey = list(height = 1),
                           par.settings=list(fontsize=list(text=8)))
title.grob <- textGrob(label = "Growth (log ∆) light p.c.", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
                       hjust = 0.5, vjust = 0, 
                       gp = gpar(fontsize = 12, fontface = "bold"))
Light.growth <- arrangeGrob(Light.growth.map, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/Light.growth_1993.png",
       width = 9, height = 9, units = "cm",
       Light.growth, type = "cairo")


## ____ ----

## Ethnic status map ----

setkey(Panel75, unit.name, year)
Panel75[, status6 := droplevels(status6)]
Panel75[, status6 := factor(status6, c("DISCRIMINATED", "POWERLESS",
                                       "JUNIOR PARTNER", "SENIOR PARTNER",
                                       "DOMINANT", "MONOPOLY"))]
Maps.status <- Panel75[, .(status6 = status6[1]),
                     by = unit.name]

plotdata <- ethnicities.in.admin2[, c("unit.name", "NAME_0", "group")]
plotdata <- merge(plotdata, Maps.status, by = "unit.name")
ProportionNA(data.table(plotdata@data))
plotdata <- plotdata[!is.na(plotdata$status6),]


# Ethnic power status

status6.map <- spplot(plotdata, zcol = "status6",
                           col.regions = viridis(6),
                           lwd = 0.1,
                      colorkey = list(height = 0.4),
                      par.settings=list(fontsize=list(text=8)))
# title.grob <- textGrob(label = "Initial ethnic power status", x = unit(4.5, "cm"), y = unit(-0.6, "cm"),
#                        hjust = 0.5, vjust = 0, 
#                        gp = gpar(fontsize = 12, fontface = "bold"))
status6 <- arrangeGrob(status6.map)#, top = title.grob)
ggsave(file = "/data/Data/Output/Maps/status6.png",
       width = 11, height = 9, units = "cm",
       status6, type = "cairo")


