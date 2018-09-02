install.packages("readstata13")
library(readstata13)
install.packages("haven")
library(haven)

# ----

x <- shapefile("/data/Data/HarariLaFerrara/dataset/raster_Africa.shp")
length(unique(x@data$CELLID))
plot(x)

y <- read.dta13("/data/Data/HarariLaFerrara/dataset/geoconflict_main.dta")
names(y)
y <- as.data.table(y)
y[, length(unique(`_ID`))]

plot(x[x@data$CELLID %in% y[, unique(`_ID`)], ]) 
# missing cells correspond to figure in article - missing component is probs agri area

vars <- y[, .(year, lat, lon, `_ID`,
              SPEI4pg, L1_SPEI4pg, L2_SPEI4pg,
              GSmain_ext_SPEI4pg, L1_GSmain_ext_SPEI4pg, L2_GSmain_ext_SPEI4pg)]

vars[, unique(year)]

