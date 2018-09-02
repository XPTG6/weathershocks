## Check if we can use SEDAC GRUMP Settlement Points population to control for
## share of urban pop. in each ethnic subdivision.


## Urban population ----
SettlementPoints <- shapefile("/data/Data/GRUMP_Settlement_Points/global_settlement_points_v1.01.shp")
e <- extent(-20, 60, -40, 40)
SettlementPoints <- crop(SettlementPoints, e)
nrow(SettlementPoints@data[is.na(SettlementPoints@data$Urborrur),])
nrow(SettlementPoints@data[SettlementPoints@data$Pop == "-999",])/nrow(SettlementPoints@data)
## 96% of African settlements don't have population...

## We can't :(