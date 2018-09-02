# #ISSUE: topolgy error somewhere in Uganda (self-intersecting polygon or perhaps overlaps)
## diagnostic code
if(sum(!gIsValid(admin_areas_2, byid = T))>0)warning(paste(sum(!gIsValid(admin_areas_2, byid = T)),
                                                           "self-intersecting geometries in admin_areas_2"))
# one self-intersection at or near point 33.489303589999999 0.087360999999999994
if(sum(gOverlaps(admin_areas_2, byid = T))>0)warning(paste(sum(gOverlaps(admin_areas_2, byid = T)),
                                                           "overlapping polygons in admin_areas_2"))
# can't run because of self-intersection

# Looking into self-intersection
plot(admin_areas_2[admin_areas_2$NAME_0 == "Uganda",])
plot(admin_areas_2[admin_areas_2$NAME_0 == "Uganda",], xlim = c( 33.48 , 33.49 ), ylim = c(0.08 , 0.09 ))
points(33.489303589999999, 0.087360999999999994, pch = 3, cex = 3, col = "red")
which(!gIsValid(admin_areas_2, byid = T)) #6028

a <- clgeo_Clean(admin_areas_2, strategy = "BUFFER")
sum(gIsValid(a, byid=TRUE)==FALSE)

# Could also use gBuffer but need to take care of projection.
# admin_areas_2 <- gBuffer(admin_areas_2, byid=TRUE, width=0)
# sum(gIsValid(admin_areas_2, byid=TRUE)==FALSE)



## Modifying polygons in GeoEPR [NOT HELPFUL] ----


if(sum(!gIsValid(GeoEPR, byid = T))>0)warning(paste(sum(!gIsValid(GeoEPR, byid = T)),
                                                    "self-intersecting geometries in GeoEPR"))
if(sum(gOverlaps(GeoEPR, byid = T))>0)warning(paste(sum(gOverlaps(GeoEPR, byid = T)),
                                                    "overlapping polygons in GeoEPR"))
GeoEPR@data[GeoEPR@data$statename == "Uganda", ]
gIsValid(GeoEPR[GeoEPR@data$statename == "Uganda", ], byid = T, reason = T)
gOverlaps(GeoEPR[GeoEPR@data$statename == "Uganda", ], byid = T)

# diagnostic plots
plot(admin_areas_0[admin_areas_0$NAME_0 == "Uganda",])
plot( GeoEPR , xlim = c( 33 , 34 ), ylim = c(-0.2 , 0.2 ), border = "green", col = "yellow", add = T)
points(33.489303589999999, 0.087360999999999994, pch = 3, cex = 3, col = "red")
plot(GeoEPR[rownames(GeoEPR@data) == 678,], border = "yellow", add = T)  
plot(GeoEPR[rownames(GeoEPR@data) == 665,], border = "purple", add = T) 
plot(GeoEPR[rownames(GeoEPR@data) == 677,], border = "blue", add = T) 
plot(GeoEPR[GeoEPR@data$group == "Teso/Basoga",], border = "blue", add = T)

GeoEPR@data[GeoEPR@data$statename == "Uganda",]       

plot(intersect(GeoEPR[rownames(GeoEPR@data) == 665,],
               GeoEPR[rownames(GeoEPR@data) == 678,]))
####

## Check areas of small overlaps:
area(erase(GeoEPR[rownames(GeoEPR@data) == 677,],
           intersect(GeoEPR[rownames(GeoEPR@data) == 677,],
                     GeoEPR[rownames(GeoEPR@data) == 678,])))/ 1000000
area(GeoEPR[rownames(GeoEPR@data) == 677,])/ 1000000

area(erase(GeoEPR[rownames(GeoEPR@data) == 664,],
           intersect(GeoEPR[rownames(GeoEPR@data) == 664,],
                     GeoEPR[rownames(GeoEPR@data) == 677,])))/ 1000000
area(GeoEPR[rownames(GeoEPR@data) == 664,])/ 1000000
####

# Fixing small overlaps
GeoEPR <- rbind(GeoEPR[rownames(GeoEPR@data) != 664,],
                erase(GeoEPR[rownames(GeoEPR@data) == 664,],
                      intersect(GeoEPR[rownames(GeoEPR@data) == 664,],
                                GeoEPR[rownames(GeoEPR@data) == 677,])))
GeoEPR <- rbind(GeoEPR[rownames(GeoEPR@data) != 677,],
                erase(GeoEPR[rownames(GeoEPR@data) == 677,],
                      intersect(GeoEPR[rownames(GeoEPR@data) == 677,],
                                GeoEPR[rownames(GeoEPR@data) == 678,])))

# Fixing large overlap
newpol <- intersect(GeoEPR[rownames(GeoEPR@data) == 665,],
                    GeoEPR[rownames(GeoEPR@data) == 678,])

newpol@data <- newpol@data[, 1:11]
names(newpol@data) <- names(GeoEPR@data)
newpol@data$sqkm <- area(newpol) / 1000000 #Warning: full correction requires subtracting area from previous polygons
newpol@data$group <- "Teso/Basoga"
newpol@data$groupid <- 4001
newpol@data$gwgroupid <- 50004001

GeoEPR <- erase(GeoEPR,
                newpol)
GeoEPR <- rbind(GeoEPR,
                newpol)

# Checks
# plot(GeoEPR[GeoEPR@data$group == "Teso",], border = "red")
# plot(aa[aa@data$group == "Teso",], add = T)
# plot(erase(GeoEPR[GeoEPR@data$group == "Teso",],
#            newpol),
#      border = "green",
#      add = T)
