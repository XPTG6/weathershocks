## Preparation ----
# Load
ethnicities.in.admin2 <- shapefile("/data/Data/Shapefiles/ethnadmin2_wraster")
load("/data/Data/Robjects/admin_areas_2")
proj4string(ethnicities.in.admin2) <- admin_areas_2@proj4string 

## ____ ----

## Prepare data ----

# exclude territories ending pre-1994 (at least two years of data)
ethnicities.in.admin2 <- ethnicities.in.admin2[ethnicities.in.admin2@data$to >= 1994, ]

ethnicities.in.admin2@data$RowIndex <- seq.int(nrow(ethnicities.in.admin2@data))
regression.table <- ethnicities.in.admin2@data
regression.table <- data.table(regression.table)
regression.table[statename == "Cote d'Ivoire", statename := "Côte d'Ivoire"]
regression.table[statename == "Congo", statename := "Republic of Congo"]
regression.table[statename == "Congo, DRC", statename := "Democratic Republic of the Congo"]
regression.table[statename == "The Gambia", statename := "Gambia"]
regression.table[statename == "Libya (Tripolitania, Cyrenaica, Fezzan)", statename := "Libya"]
regression.table[NAME_0 == "Western Sahara", NAME_0 := "Morocco"]
regression.table[statename == "Sudan" & NAME_0 == "South Sudan", NAME_0 := "Sudan"]
## Sudan: because admin dataset is as of today - these are ethnic territories pre-2011 (+ border incongruencies)


## Diagnostic ----


# View(regression.table[NAME_0 != statename, .(NAME_0, statename)])
# x <- regression.table[NAME_0 != statename,]
# setkey(x, areasqkm)
# setkey(x, Pop2015)
# View(x[, .(NAME_0, statename, group, from, to, Pop2015, areasqkm)])
y <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$group == "Kunama",
                                              c("NAME_0", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
y <- y[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, from, to, group)]

z <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$group == "Hutu" &
                                                ethnicities.in.admin2@data$statename == "Rwanda",
                                              c("NAME_0", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
z <- z[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, from, to, group)]

q <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$group == "Southerners (Lomwe, Mang'anja, Nyanja, Yao)",
                                              c("NAME_0", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
q <- q[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, from, to, group)]

p <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$group == "Other Southern groups",
                                              c("NAME_0", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
p <- p[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, from, to, group)]

r <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$group == "Arabs" &
                                                ethnicities.in.admin2@data$statename == "Algeria",
                                              c("NAME_0", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
r <- r[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, from, to, group)]


eritrea <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$statename == "Eritrea",
                                              c("NAME_0", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
eritrea <- eritrea[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, from, to, group)]

## Border changes since 1990 (from wiki) ----

# 1990 — Namibia gets independent from occupying South Africa.
# 1991 May 18 — Somaliland declares independence from Somalia but is not recognized by any other country.
# 1993 May 24 — Eritrea breaks off from Ethiopia.
 # ---> but note Ethiopian-Eritrean War https://en.wikipedia.org/wiki/Eritrean%E2%80%93Ethiopian_War
 #      has led to further changes & temporary occupation
# 1994 February 28 — Walvis Bay formally[weasel words] transferred by South Africa to Namibia.
# 2008 August 14 — Bakassi transferred to Cameroon by Nigeria
# 2011 July 9 — South Sudan formally obtains independence from the Republic of Sudan.
 
## Bakassi (Nigeria to Cameroon in 2008) ----

Bakassi <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$statename %in% c("Nigeria", "Cameroon") &
                                                      ethnicities.in.admin2@data$NAME_0 %in% c("Nigeria", "Cameroon") &
                                                      ethnicities.in.admin2@data$NAME_0  != ethnicities.in.admin2@data$statename,
                                                    c("NAME_0", "NAME_1", "NAME_2", "statename", "areasqkm", "Pop2015", "from", "to", "group")])
Bakassi <- Bakassi[, .(areasqkm = sum(areasqkm), Pop2015 = sum(Pop2015)), by = .(NAME_0, statename, NAME_1, NAME_2, from, to, group)]

# Only 2 sqkm misaligned...

Bakassi <- as.data.table(ethnicities.in.admin2@data[ethnicities.in.admin2@data$NAME_2 == "Bakassi",
                                                    c("NAME_0", "NAME_1", "NAME_2", "statename", "areasqkm", "Pop2015", "from", "to", "group")])

na.omit(admin_areas_2@data[admin_areas_2@data$NAME_2 == "Bakassi", c("NAME_0", "NAME_1", "NAME_2")])

## Plots ----

# diagnostic plot, small areas
plot(ethnicities.in.admin2[ethnicities.in.admin2@data$RowIndex == 7982, ])
map(admin_areas_0,add=T,fill=F, col="red", lwd = 0.2)
# clearly just approximation error in drawing boundaries

# unclear about larger areas... checked google for border changes but doesn't seem to fit time period
map(admin_areas_0[admin_areas_0@data$NAME_0 %in% c("Morocco", "Algeria"),],fill=F,col="red", lwd = 0.2)
plot(ethnicities.in.admin2[ethnicities.in.admin2@data$RowIndex %in% c(4152, 4182), ], add=T)
