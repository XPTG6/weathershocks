
## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
rm(list = ls())
`%nin%` <- function(a, b){! a %in% b}
ProportionNA <- function(x){x[, lapply(.SD, function(y){sum(is.na(y))/length(y)})]}
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "grid", "gridExtra",
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


## ____ ----

## List of countries and ethnic groups ----

# no. of included countries
Panel75[, length(unique(NAME_0))]

setkey(Panel75, NAME_0, group, unit.name, year)
Panel75[, Changed.status := status!=data.table::shift(status, 1, type = "lag"), by = .(NAME_0, group, gwgroupid, unit.name)]
Panel75[is.na(Changed.status), Changed.status := 0]
Panel75[, Change.N := cumsum(Changed.status), by = .(NAME_0, group, gwgroupid, unit.name)]

Country.Group.List <- Panel75[, .(status = status[1],
                                  status.from = min(status.from),
                                  status.to = max(status.to)),
                              by = .(NAME_0, group, gwgroupid, Change.N, status6)]

save_kable(kable(Country.Group.List[, .(NAME_0, group, status6,
                                        status, status.from, status.to)],
          type = "html",summary = F),
          file = "/data/Data/Output/Descriptives_Adm2_75/Country.Group.List.html")

## Other numbers ----

## subdivisions by ethnic power status
subdivisions <- Panel75[, .(status6 = status6[1]), by = unit.name][, length(unit.name), by = status6]
subdivisions <- merge(subdivisions[, .(subdivisions = round(V1/sum(V1), 3), status6)],
                      Panel75[, .(units = round(length(unit.name)/nrow(Panel75), 3)), by = status6],
                      by = "status6")
subdivisions <- merge(subdivisions,
                      Panel75[, .(actual = round(length(unit.name)/nrow(Panel75), 3)), by = .(status6 = status)],
                      by = "status6")

## ratio of irrigated and rainfed by status
Panel75[, .(agri.irrigated = agri.irrigated[.N],
            agri.rainfed = agri.rainfed[.N]),
        by = .(unit.name, status6)][, sum(agri.irrigated)/(sum(agri.rainfed)+sum(agri.irrigated)), by = status6]
3461/(24750+3461)




## ____ ----

## Distribution of key variables ----

ProportionNA(Panel75)

ggplot(Panel75[!is.na(Light.growth)]) +
  geom_density(aes(x = Light.growth))

Panel75[, quantile(Light.growth, seq(0,1,0.005))] #skewed but symmetric and no obvious cutoff
# Leave as is

Panel75[!is.na(Temp.shock), quantile(Temp.shock, seq(0,1,0.005))]
Panel75[!is.na(Rain.shock), quantile(Rain.shock, seq(0,1,0.005))]
Panel75[!is.na(SPEI4), quantile(SPEI4, seq(0,1,0.01))]
Panel75[!is.na(SPEI6), quantile(SPEI6, seq(0,1,0.01))]
Panel75[!is.na(SPEI12), quantile(SPEI12, seq(0,1,0.01))]

ggplot(Panel75) +
  geom_density(aes(x = SPEI4))
ggplot(Panel75) +
  geom_density(aes(x = SPEI6))
ggplot(Panel75) +
  geom_density(aes(x = SPEI12))


ggplot(Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)]) +
  geom_density(aes(x = Temp.shock))
ggplot(Panel75[!is.na(Temp.shock) & !is.na(Rain.shock)]) +
  geom_density(aes(x = Rain.shock))
        

## Sample summary ----

Panel75[, .(min(year), max(year))]

cols <- c("Temp","Rain","Temp.shock","Rain.shock",
          "SPEI4","SPEI12", "SPEI6",
          "Light.cap", "Light.growth",
          "Conf.events", "Deaths.max", 
          "Conflict", "Any.Deaths",
          "Pop.Approx")

SummaryTable <- function(x){c(signif(mean(x, na.rm = T), 4),
                              signif(sd(x, na.rm = T), 4),
                              signif(min(x, na.rm = T), 4),
                              signif(quantile(x, 0.25, na.rm = T), 4),
                              signif(median(x, na.rm = T), 4),
                              signif(quantile(x, 0.75, na.rm = T), 4),
                              signif(max(x, na.rm = T), 3),
                              N = sum(!is.na(x)))}

Sample.Summary <- Panel75[, lapply(.SD, SummaryTable), .SDcols = cols]
Sample.Summary[, Statistic := c("Mean", "SD", "Min", "25th Pctl", "Median", "75th Pctl", "Max", "N")]
Sample.Summary <- dcast(melt(Sample.Summary, id.vars = "Statistic"), variable ~ Statistic)

FixVars <- c("Pop1990", "areasqkm", "agri.irrigated", "agri.rainfed")
FixVars.Summary <- Panel75[, .SD[.N], by = unit.name, .SDcols = FixVars]
FixVars.Summary <- FixVars.Summary[, lapply(.SD, SummaryTable), .SDcols = FixVars]
FixVars.Summary[, Statistic := c("Mean", "SD", "Min", "25th Pctl", "Median", "75th Pctl", "Max", "N")]
FixVars.Summary <- dcast(melt(FixVars.Summary, id.vars = "Statistic"), variable ~ Statistic)

Sample.Summary <- rbind(Sample.Summary, FixVars.Summary)
setnames(Sample.Summary, "variable", "Statistic")
setcolorder(Sample.Summary, c("Statistic", "Mean", "SD", "Min", "25th Pctl", "Median", "75th Pctl", "Max", "N"))

kable(Sample.Summary, format = "rst")
stargazer(Sample.Summary, type = "html",summary = F,
          out = "/data/Data/Output/Descriptives_Adm2_75/Sample.Summary.html")

## Status summary ----

Status.Summary <- Panel75[, lapply(.SD, SummaryTable), by = status6, .SDcols = cols]
Status.Summary[, Statistic := rep(c("Mean", "SD", "Min", "25th Pctl", "Median", "75th Pctl", "Max", "N"), 6)]

Status.FixVars.Summary <- Panel75[, .SD[.N], by = .(unit.name, status6), .SDcols = FixVars]
Status.FixVars.Summary <- Status.FixVars.Summary[, lapply(.SD, SummaryTable), by = status6, .SDcols = FixVars]
Status.FixVars.Summary[, Statistic := rep(c("Mean", "SD", "Min", "25th Pctl", "Median", "75th Pctl", "Max", "N"), 6)]

Status.Summary <- merge(Status.Summary, Status.FixVars.Summary, by = c("Statistic", "status6"))
setcolorder(Status.Summary, c("status6",
                              "Temp","Rain","Temp.shock","Rain.shock",
                              "SPEI4","SPEI12", "SPEI6",
                              "Light.cap", "Light.growth",
                              "Conf.events", "Deaths.max",
                              "Conflict", "Any.Deaths",
                              "Pop.Approx",
                              "Pop1990", "areasqkm", "agri.irrigated", "agri.rainfed"))
Status.Means <- Status.Summary[, lapply(.SD, function(x){paste0(x[Statistic == "Mean"], " (",
                                                                x[Statistic == "SD"], ")")}),
                               by = status6, .SDcols = c(cols, FixVars)]


stargazer(Status.Means[, c("status6",
                           "Temp","Rain","Temp.shock","Rain.shock",
                           "SPEI4","SPEI12", "SPEI6",
                           "Light.cap", "Light.growth")], type = "html",summary = F,
          out = "/data/Data/Output/Descriptives_Adm2_75/Status.Means.1.html")

stargazer(Status.Means[, c("status6",
                           "Deaths.max","Conf.events",
                           "Pop.Approx",
                           "Pop1990", "areasqkm", "agri.irrigated", "agri.rainfed")],
          type = "html",summary = F,
          out = "/data/Data/Output/Descriptives_Adm2_75/Status.Means.2.html")


## ____ ----

## Non-parametric relationships ----


ME.theme <- theme(strip.background = element_blank(),
                  strip.placement = "outside",
                  strip.text.y = element_text(size = 11, angle = 180),
                  panel.grid.major.y = element_line(colour = "grey", size = 0.2, linetype = "dashed"),
                  panel.grid.minor.y = element_line(colour = "grey", size = 0.1, linetype = "dashed"),
                  plot.background = element_blank(),
                  panel.background = element_blank(),
                  panel.spacing = unit(1, "lines"),
                  panel.border = element_rect(fill = NA),
                  legend.key=element_blank(),
                  legend.position="none",
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 11),
                  plot.title = element_text(size = 12, face = "bold"),
                  plot.margin = unit(c(0.5, 0.5, 0.3, 0.5), "cm")) 




plot.table <- Panel75[, .(unit.name, year,
                          SPEI4, SPEI4.lag, SPEI12, Rain.shock, Temp.shock, Rain, Temp,
                          Light.cap, Light.growth)]

ggplot(plot.table[!is.na(Temp.shock) & !is.na(Rain.shock)]) +
  geom_smooth(aes(y = Light.growth,  x = Temp.shock), colour = "red")
  
ggplot(plot.table[!is.na(Temp.shock) & !is.na(Rain.shock)]) +
  geom_smooth(aes(y = Light.growth,  x = Rain.shock), colour = "blue")

ggplot(plot.table) +
  geom_smooth(aes(y = Light.growth,  x = SPEI4.lag), colour = "yellow") +
  geom_smooth(aes(y = Light.growth,  x = SPEI4), colour = "red")


## ____ ----

## Time trends ----

plot.theme <- theme(strip.background = element_blank(),
                  strip.placement = "outside",
                  strip.text.y = element_text(size = 11, angle = 180),
                  panel.grid.major.y = element_line(colour = "grey", size = 0.2, linetype = "dashed"),
                  panel.grid.minor.y = element_line(colour = "grey", size = 0.1, linetype = "dashed"),
                  plot.background = element_blank(),
                  panel.background = element_blank(),
                  panel.spacing = unit(1, "lines"),
                  legend.title = element_blank(),
                  legend.background = element_blank(),
                  legend.box.background = element_blank(),
                  legend.key = element_blank(),
                  panel.border = element_rect(fill = NA),
                  legend.text = element_text(size = 14),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  plot.title = element_text(size = 12, face = "bold"),
                  plot.margin = unit(c(0.5, 0.5, 0.3, 0.5), "cm")) 

plot.table <- Panel75[, .(SPEI4 = mean(SPEI4),
                          SPEI12 = mean(SPEI12)), by = year]
plot.table <- melt(plot.table, id.vars = "year", id.)

p <- ggplot(plot.table) +
  geom_line(aes(x = year, y = value, colour = variable), size = 1) +
  scale_colour_brewer(type = "qual") +
  plot.theme +
  labs(title = NULL,
       x = "Year", y = NULL)

ggsave(file = "/data/Data/Output/Descriptives_Adm2_75/SPEItrend.png",
       width = 27.5, height = 18, units = "cm",
       p, type = "cairo")



