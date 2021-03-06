
## Setup ----
options(scipen=999) # remove scientific notation (0 is default)
setwd("/data/Data")
rm(list = ls())
gc()
`%nin%` <- function(a, b){! a %in% b}
ProportionNA <- function(x){x[, lapply(.SD, function(y){sum(is.na(y))/length(y)})]}
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "grid", "gridExtra",
              "lemon", "SPEI", "RcppRoll",
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

which.pop <- "Pop.Approx"
source("~/Rscripts/Prep/Adm2_75_PanelPrep.R")

## ____ ----


## Rain & Temp regressions ----

Panel <- copy(Panel75)
regA <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
regB <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg1 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag |year.f + unit.name.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg2 <- felm(Light.growth ~ Rain.shock + Rain.shock.lag + Rain.shock:status6 + Rain.shock.lag:status6 + Temp.shock + Temp.shock.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg3 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + unit.name.f|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])
reg4 <- felm(Light.growth ~ Temp.shock + Temp.shock.lag + Temp.shock:status6 + Temp.shock.lag:status6 + Rain.shock + Rain.shock.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel[!is.na(Temp.shock) & !is.na(Rain.shock)])


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/rain_anomaly_status6_ApproxPop.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

stargazer(regA, regB, reg3, reg4,
          type = "html", out = "/data/Data/Output/Adm2_75/temp_anomaly_status6_ApproxPop.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "Rain.shock"
yaxis <- c(-0.2, 0.2)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")

reg1 <- reg3
reg2 <- reg4

dep.var <- "Light.growth"
ind.var <- "Temp.shock"
yaxis <- c(-0.2, 0.3)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")

rm(regA, regB, reg1, reg2, reg3, reg4, Panel)

## SPEI4 regressions ----

Panel <- copy(Panel75)

regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_ApproxPop.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")
source("~/Rscripts/Results/DiffME_figure.R")

rm(regA, regB, reg1, reg2, Panel)

## SPEI6 & SPEI12 regressions ----

Panel <- copy(Panel75)

regA <- felm(Light.growth ~ SPEI6 + SPEI6.lag|year.f + unit.name.f|0|admin2.f,
             Panel)
regB <- felm(Light.growth ~ SPEI6 + SPEI6.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)
reg1 <- felm(Light.growth ~ SPEI6 + SPEI6.lag + SPEI6:status6 + SPEI6.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI6 + SPEI6.lag + SPEI6:status6 + SPEI6.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI6_status6_ApproxPop.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI6"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")


rm(regA, regB, reg1, reg2, Panel)

Panel <- copy(Panel75)

regA <- felm(Light.growth ~ SPEI12 + SPEI12.lag|year.f + unit.name.f|0|admin2.f,
             Panel)
regB <- felm(Light.growth ~ SPEI12 + SPEI12.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)
reg1 <- felm(Light.growth ~ SPEI12 + SPEI12.lag + SPEI12:status6 + SPEI12.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI12 + SPEI12.lag + SPEI12:status6 + SPEI12.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI12_status6_ApproxPop.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI12"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")

## ____ ----

## Other ref groups ----

rm(reg1, reg2, Panel)

Panel <- copy(Panel75)

Panel[, status6 := relevel(status6, "POWERLESS")]

reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "POWERLESS"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")
source("~/Rscripts/Results/DiffME_figure.R")

rm(reg1, reg2, Panel)

Panel <- copy(Panel75)

Panel[, status6 := relevel(status6, "SENIOR PARTNER")]

reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "SENIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")
source("~/Rscripts/Results/DiffME_figure.R")


rm(reg1, reg2, Panel)

Panel <- copy(Panel75)


Panel[, status6 := relevel(status6, "MONOPOLY")]

reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "MONOPOLY"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")
source("~/Rscripts/Results/DiffME_figure.R")


rm(reg1, reg2, Panel)

Panel <- copy(Panel75)


Panel[, status6 := relevel(status6, "DOMINANT")]

reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "DOMINANT"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")
source("~/Rscripts/Results/DiffME_figure.R")

rm(reg1, reg2, Panel)

Panel <- copy(Panel75)


Panel[, status6 := relevel(status6, "DISCRIMINATED")]

reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel)

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "DISCRIMINATED"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [interp. pop.]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")
source("~/Rscripts/Results/DiffME_figure.R")

## Plot DiffME ----

title.grob <- textGrob(
  label = paste0("Differences between total marginal effects of ", ind.var,
                 " on growth (log ∆) in per capita night lights luminosity\nby ethnic group status [interpolated pop.]"),
  x = unit(0.8, "lines"), 
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 12, fontface = "bold"))

notes.grob <- textGrob(
  label = paste0("The figure shows midpoint estimates and 95% C.I.s for the main model, which includes two-way fixed effects (ethnic subdivision and year dummies) and linear time trends at the admin\narea level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry."),
  x = unit(1.2, "lines"), 
  y = unit(0.4, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 8, fontface = "italic"))

DiffME.plot <- arrangeGrob(DIS.DiffME.plot, POW.DiffME.plot, JUN.DiffME.plot,
                           SEN.DiffME.plot, DOM.DiffME.plot, MON.DiffME.plot,
                           top = title.grob,
                           bottom = notes.grob,
                           ncol = 3)
grid.arrange(DiffME.plot)
ggsave(file = paste0("/data/Data/Output/Plots/DiffME_Adm2_75_", ind.var, "_ApproxPop.png"),
       width = 27.5, height = 18, units = "cm",
       DiffME.plot, type = "cairo")

## ____ ----
rm(reg1, reg2, Panel)

which.pop <- "Pop.Approx"
load("/data/Data/Adm2_75/Panel75_Final")
source("~/Rscripts/Prep/Adm2_75_PanelPrep.R")


## Balanced sample ----

Panel75[, length(unique(year)), by = unit.name][, table(V1)]
bal.units <- Panel75[, length(unique(year)), by = unit.name][V1 == 21, unit.name]
Panel.Bal <- Panel75[unit.name %in% bal.units]

regA <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f|0|admin2.f,
             Panel.Bal)
regB <- felm(Light.growth ~ SPEI4 + SPEI4.lag|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.Bal)
reg1 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f|0|admin2.f,
             Panel.Bal)
reg2 <- felm(Light.growth ~ SPEI4 + SPEI4.lag + SPEI4:status6 + SPEI4.lag:status6|year.f + unit.name.f + admin2.f : time.trend|0|admin2.f,
             Panel.Bal)


stargazer(regA, regB, reg1, reg2,
          type = "html", out = "/data/Data/Output/Adm2_75/SPEI4_status6_ApproxPop_Balanced.html",
          add.lines = list(c("Ethnic Subd. FE", rep("Yes", 4)),
                           c("Year FE", rep("Yes", 4)),
                           c("Admin. 2 trends", rep(c("No", "Yes"), 2))),
          notes.align = "r",
          notes.label = "Robust S.E.s are clustered at level-2 administrative areas.")

dep.var <- "Light.growth"
ind.var <- "SPEI4"
yaxis <- c(-0.4, 0.25)
ref.group <- "JUNIOR PARTNER"
path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, "_ApproxPop_Balanced.png")
manual.title <- T
title.label <- paste0("Marginal effect of ", ind.var,
                      " on growth (log ∆) in p.c. night lights luminosity by ethnic group status\n[interpolated population, balanced panel]")
notes.label <- paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")
source("~/Rscripts/Results/MEfigure.R")

