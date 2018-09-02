## This scripts derives some key variables for regression and descriptives
## It also restricts the panel to appropriate period / countries and applies any necessary exclusions
## Use: it is sourced in all regression scripts after loading Panel75
## Note it should also work for Adm1 (Change NAME_2 to 1), need to adapt for Main (interpolated ERA data) by changing Panel75 to Panel and removing references to SPEI

## Parameters ----

## Panel 75
## which.pop <- "Pop1990" or "Pop.Approx"

## Basics ----

setkey(Panel75, NAME_0, NAME_2, unit.name, year)

## Fix unit.name of units exchanging country
x <- Panel75[, length(unique(NAME_0))>1, by = unit.name][V1 == T, unit.name]
# View(Panel75[unit.name %in% x])
Panel75[unit.name %in% x, unit.name := paste0(unit.name, "_", NAME_0)]

Panel75[, Light.cap := Light/eval(parse(text = which.pop))]

Panel75[, Conflict := Conf.events>0]
Panel75[, Any.Deaths := Deaths.max>0]

Panel75[, unit.name.f := as.factor(unit.name)]
Panel75[, year.f := as.factor(year)]
Panel75[, country.f := as.factor(NAME_0)]
Panel75[, admin2.f := as.factor(NAME_2)]
Panel75[, admin1.f := as.factor(NAME_1)]

Panel75[, year := as.double(year)]

Panel75[year >1992, time.trend := as.double(seq_len(length(year))), by = unit.name]

## Status ----

# Pre-treatment status:
Panel75[status %nin% c("STATE COLLAPSE", "IRRELEVANT", "SELF-EXCLUSION"), status6 := status[1], by = unit.name]
Panel75[, status6 := na.locf(status6, na.rm = F), by = unit.name]
Panel75[status6 %in% c("JUNIOR PARTNER", "SENIOR PARTNER"),
        `:=` (status2 = "included",
              status3 = "shared")]
Panel75[status6 %in% c("DOMINANT", "MONOPOLY"),
        `:=` (status2 = "included",
              status3 = "dominant")]
Panel75[status6 %in% c("POWERLESS", "DISCRIMINATED"),
        `:=` (status2 = "excluded",
              status3 = "excluded")]
# Irrelevant, state collapse, and self-exclusion (none in sample) as NA

Panel75[, status6 := relevel(status6, "JUNIOR PARTNER")]

## Light growth ----

# Light growth:
light.shift <- Panel75[Light.cap>0, min(Light.cap)]
Panel75[, Light.cap.adj := Light.cap + light.shift]
Panel75[, Light.growth := log(Light.cap.adj) - log(data.table::shift(Light.cap.adj,
                                                                     type = "lag")),
        by = unit.name]

# View(Panel75[year>1992 & is.na(Light.growth)]) # first year of new units (etnhnoadmins)
# Panel75[year>1992 & !is.na(Light.growth), quantile(Light.growth, seq(0, 1, 0.05))]

## Rain & Temp ----

## Weather anomaly measures
Panel75[, Rain.shock := (Rain-Rain.LRmean)/Rain.LRsd]
Panel75[, Temp.shock := (Temp-Temp.LRmean)/Temp.LRsd]


## DEPRECATED b/c distribution looks OK after dealing with LR values properly.
# # Set outliers in Temp.shock and Rain.shock to NA
# Panel75[!is.na(Temp.shock), `:=` (Temp.shock.Q995 = quantile(Temp.shock, .995),
#                                   Temp.shock.Q005 = quantile(Temp.shock, .005))]
# Panel75[!is.na(Rain.shock), `:=` (Rain.shock.Q995 = quantile(Rain.shock, .995),
#                                   Rain.shock.Q005 = quantile(Rain.shock, .005))]
# 
# Panel75[Temp.shock < Temp.shock.Q005 | Temp.shock > Temp.shock.Q995,
#         `:=` (Temp.shock = NA,
#               Rain.shock = NA)]
# Panel75[Rain.shock < Rain.shock.Q005 | Rain.shock > Rain.shock.Q995,
#         `:=` (Temp.shock = NA,
#               Rain.shock = NA)]
####


## Lags
Panel75[, Temp.shock.lag := data.table::shift(Temp.shock, 1, type = "lag"), by = unit.name]
Panel75[, Rain.shock.lag := data.table::shift(Rain.shock, 1, type = "lag"), by = unit.name]
Panel75[, SPEI4.lag := data.table::shift(SPEI4, 1, type = "lag"), by = unit.name]
Panel75[, SPEI6.lag := data.table::shift(SPEI6, 1, type = "lag"), by = unit.name]
Panel75[, SPEI12.lag := data.table::shift(SPEI12, 1, type = "lag"), by = unit.name]

## Leads
Panel75[, Temp.shock.lead := data.table::shift(Temp.shock, 1, type = "lead"), by = unit.name]
Panel75[, Rain.shock.lead := data.table::shift(Rain.shock, 1, type = "lead"), by = unit.name]
Panel75[, SPEI4.lead := data.table::shift(SPEI4, 1, type = "lead"), by = unit.name]
Panel75[, SPEI6.lead := data.table::shift(SPEI6, 1, type = "lead"), by = unit.name]
Panel75[, SPEI12.lead := data.table::shift(SPEI12, 1, type = "lead"), by = unit.name]


## Pre-treatment variables ----

# Pre-treatment conflict
Panel75[year>=1989 & year <= 1992, Pre.Conflict := sum(Conf.events), by = unit.name]
Panel75[, Pre.Conflict := na.locf(Pre.Conflict, na.rm = F), by = unit.name]
Panel75[!is.na(Pre.Conflict) & Pre.Conflict>0, quantile(Pre.Conflict, seq(0,1,0.1))]
Panel75[, Pre.Conflict := cut(Pre.Conflict,
                              c(0, 0.01, 1, 5, Inf),
                              c("0", "1", "2-5", ">5"),
                              include.lowest = T)]


Panel75[year>=1989 & year <= 1992, Pre.Deaths := sum(Deaths.max), by = unit.name]
Panel75[, Pre.Deaths := na.locf(Pre.Deaths, na.rm = F), by = unit.name]
Panel75[!is.na(Pre.Deaths) & Pre.Deaths>0, quantile(Pre.Deaths, seq(0,1,0.1))]
Panel75[, Pre.Deaths := cut(Pre.Deaths,
                              c(0, 0.01, 5, 25, 100, Inf),
                              c("0", "≤5", "6-25", "26-100", ">100"),
                              include.lowest = T)]

# Pre-treatment institutions:

Panel75[checks == -999, checks := NA]
Panel75[!is.na(checks) & year %in% 1991:1992, Pre.Checks := mean(checks), by = unit.name]
Panel75[, Pre.Checks := na.locf(Pre.Checks, na.rm = F), by = unit.name]


# Pre-treatment agriculture:

Panel75[, Pre.agri.irrigated.cap := agri.irrigated[.N]/Pop1990[.N], by = unit.name]
Panel75[, Pre.agri.rainfed.cap := agri.rainfed[.N]/Pop1990[.N], by = unit.name]

Panel75[, Pre.agri.irrigated.cap[.N], by = unit.name][, quantile(V1, seq(0,1,0.1))]
Panel75[, Pre.agri.rainfed.cap[.N], by = unit.name][, quantile(V1, seq(0,1,0.1))]

Panel75[, Pre.agri.irrigated.cap := Pre.agri.irrigated.cap>0]

rainfed.breaks <- Panel75[, quantile(Pre.agri.rainfed.cap, seq(0,1,0.25))]
Panel75[, Pre.agri.rainfed.cap := cut(Pre.agri.rainfed.cap,
                                      c(rainfed.breaks),
                                      c("1st Qrt", "2nd Qrt", "3rd Qrt", "4th Qrt"),
                                      include.lowest = T)]

# Pre-treatment GDP (lights):
Panel75 <- merge(Panel75,
                 Panel75[year == 1992,
                         .(Pre.Lights.Adm2 = sum(Light, na.rm = T) / sum(Pop1990, na.rm = T)),
                         by = NAME_2],
                 by = "NAME_2",
                 all.x = T)

Panel75 <- merge(Panel75,
                 Panel75[year == 1992,
                         .(Pre.Lights.Country = sum(Light, na.rm = T) / sum(Pop1990, na.rm = T)),
                         by = NAME_0],
                 by = "NAME_0",
                 all.x = T)


# Pre-treatment group size (using Pop1990):

Panel75[year == 1992, Country.Pop.1990 := sum(Pop1990), by = NAME_0]
Panel75 <- merge(Panel75,
                 Panel75[year == 1992,
                         .(Pre.Group.Size = sum(Pop1990)/Country.Pop.1990[.N]),
                         by = .(gwgroupid)],
                 by = c("gwgroupid"),
                 all.x = T)
Panel75[, Country.Pop.1990 := NULL]
Panel75[year == 1990, Pre.Group.Size[.N], by = .(group, NAME_0)][, sum(V1), by = NAME_0]

## Exclusions ----

## Exclude Northern Africa:
Panel75 <- Panel75[!NAME_0 %in% c("Morocco", "Tunisia", "Egypt", "Algeria", "Libya")]

# Exclude pre-1992:
Panel75 <- Panel75[year > 1992 & !is.na(Light.growth)]

# Exclude NAs in status6 (irrelevant, state collapse):
Panel75 <- Panel75[!is.na(status6)]



