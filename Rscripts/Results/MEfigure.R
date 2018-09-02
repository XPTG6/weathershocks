## Marginal effects plot (cont., lag, and combined)

## Parameters ----
# reg1, reg2 (can add more to select regressions)
# dep.var <- "Light.growth"
# ind.var <- "SPEI4"
# yaxis <- c(-0.2, 0.3)
# ref.group <- "JUNIOR PARTNER"
# path.name <- paste0("/data/Data/Output/Plots/Adm2_75_", ind.var, ".png")
# manual.title <- T or F
# title.label (optional)
# notes.label (optional)

## Setup ----
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "grid", "gridExtra", "lemon")
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)

## Select regressions
reg.list <- list(reg1, reg2)

## Identify irrelevant controls ----

### Ignores any other controls that are not interacted with status
irr.controls <- unique(unlist(lapply(reg.list,
                       function(x){setdiff(unique(rownames(x$coefficients)),
                                           c(grep("status", rownames(x$coefficients), value = T),
                                             c(ind.var, paste0(ind.var, ".lag"))))})))
## Note: does not ignore direct status terms (it should, if there are some)
## Note: does not ignore any other controls interacted with status (it should, if interaction does not include independent variable)

## Direct terms ----

dir.coeff <- cbind(Reg = c(1,2),
                   rbindlist(lapply(reg.list,
                                    function(x){
                                      data.table(t(x$coefficients[c(ind.var, paste0(ind.var, ".lag")), dep.var]))})))

dir.var <- rbindlist(lapply(reg.list,
                                  function(x){
                                    data.table(t(diag(x$clustervcv)[c(ind.var, paste0(ind.var, ".lag"))]))}))
setnames(dir.var, names(dir.var), paste0("Var.", names(dir.var)))

dir.cov <- rbindlist(lapply(reg.list,
                                  function(x){
                                    data.table(Cov.DCDL = x$clustervcv[ind.var, paste0(ind.var, ".lag")])}))

plot.table <- cbind(dir.coeff, dir.var)
plot.table <- cbind(plot.table, dir.cov)


## Inter coeff.s  ----

IC.coeff <- melt(cbind(Reg = c(1,2), # Interaction, contemporaneous
                   rbindlist(lapply(reg.list,
                                    function(x){
                                      data.table(t(x$coefficients[grep("lag",
                                                                       setdiff(rownames(x$coefficients),
                                                                          c(ind.var, paste0(ind.var, ".lag"),
                                                                            irr.controls)),
                                                                       value = T,
                                                                       invert = T), # invert to exclude lag
                                                                  dep.var]))}))),
                  id.vars = "Reg",
                  variable.name = "Status",
                  value.name = "IC")
IC.coeff[, Status := substr(as.character(Status), nchar(paste0(ind.var, ":status6"))+1, nchar(as.character(Status)))]

IL.coeff <- melt(cbind(Reg = c(1,2), # Interaction, lag
                       rbindlist(lapply(reg.list,
                                        function(x){
                                          data.table(t(x$coefficients[grep("lag",
                                                                           setdiff(rownames(x$coefficients),
                                                                                   c(ind.var, paste0(ind.var, ".lag"),
                                                                                     irr.controls)),
                                                                           value = T,
                                                                           invert = F), # don't invert to capture lag
                                                                      dep.var]))}))),
                 id.vars = "Reg",
                 variable.name = "Status",
                 value.name = "IL")
IL.coeff[, Status := substr(as.character(Status), nchar(paste0(ind.var, ".lag:status6"))+1, nchar(as.character(Status)))]

I.coeff <- merge(IC.coeff, IL.coeff, by = c("Reg", "Status"))

plot.table <- merge(plot.table, I.coeff, by = "Reg")

## Inter var.s ----
IC.var <- melt(cbind(Reg = c(1,2), # Interaction, contemporaneous
                       rbindlist(lapply(reg.list,
                                        function(x){
                                          data.table(t(diag(x$clustervcv)[grep("lag",
                                                                           setdiff(rownames(x$clustervcv),
                                                                                   c(ind.var, paste0(ind.var, ".lag"),
                                                                                     irr.controls)),
                                                                           value = T,
                                                                           invert = T)]))}))),
                 id.vars = "Reg",
                 variable.name = "Status",
                 value.name = "Var.IC")
IC.var[, Status := substr(as.character(Status), nchar(paste0(ind.var, ":status6"))+1, nchar(as.character(Status)))]


IL.var <- melt(cbind(Reg = c(1,2), # Interaction, lag
                     rbindlist(lapply(reg.list,
                                      function(x){
                                        data.table(t(diag(x$clustervcv)[grep("lag",
                                                                             setdiff(rownames(x$clustervcv),
                                                                                     c(ind.var, paste0(ind.var, ".lag"),
                                                                                       irr.controls)),
                                                                             value = T,
                                                                             invert = F)]))}))),
               id.vars = "Reg",
               variable.name = "Status",
               value.name = "Var.IL")
IL.var[, Status := substr(as.character(Status), nchar(paste0(ind.var, ".lag:status6"))+1, nchar(as.character(Status)))]

I.var <- merge(IC.var, IL.var, by = c("Reg", "Status"))

plot.table <- merge(plot.table, I.var, by = c("Reg", "Status"))

## DI cov.s ----

DI.cov <- cbind(Reg = c(rep(1, 5), rep(2, 5)), # Interaction, contemporaneous
                     rbindlist(lapply(reg.list,
                                      function(x){
                                        data.table(Status = substr(grep("lag", # note levels always in same order, so one columns enough
                                                                 setdiff(colnames(x$clustervcv),
                                                                         c(ind.var, paste0(ind.var, ".lag"),
                                                                           irr.controls)),
                                                                 value = T,
                                                                 invert = T),
                                                                 nchar(paste0(ind.var, ":status6"))+1,
                                                                 nchar(as.character(grep("lag", # note levels always in same order, so one columns enough
                                                                                         setdiff(colnames(x$clustervcv),
                                                                                                 c(ind.var, paste0(ind.var, ".lag"),
                                                                                                   irr.controls)),
                                                                                         value = T,
                                                                                         invert = T)))),
                                                   Cov.DCIC = x$clustervcv[ind.var,
                                                                            grep("lag",
                                                                             setdiff(colnames(x$clustervcv),
                                                                                     c(ind.var, paste0(ind.var, ".lag"),
                                                                                       irr.controls)),
                                                                             value = T,
                                                                             invert = T)],
                                                   Cov.DCIL = x$clustervcv[ind.var,
                                                                           grep("lag",
                                                                                setdiff(colnames(x$clustervcv),
                                                                                        c(ind.var, paste0(ind.var, ".lag"),
                                                                                          irr.controls)),
                                                                                value = T,
                                                                                invert = F)],
                                                   Cov.DLIC = x$clustervcv[paste0(ind.var, ".lag"),
                                                                           grep("lag",
                                                                                setdiff(colnames(x$clustervcv),
                                                                                        c(ind.var, paste0(ind.var, ".lag"),
                                                                                          irr.controls)),
                                                                                value = T,
                                                                                invert = T)],
                                                   Cov.DLIL = x$clustervcv[paste0(ind.var, ".lag"),
                                                                           grep("lag",
                                                                                setdiff(colnames(x$clustervcv),
                                                                                        c(ind.var, paste0(ind.var, ".lag"),
                                                                                          irr.controls)),
                                                                                value = T,
                                                                                invert = F)])})))
plot.table <- merge(plot.table, DI.cov, by = c("Reg", "Status"))

## II Cov.s ----

II.cov <- cbind(Reg = c(rep(1, 5), rep(2, 5)), # Interaction, contemporaneous
                rbindlist(lapply(reg.list,
                                 function(x){
                                   data.table(Status = substr(grep("lag", # note levels always in same order, so one columns enough
                                                                   setdiff(colnames(x$clustervcv),
                                                                           c(ind.var, paste0(ind.var, ".lag"),
                                                                             irr.controls)),
                                                                   value = T,
                                                                   invert = T),
                                                              nchar(paste0(ind.var, ":status6"))+1,
                                                              nchar(as.character(grep("lag", # note levels always in same order, so one columns enough
                                                                                      setdiff(colnames(x$clustervcv),
                                                                                              c(ind.var, paste0(ind.var, ".lag"),
                                                                                                irr.controls)),
                                                                                      value = T,
                                                                                      invert = T)))),
                                              Cov.ICIL = diag(x$clustervcv[grep("lag",
                                                                                   setdiff(colnames(x$clustervcv),
                                                                                           c(ind.var, paste0(ind.var, ".lag"),
                                                                                             irr.controls)),
                                                                                   value = T,
                                                                                   invert = T),
                                                                              grep("lag",
                                                                                   setdiff(colnames(x$clustervcv),
                                                                                           c(ind.var, paste0(ind.var, ".lag"),
                                                                                             irr.controls)),
                                                                                   value = T,
                                                                                   invert = F)]))})))
plot.table <- merge(plot.table, II.cov, by = c("Reg", "Status"))

setnames(plot.table,
         names(plot.table),
         gsub(ind.var, "DC",
              gsub(paste0(ind.var, ".lag"), "DL",
                   names(plot.table))))

## Marginal effects ----

plot.table[, `:=` (ME.cont = DC + IC,
                   ME.lag = DL + IL,
                   ME.tot = DC + IC + DL + IL)]

## S.E. of ME ----

plot.table[, `:=` (SE.ME.cont = sqrt(Var.DC + Var.IC + 2*Cov.DCIC),
                   SE.ME.lag = sqrt(Var.DL + Var.IL + 2*Cov.DLIL),
                   SE.ME.tot = sqrt(Var.DC + Var.IC + Var.DL + Var.IL +
                     2*(Cov.DCDL + Cov.DCIC + Cov.DCIL + Cov.DLIC + Cov.DLIL + Cov.ICIL)))]

## Add reference groups ----

plot.table <- rbind(plot.table,
                    data.table(Reg = c(1,2), Status = rep(ref.group, 2)),
                    fill = T)
cols <- c("DC", "DL", "Var.DC", "Var.DL", "Cov.DCDL")
plot.table[, (cols) := lapply(.SD, function(x){na.locf(x, na.rm = F)}), by = Reg,  .SDcols = cols]

plot.table[Status == ref.group,
           `:=` (ME.cont = DC,
                 ME.lag = DL,
                 ME.tot = DC + DL,
                 SE.ME.cont = sqrt(Var.DC),
                 SE.ME.lag = sqrt(Var.DL),
                 SE.ME.tot = sqrt(Var.DC + Var.DL + 2 * Cov.DCDL))]

## 95% Confidence Interval ----
cols <- c("ME.cont", "ME.lag", "ME.tot")
plot.table[, `:=` (max.cont = ME.cont + qnorm(0.975) * SE.ME.cont,
                   min.cont = ME.cont - qnorm(0.975) * SE.ME.cont,
                   max.lag = ME.lag + qnorm(0.975) * SE.ME.lag,
                   min.lag = ME.lag - qnorm(0.975) * SE.ME.lag,
                   max.tot = ME.tot + qnorm(0.975) * SE.ME.tot,
                   min.tot = ME.tot - qnorm(0.975) * SE.ME.tot)]

## Melt table ----

ME.plot <- plot.table[, .(Reg, Status ,ME.cont, ME.lag, ME.tot)]
ME.plot <- melt(ME.plot, id.vars = c("Reg", "Status"), variable.name = "Lag", value.name = "ME")
ME.plot[, Lag := substr(Lag, 4, 7)]
max.plot <- plot.table[, .(Reg, Status, max.cont, max.lag, max.tot)]
max.plot <- melt(max.plot, id.vars = c("Reg", "Status"), variable.name = "Lag", value.name = "CE.max")
max.plot[, Lag := substr(Lag, 5, 8)]
min.plot <- plot.table[, .(Reg, Status, min.cont, min.lag, min.tot)]
min.plot <- melt(min.plot, id.vars = c("Reg", "Status"), variable.name = "Lag", value.name = "CE.min")
min.plot[, Lag := substr(Lag, 5, 8)]
plot <- merge(ME.plot, max.plot, by = c("Reg", "Status", "Lag"))
plot <- merge(plot, min.plot, by = c("Reg", "Status", "Lag"))
plot[, `:=` (Reg = as.factor(Reg),
             Status = as.factor(Status),
             Lag = as.factor(Lag))]
## Plot ----

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

p <- ggplot(plot) +
  facet_rep_grid(Lag~.,
                 switch = "y", 
                 labeller = as_labeller(c(cont = "Cont.", lag = "Lag 1y", tot = "Total")),
                 repeat.tick.labels = T)  +
  geom_point(aes(x = Status, y = ME, group = Reg, alpha = Reg),
                  position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x = Status, ymin = CE.min, ymax = CE.max, group = Reg, alpha = Reg),
                position = position_dodge(width=0.3), width = 0.2) +
  geom_hline(yintercept = 0) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_x_discrete(limits = c("DISCRIMINATED", "POWERLESS", "JUNIOR PARTNER",
                              "SENIOR PARTNER", "DOMINANT", "MONOPOLY")) +
  ME.theme +
  coord_cartesian(ylim = yaxis) +
  labs(title = NULL,
       x = "Ethnic group status", y = NULL)

title.grob <- textGrob(
  label = if(manual.title == T){title.label}else{paste0("Marginal effect of ", ind.var,
                 " on growth (log ∆) in p.c. night lights luminosity by ethnic group status [1990 pop.]")},
  x = unit(0.8, "lines"), 
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 12, fontface = "bold"))

notes.grob <- textGrob(
  label = if(manual.title == T){notes.label}else{paste0("The figure shows midpoint estimates and 95% C.I.s for two distinct models: the left-side estimates are calculated using two-way fixed effects (ethnic subdivision and year dummies),\nthe right-side ones also include linear time trends at the admin area level. Robust S.E.s are clustered at the ethnic subdivision level.\nLog difference approximates percentage change for small numbers, e.g. 0.1 ≈ 10%. However, it is preferable to percentage change because it preserves symmetry.")},
  x = unit(1.2, "lines"), 
  y = unit(0.4, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 8, fontface = "italic"))

p1 <- arrangeGrob(p, top = title.grob, bottom = notes.grob)
grid.arrange(p1)

ggsave(file = path.name,
       width = 27.5, height = 18, units = "cm",
       p1, type = "cairo")

print(paste0("Saved plot in: ", path.name))