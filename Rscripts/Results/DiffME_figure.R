## Difference in marginal effects plot (main regression, combined)

## Parameters ----
# plot.table
# reg2
# dep.var <- "Light.growth"
# ind.var <- "SPEI4"
# yaxis <- c(-0.2, 0.3)
# ref.group <- "JUNIOR PARTNER"

## Setup ----
packages <- c("data.table", "dplyr", "ggplot2", "zoo", "grid", "gridExtra", "lemon")
if(max(!packages %in% installed.packages())>=1)install.packages(packages[!packages %in% installed.packages()])
lapply(packages, require, character.only = TRUE)

## Calculation ----

plot.table[, Ref.ME.tot := ME.tot[Status==ref.group], by = Reg]

plot.table[, Diff.ME.tot := ME.tot - Ref.ME.tot]
plot.table[, SE.Diff.ME.tot := sqrt(Var.IC + Var.IL + 2 * Cov.ICIL)]

plot.table[, max.Diff.ME.tot := Diff.ME.tot + qnorm(0.975) * SE.Diff.ME.tot]
plot.table[, min.Diff.ME.tot := Diff.ME.tot - qnorm(0.975) * SE.Diff.ME.tot]

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
                  axis.title.y = element_text(size = 11, vjust = 1, angle = 0),
                  plot.title = element_text(size = 11, hjust = 0.5),
                  plot.margin = unit(c(0.5, 0.5, 0.3, 0.5), "cm")) 

Status.Labels <- data.frame(Extended = c("DISCRIMINATED", "POWERLESS", "JUNIOR PARTNER",
                              "SENIOR PARTNER", "DOMINANT", "MONOPOLY"),
                            Short = c("DIS", "PWL", "JPR", "SPR", "DOM", "MON"))

assign(paste0(substr(ref.group, 1, 3), ".DiffME.plot"),
       ggplot(plot.table[Reg == 2 & Status != ref.group]) +
  geom_col(aes(x = Status, y = Diff.ME.tot),
             position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x = Status, ymin = min.Diff.ME.tot, ymax = max.Diff.ME.tot),
                position = position_dodge(width=0.3), width = 0.2) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = setdiff(c("DISCRIMINATED", "POWERLESS", "JUNIOR PARTNER",
                                "SENIOR PARTNER", "DOMINANT", "MONOPOLY"), ref.group),
                   labels = setdiff(Status.Labels$Short, Status.Labels$Short[Status.Labels$Extended == ref.group])) +
  ME.theme +
  # coord_cartesian(ylim = yaxis) +
  labs(title = paste0("Relative to ", ref.group),
       x = "Ethnic group status", y = NULL))
