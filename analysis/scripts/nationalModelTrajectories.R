# Example trajectories from national model
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

dia_shp <- 23

err_col <- "grey50"

baseDir <- "."
#numbers from Johnson et al for validation
johnsonCompare <- read.csv(paste0(baseDir,"/data/Johnson et al. figures5_6.csv"))

# get outputs from demographics
covTableSim <- data.frame(Anthro = seq(0, 10, 20, 30, 40, 50, 
                                     60, 70, 80, 90), 
                          Fire = 0,
                          fire_excl_anthro = 0)
covTableSim$polygon <- paste0("Missisa_", covTableSim$Anthro + 1)
covTableSim$area <- "FarNorth"
covTableSim$Total_dist <- covTableSim$Anthro + covTableSim$fire_excl_anthro

popGrowthPars <- demographicCoefficients(500,
  modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

popGrowthParsSmall <- demographicCoefficients(35,
  modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

rateSamples <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthParsSmall,
  ignorePrecision = F, returnSample = T, useQuantiles = T
)

rateSamplesLarge <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = T, useQuantiles = T
)

rateSummaries <- demographicRates(
  covTable = covTableSim, popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = F, useQuantiles = F
)

rateSamples$S_PIlow <- 1
rateSamples$S_PIhigh <- 1
rateSamples$rep <- as.factor(rateSamples$replicate)
levels(rateSamples$rep) <- sample(unique(rateSamples$replicate), replace = F)
rateSamples$rep <- as.character(rateSamples$rep)
rateSamplesLarge$S_PIlow <- 1
rateSamplesLarge$S_PIhigh <- 1

rateSamples$R_PIlow <- 1
rateSamples$R_PIhigh <- 1
rateSamples$fullGrp <- paste(rateSamples$rep) # ,rateSamples$Fire)
rateSamplesLarge$R_PIlow <- 1
rateSamplesLarge$R_PIhigh <- 1
rateSamplesLarge$fullGrp <- paste(rateSamplesLarge$rep) # ,rateSamplesLarge$Fire)

str(johnsonCompare)
johnsonCompare$Anthro=johnsonCompare$anthro
johnsonCompare$S_bar=johnsonCompare$Sresp
johnsonCompare$S_PIlow = johnsonCompare$Slow_95_pred
johnsonCompare$S_PIhigh = johnsonCompare$Shigh_95_pred
johnsonCompare$R_bar=johnsonCompare$Rresp/100
johnsonCompare$R_PIlow = johnsonCompare$Rlow_95_pre/100
johnsonCompare$R_PIhigh = johnsonCompare$Rhigh_95_pred/100

pal = colorRampPalette(brewer.pal(7, "Blues"))(35)

base1 <- ggplot(data = rateSummaries, 
                aes(x = Anthro, y = S_bar, ymin = S_PIlow, ymax = S_PIhigh)) +
  geom_ribbon(fill="#67A9CF",colour=NA,data=johnsonCompare,alpha=0.25)+
  geom_line(data = subset(rateSamples), size = 0.5, alpha = 1, 
            aes(x = Anthro, y = S_bar, group = rep, colour = rep)) +
  scale_color_manual(values=pal)+
  geom_line(colour = "#2166AC", size = 2, linetype = "dotted") +
  xlab("Anthropogenic Disturbance (%)") +
  ylab("Adult Female Survival") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0.65, 1)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))


plot_recruitment3 <- ggplot(data = rateSummaries,
                            aes(x = Anthro, y = R_bar * 100, 
                                ymin = R_PIlow * 100, ymax = R_PIhigh * 100)) +
  geom_ribbon(fill="#67A9CF",colour=NA,data=johnsonCompare,alpha=0.25)+
  geom_line(data = rateSamples, size = 0.5, 
            aes(x = Anthro, y = R_bar * 100, group = fullGrp, color = fullGrp),
            alpha = 1) +scale_color_manual(values=pal)+
  geom_line(colour = "#2166AC", size = 2, linetype = "dotted") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  xlab("Anthropogenic disturbance (%)") +
  ylab("Recruitment (calves/100 cows)") +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

# demography
pars <- data.frame(N0 = 1000)
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = 1))
pars <- merge(pars, rateSamplesLarge)
numSteps <- 20
pars1 <- cbind(pars, caribouPopGrowth(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "binomial"
))

pars <- data.frame(N0 = 1000)
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = 1)) 
pars <- merge(pars, rateSamples)
numSteps <- 20
pars2 <- cbind(pars, caribouPopGrowth(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "binomial"
))

oo <- pars2 %>%
  select(Anthro, lambda, fullGrp, rrp) %>%
  group_by(fullGrp, Anthro) %>%
  summarise(lambda = median(lambda))

ooT <- pars1 %>%
  select(Anthro, lambda, fullGrp, rrp) %>%
  group_by(fullGrp, Anthro) %>%
  summarise(lambda = median(lambda))

oo$lambdaH <- oo$lambda
oo$lambdaL <- oo$lambda

plot_lambda <- ggplot(oo, 
                      aes(x = Anthro, y = lambda, ymin = lambdaH, ymax = lambdaL)) +
  geom_line(size = 0.5,
            aes(x = Anthro, y = lambda, group = fullGrp, color = fullGrp), 
            alpha = 1) + scale_color_manual(values=pal)+
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  xlab("Anthropogenic disturbance (%)") +
  ylab(expression("Population Growth Rate " * lambda)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

# combine ggplots to one figure
ggpubr::ggarrange(plot_lambda, plot_recruitment3,base1, labels = "",
                  ncol = 3, vjust = 1)

ggsave(paste0(baseDir,"/analysis/paper/figs/Figure5_missisaDemoRates.png"), width = 12, height = 3.6, units = "in", 
       dpi = 1200)
