# Prior and posterior predictions from Bayesian model
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

baseDir <- "."

simBig<-getSimsNational(replicates=3000,forceUpdate=T) #If called with default parameters, use saved object to speed things up.

monitoringScns = data.frame(obsYears=c(1,15),collarCount=c(1,30),cowMult=c(6),collarInterval=c(1),assessmentYrs=c(3))
stateScns = data.frame(obsAnthroSlope=c(2),projAnthroSlope=c(2))
stateScns = merge(stateScns,data.frame(rep=seq(1:1)))

stateScns$sQuantile=0.8
stateScns$rQuantile = 0.8
scns=merge(monitoringScns,stateScns)

scns$iAnthro = 0
scns$tA = scns$iAnthro+(scns$obsYears)*scns$obsAnthroSlope
scns$projYears = 50-scns$obsYears
scns$N0 = 1000
scns$adjustR = TRUE

####################
eParsIn = list()
eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                Count = 100,
                                Class = "cow")
eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                       numStarts = 30)
eParsIn$collarOnTime=1
eParsIn$collarOffTime=12
eParsIn$collarNumYears=1

labFontSize = 10; breakInterval=5

priorResult = caribouMetrics:::runScnSet(scns[1,],eParsIn,simBig,getKSDists=F,printProgress=F)
priorResult$obs.all=NULL
recPrior =  plotRes(priorResult, "Recruitment", lowBound=0, highBound = 0.75,
                    legendPosition="left",breakInterval=breakInterval,labFontSize=labFontSize)
plot(recPrior)
survPrior =  plotRes(priorResult, "Adult female survival", lowBound=0.6,
                     legendPosition="none",breakInterval=breakInterval,labFontSize=labFontSize)
plot(survPrior)
lambdaPrior =  plotRes(priorResult, "Population growth rate", lowBound=0.6,
                       legendPosition="none",breakInterval=breakInterval,labFontSize=labFontSize)
plot(lambdaPrior)

posteriorResult = caribouMetrics:::runScnSet(scns[2,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosterior =  plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = 0.75,
                        legendPosition="left",breakInterval=breakInterval,labFontSize=labFontSize)
plot(recPosterior)
survPosterior =  plotRes(posteriorResult, "Adult female survival", lowBound=0.6,
                         legendPosition="none",breakInterval=breakInterval,labFontSize=labFontSize)
plot(survPosterior)
lambdaPosterior =  plotRes(posteriorResult, "Population growth rate", lowBound=0.6,
                           legendPosition="none",breakInterval=breakInterval,labFontSize=labFontSize)
plot(lambdaPosterior)

# combine ggplots to one figure
ggpubr::ggarrange(recPrior,survPrior, lambdaPrior,
                  recPosterior,survPosterior, lambdaPosterior, labels = "",
                  ncol = 3, nrow=2,vjust = 1,widths=c(1,0.7,0.7))

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamples.png"), width = 12*0.8, height = 3.6*2, units = "in",
       dpi = 1200)

