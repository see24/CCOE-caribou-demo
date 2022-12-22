#Run scenario batches on cloud machine

workingDir = "C:/Users/HughesJo/Documents/gitprojects/Caribou-Demographic-Projection-Paper"
toolDir = "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
libDir = .libPaths()[1]
cpageId=2 #which batch?
setName = "s1"


#######################
setwd(workingDir)
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)

packages <- c("R2jags","gdata","mcmcplots",
              "ggplot2","RODBC","plyr","survival","gdata","data.table",
              "tidyr","dplyr","caribouMetrics")

for(p in packages){library(p,lib.loc=libDir,character.only=T)}

scns = subset(read.csv(paste0("tabs/",setName,".csv")),pageId==cpageId)
nrow(scns)

####################
eParsIn = list()
eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                Count = 100,
                                Class = "cow")
eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                       numStarts = 30)
eParsIn$collarOnTime=1
eParsIn$collarOffTime=12
eParsIn$collarNumYears=3


setwd(toolDir)

source("CaribouDemoFns.R")
simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.

scResults = runScnSet(scns[1,],eParsIn,simBig,getKSDists=F,printProgress=F)
setwd(workingDir)

saveRDS(scResults,paste0("results/",setName,"/r",cpageId,".Rds"))

