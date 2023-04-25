#!/usr/bin/env Rscript

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
n_batches <- 6
n_scns <- 2

setName = "s2"

#######################
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)

packages <- c("R2jags","gdata","mcmcplots",
              "ggplot2","RODBC","plyr","dplyr","survival","gdata","data.table",
              "tidyr","caribouMetrics")

for(p in packages){library(p,character.only=T)}

simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.



allScns = read.csv(paste0("tabs/",setName,".csv"))

library(doFuture)

future::plan("multisession")

foreach(cpageId = 1:n_batches) %dofuture% {

  scns = subset(allScns,pageId==cpageId)
  nrow(allScns)

  message("batch ", cpageId, " started")

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

  scResults = caribouMetrics:::runScnSet(scns[1:n_scns,],eParsIn,simBig,getKSDists=F,printProgress=F)


  saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

  message("batch ", cpageId, " complete")

}
