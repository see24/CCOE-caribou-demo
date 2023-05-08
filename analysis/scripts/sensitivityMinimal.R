#!/usr/bin/env Rscript

#Run scenario batches on cloud machine
#assuming workingDir is where we are, cmd li{ne invocation is
#Rscript --vanilla ./cloudDeploymentSandbox/Caribou-Demographic-Projection-Paper/Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R 1
#Rscript --vanilla ./analysis/scripts/sensitivityMinimal.R 1 "local"
#nohup Rscript --vanilla ./analysis/scripts/sensitivityMinimal.R 1 "local" &

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
library(doFuture)
library(caribouMetrics)

setName = "s4"

#######################
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)



future::plan("multisession")

foreach(cpageId = 1:n_batches, .options.future = list(seed = TRUE)) %dofuture% {

  simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.

  allScns = read.csv(paste0("tabs/",setName,".csv"))
  n_batches <- length(unique(allScns$pageId))

  ####################
  eParsIn = list()
  eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                  Count = 100,
                                  Class = "cow")
  eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                         numStarts = 30)
  eParsIn$collarOnTime=1
  eParsIn$collarOffTime=12
  eParsIn$collarNumYears=6

  scns = subset(allScns, pageId==cpageId)

  rm(allScns)

  message("batch ", cpageId, " started")

  scResults = caribouMetrics:::runScnSet(scns,eParsIn,simBig,getKSDists=F,printProgress=F)

  saveRDS(scResults,paste0("results/",setName,"/r",cpageId,".Rds"))

  message("batch ", cpageId, " complete")

}
future::plan("sequential")
