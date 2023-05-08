#!/usr/bin/env Rscript


# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
n_batches <- 24
n_scns <- 4

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

  scns = subset(allScns, pageId==cpageId)

  rm(allScns)

  message("batch ", cpageId, " started")

  scResults = caribouMetrics:::runScnSet(scns[1:n_scns,],eParsIn,simBig,getKSDists=F,printProgress=F)

  saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

  message("batch ", cpageId, " complete")

}
future::plan("sequential")

# mem allocated to run 24 scenarios 35.9 GB not necessarily used simultaneously
# while running 24 scenarios memory usage was up to 3.6 GB at one point
# D16s_v3 VM has 16 cores and twice as much memory 4 GB per core so might work better.
