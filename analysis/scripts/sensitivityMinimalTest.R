#!/usr/bin/env Rscript
#nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTest.R" 1 &
n_reps <- "all"

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
cpageId <- commandArgs(trailingOnly = TRUE)
# cpageId <- 5

library(caribouMetrics)

setName = "s5"

#######################
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)


simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.

allScns = read.csv(paste0("tabs/",setName,".csv"))
unique(allScns$pageId)
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

message("batch ", cpageId, " started")

if(n_reps=="all"){
  scResults = caribouMetrics:::runScnSet(scns,eParsIn,simBig,getKSDists=F,printProgress=F)
}else{
  scResults = caribouMetrics:::runScnSet(subset(scns,rep<=n_reps),eParsIn,simBig,getKSDists=F,printProgress=F)
}

saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

message("batch ", cpageId, " complete")
