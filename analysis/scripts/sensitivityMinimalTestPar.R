#!/usr/bin/env Rscript

# Run batches from Rscript that uses parallel backend

args = commandArgs(trailingOnly=TRUE)
#args=1
if(args[2]=="local"){
  baseDir = "C:/Users/HughesJo/Documents"
  workingDir = paste0(baseDir,"/gitprojects/Caribou-Demographic-Projection-Paper")
  toolDir = paste0(baseDir,"/gitprojects/BayesianCaribouDemographicProjection")
  libDir = paste0(baseDir,"")
}else if (args[2]=="local_SE"){
  baseDir = "C:/Users/EndicottS/Documents"
  workingDir = paste0(baseDir,"/gitprojects/Caribou-Demographic-Projection-Paper")
  toolDir = paste0(baseDir,"/gitprojects/BayesianCaribouDemographicProjection")
  libDir = paste0("C:/Users/endicotts/AppData/Local/Programs/R/R-4.2.1/library")
}else if (args[2]=="docker"){
  baseDir = "/"
  workingDir = paste0(baseDir,"Caribou-Demographic-Projection-Paper")
  toolDir = paste0(baseDir,"BayesianCaribouDemographicProjection")
  libDir = NULL
}else{
  baseDir = getwd()
  workingDir = file.path(baseDir,"Caribou-Demographic-Projection-Paper")
  toolDir = file.path(baseDir,"BayesianCaribouDemographicProjection")
  libDir = NULL
}


setName = "s2"



#######################
setwd(workingDir)
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)

packages <- c("R2jags","gdata","mcmcplots",
              "ggplot2","RODBC","plyr","dplyr","survival","gdata","data.table",
              "tidyr","caribouMetrics")

for(p in packages){library(p,lib.loc=libDir,character.only=T)}

setwd(toolDir)

source("CaribouDemoFns.R")
simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.

setwd(workingDir)

allScns = read.csv(paste0("tabs/",setName,".csv"))

library(doFuture)

future::plan("multisession")

foreach(cpageId = 1:6) %dofuture% {

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

  setwd(toolDir)

  scResults = runScnSet(scns[1:2,],eParsIn,simBig,getKSDists=F,printProgress=F)

  setwd(workingDir)

  saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

  message("batch ", cpageId, " complete")

}
