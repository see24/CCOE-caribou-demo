#!/usr/bin/env Rscript

#Run scenario batches on cloud machine
#assuming workingDir is where we are, cmd li{ne invocation is
#Rscript --vanilla ./cloudDeploymentSandbox/Caribou-Demographic-Projection-Paper/Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R 1
#Rscript --vanilla ./analysis/scripts/sensitivityMinimal.R 1 "local"
#nohup Rscript --vanilla ./analysis/scripts/sensitivityMinimalTest.R 1 "local" &

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
}else if (args[2]=="local_docker"){
  baseDir = "home"
  workingDir = paste0(baseDir,"project")
  toolDir = paste0(baseDir,"tool")
  libDir = NULL
}else{
  baseDir = getwd()
  print(baseDir)
  workingDir = file.path(baseDir,"Caribou-Demographic-Projection-Paper")
  print(workingDir)
  toolDir = file.path(baseDir,"BayesianCaribouDemographicProjection")
  print(toolDir)
  libDir = "R-packages"
  print(libDir)
  print(installed.packages()[,1:2])
}

cpageId=args[1] #which batch?
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

allScns = read.csv(paste0("tabs/",setName,".csv"))
scns = subset(allScns,pageId==cpageId)
nrow(allScns)

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

saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

