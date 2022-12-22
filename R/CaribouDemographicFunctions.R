#############################################################################

#################################################################################


# functions for caribou demographics paper

# run scenario - temporary wrapper
runScenario<-function(scns,quants=NULL,Anthro=NULL,survAnalysisMethod="KaplanMeier",getKSDists=T){
  #quants=NULL;Anthro=NULL;survAnalysisMethod="KaplanMeier";getKSDists=F
  retdir <-getwd()
  wdir <- app_path

  dir.create("figs")
  dir.create("tabs")
  dir.create("results")

  source(paste0(wdir,"/CaribouDemoFns.R"))

  scns<-fillDefaults(scns)

  # usage
  packages <- c("shiny", "R2jags","gdata","mcmcplots",
                "ggplot2", "rmarkdown", "knitr",
                "RODBC","plyr","survival","gdata","data.table")

  ipak(packages)
  load.module("glm")

  library(caribouMetrics)
  library(tidyr)
  library(dplyr)

  eParsIn = list()
  eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                  Count = 100,
                                  Class = "cow")
  eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                         numStarts = 30)
  eParsIn$collarOnTime=1
  eParsIn$collarOffTime=12
  eParsIn$collarNumYears=3

  ##########
  #Get full set of sims for comparison

  if(is.null(quants)){
    simBig<-getSimsNational(wdir=wdir,adjustR=unique(scns$adjustR))#If called with default parameters, use saved object to speed things up.
  }else{
    simBig<-getSimsNational(quants=quants,Anthro=Anthro,adjustR=unique(scns$adjustR))#If called with default parameters, use saved object to speed things up.
  }

  setwd(wdir)
  scResults = runScnSet(scns,eParsIn,simBig,survAnalysisMethod,getKSDists=getKSDists,printProgress=T)
  setwd(retdir)
  return(scResults)
}
