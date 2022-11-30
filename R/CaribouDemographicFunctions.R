#############################################################################

#################################################################################


# functions for caribou demographics paper

# run scenario - temporary wrapper
runScenario<-function(scns){
  retdir <-getwd()
  wdir <- app_path

  dir.create("figs")
  dir.create("tabs")

  source(paste0(wdir,"/CaribouDemoFns.R"))

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
  eParsIn$cowCounts <- data.frame(Year = 1981:2018,
                                  Count = 100,
                                  Class = "cow")
  eParsIn$freqStartsByYear <- data.frame(Year = 1981:2018,
                                         numStarts = 30)
  eParsIn$collarOnTime=1
  eParsIn$collarOffTime=12
  eParsIn$collarNumYears=1

  ##########
  #Get full set of sims for comparison
  simBig<-getSimsNational(wdir=wdir)#If called with default parameters, use saved object to speed things up.

  setwd(wdir)
  scResults = runScnSet(scns,eParsIn,simBig)
  setwd(retdir)
  return(scResults)
}
