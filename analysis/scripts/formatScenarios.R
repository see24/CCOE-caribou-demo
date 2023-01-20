
#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
library(caribouMetrics)

rawScns<-read.csv(here::here("tabs/s2.csv"))
nrow(rawScns)
rawScns = subset(rawScns,st!=1)

monitoringPars = list(P=list(description = "monitoring duration",unit="years"),
                      st=list(description="target # of collars", unit="cows"),
                      cmult=list(description="calf:cow survey multiplier",unit = "cows per collared cow"),
                      ri = list(description="time between collar deployments", unit="years"),
                      assessmentYrs = list(description="assessment duration",unit="years"))

start=T
for(par in names(monitoringPars)){
  #par="st"
  crow = data.frame(name=par,description=monitoringPars[[par]]$description,
                    units=monitoringPars[[par]]$unit,
                    values = paste(unique(rawScns[,par]),collapse=","))

  if(start){
    tbl = crow
    start=F
  }else{
    tbl=rbind(tbl,crow)
  }
}

