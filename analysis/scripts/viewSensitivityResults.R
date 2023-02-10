#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
#devtools::load_all(here::here())
library(tidyverse)
library(ggplot2)
source( paste0(app_path,"/CaribouDemoFns.R"))
scn_defaults <- c(eval(formals(fillDefaults)$defList),
                  curYear = eval(formals(fillDefaults)$curYear))

########################
#sensitivity
setName = "s2"
scns = read.csv(here::here("tabs/s2.csv"))
scns = subset(scns,cmult!=0)
pages=unique(scns$pageId)
unique(scns$pageLab)

for(cpageId in pages){
  #cpageId=pages[1]
  p = scns$pageLab[scns$pageId==cpageId]

  if(!file.exists(paste0("results/",setName,"/r",cpageId,".Rds"))){next}
  scResults = readRDS(paste0("results/",setName,"/r",cpageId,".Rds"))

  probs = subset(scResults$rr.summary.all,(Parameter=="Population growth rate"))
  probs$projectionTime = probs$Year-2023
  probs$Anthro2023 = probs$tA
  probs$AssessmentYear = probs$Year

  #See disturbance scenarios
  #distScns = subset(probs,is.element(projectionTime,c(0,5,20))|(Year==iYr))
  distScns = subset(probs,is.element(projectionTime,c(0,5,20))|(Year<2023))
  distScns$grp = paste0(distScns$Anthro2023,distScns$aSf)

  distScns$Timeline[distScns$Year==2023]="Finish monitoring"
  distScns$Timeline[distScns$projectionTime==5]="Assessment 2028"
  distScns$Timeline[distScns$projectionTime==20]="Assessment 2043"
  distScns$Timeline[distScns$Year<2023]= paste("Start",distScns$P[distScns$Year<2023],"yrs monitoring")

  startLevels = unique(distScns$Timeline[distScns$Year<2023])
  #distScns$Timeline = factor(distScns$Timeline,levels=c(startLevels[length(startLevels):1],"Finish monitoring","Assessment 2028","Assessment 2043"))
  distScns$Anthro2023=as.character(distScns$Anthro2023)

  timelineLabs = unique(subset(distScns,(Year==iYr)|(Year>=2023),select=c(Year,Timeline,Anthro,Anthro2023,grp)))
  timelineLabs = timelineLabs[order(timelineLabs$Year),]
  distScns$Anthro=pmax(0,distScns$Anthro) #temp: remove this is next round of analysis. Problem fixed where it should be.
  png(here::here(paste0("figs/",setName,"/distScns",p,".png")),
      height = 4, width = 5.51, units = "in",res=600)
  base=ggplot(distScns,aes(x=Year,y=Anthro,col=Anthro2023,group=grp))+geom_line()+geom_point(data=timelineLabs)+
    theme_bw()+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,size=8)) +
    scale_x_continuous(name="Timeline", breaks=timelineLabs$Year, labels=timelineLabs$Timeline)+ylab("Anthropogenic Disturbance")
  print(base)
  dev.off()

  probs = subset(probs,is.element(projectionTime,c(5,20)))

  #plot probability of making an error about true population state
  statusTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Population growth rate")&(Year>(iYr+P-1)))
  statusTrue$trueMean = statusTrue$Mean
  statusTrue$parameter=NULL;statusTrue$type=NULL;statusTrue$Mean=NULL

  sizeTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Female population size")&(Year>(iYr+P-1)))
  sizeTrue$trueSize = sizeTrue$Mean
  sizeTrue$parameter=NULL;sizeTrue$type=NULL;sizeTrue$Mean=NULL
  setdiff(names(statusTrue),names(probs))

  sizeProj = subset(scResults$rr.summary.all,(Parameter=="Female population size"),select=c(label,Year,Mean))
  sizeProj$projSize = sizeProj$Mean; sizeProj$Mean=NULL

  probs = merge(probs,statusTrue)
  probs = merge(probs,sizeTrue)
  probs = merge(probs,sizeProj)
  probs$P[probs$st==1]=0
  probs$viableTrue = (probs$trueMean>0.99)&(probs$trueSize>10)

  probs$wrong = ((probs$Mean<=0.99)|(probs$projSize<=10))&probs$viableTrue
  probs$CorrectStatus[probs$wrong]="no"
  probs$CorrectStatus[!probs$wrong]="yes"

  probs$pageLabB = paste0(probs$pageLab,"st",probs$st,"ri",probs$ri)
  pagesB=unique(probs$pageLabB)

  for(pp in pagesB){
    #p=pages[1]
    png(here::here(paste0("figs/",setName,"/bands",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probs,pageLabB==pp),aes(x=P,y=Mean,col=CorrectStatus))+geom_point(shape="-",size=3)+
      facet_grid(AssessmentYear~Anthro2023,labeller="label_both")+
      theme_bw()+xlab("years of monitoring")+ylab("Estimated mean population growth rate")
    print(base)
    dev.off()
  }

  ################
  #summarize outcome - proportion wrong
  str(probs)
  groupVars = c("Anthro","Anthro2023","AssessmentYear",setdiff(names(scns),c("rQ","sQ","rep")))
  probsSum <- probs %>% group_by(across(groupVars)) %>% summarize(propWrong = mean(wrong))
  probsSum <- subset(probsSum,P>0)
  probsSum$grp = paste(probsSum$st,probsSum$ri)

  table(probsSum$grp)
  probsSum$pageLabC = probsSum$pageLab
  pagesC=unique(probsSum$pageLabC)

  probsSum$RenewalInterval=as.factor(probsSum$ri)
  probsSum$NumCollars = as.factor(probsSum$st)

  probsSum$CollarYrs = as.numeric(as.character(probsSum$NumCollars))*probsSum$P
  for(pp in pagesC){
    #pp=pagesC[1]
    png(here::here(paste0("figs/",setName,"/power",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=1-propWrong,y=P,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(AssessmentYear~Anthro2023,labeller="label_both")+
      theme_bw()+ylab("years of monitoring")+xlab("Probability of correct status assessment")
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/powerEffort",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=CollarYrs,y=1-propWrong,linetype=RenewalInterval,col=NumCollars,group=grp))+geom_line()+
      facet_grid(AssessmentYear~Anthro2023,labeller="label_both")+
      theme_bw()+xlab("years of monitoring * NumCollars")+ylab("Probability of correct status assessment")
    print(base)
    dev.off()

  }
}


