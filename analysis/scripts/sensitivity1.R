#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
#devtools::load_all(here::here())

library(ggplot2)
source( paste0(app_path,"/CaribouDemoFns.R"))
scn_defaults <- c(eval(formals(fillDefaults)$defList),
                  curYear = eval(formals(fillDefaults)$curYear))

########################
#sensitivity
setName = "s1"
minimalScn = expand.grid(P=1,st=1,cmult=0,ri=1,assessmentYrs=1)

monitoringScns = expand.grid(P=c(1,2,4,8,16),st=c(30,60),cmult=c(2,3,4),ri=c(1,2,4),assessmentYrs=c(1,3))
stateScns = data.frame(tA=c(0,20,40,60,0,20,40,60),aS=c(0,1,1,1,0,0,0,0),aSf=c(1,1,1,1,0,0,0,0))
stateScns = merge(stateScns,data.frame(rep=seq(1:100)))

monitoringScns = expand.grid(P=c(1,4,16),st=c(60),cmult=c(3),ri=c(1),assessmentYrs=c(3))
stateScns = data.frame(tA=c(0,20,40,60),aS=c(0,1,1,1),aSf=c(1,1,1,1))
stateScns = merge(stateScns,data.frame(rep=seq(1:10)))

#monitoringScns = expand.grid(P=c(1,4,8,16),st=c(30,60),cmult=c(3),ri=c(1,2,4),assessmentYrs=1)
#stateScns = expand.grid(iA=c(0),rep=seq(1:30))

stateScns$sQ=runif(nrow(stateScns),min=0.01,max=0.99)
stateScns$rQ = runif(nrow(stateScns),min=0.01,max=0.99)
monitoringScns=rbind(monitoringScns,minimalScn)
scns=merge(monitoringScns,stateScns)

scns$iA = scns$tA-scns$P*scns$aS+1

scns$N0 = 10000
nrow(scns)
#scns=scns[10,]
#scResults = readRDS("temp.Rds")

if(0){
  start_time <- Sys.time()
  timeSample = runScenario(scns[2:11],getKSDists=F)
  end_time <- Sys.time()
  runTime10 = end_time - start_time
  runTimeFull = (as.numeric(runTime10)/10)*nrow(scns)/(60*24)
  runTimeFull # of processor days, roughly.
}else{runTime10=1.27}

scns$pageLab = paste0("cmult",scns$cmult,"ay",scns$assessmentYrs,"aSf",scns$aSf)
scns$pageId = as.numeric(as.factor(scns$pageLab))
unique(scns$pageId)

write.csv(scns,"tabs/s1.csv",row.names=F)
pages=unique(scns$pageLab)

#DO - add minimal monitoring.
#DO - deal with wierd prob viability values when pops are very small by (a) adding min pop size condition, and (b) calculate lambda over some number of years around target year
#DO - set up wrapper functions to work with tables of disturbance and monitoring scenarios.
#DO - figure out how to run on cloud.
runTimes=list()
str(scns)
for(pp in pages){
  #pp=pages[1]
  start_time <- Sys.time()

  (as.numeric(runTime10)/10)*nrow(subset(scns,pageLab==pp))/(60)

  scResults = runScenario(subset(scns,pageLab==pp),getKSDists=F)

  saveRDS(scResults,paste0("results/r",pp,".Rds"))

  probs = subset(scResults$rr.summary.all,(Parameter=="Population growth rate"))
  probs$projectionTime = probs$Year-2023
  probs$Anthro2023 = probs$tA
  probs$AssessmentYear = probs$Year

  #See disturbance scenarios
  distScns = subset(probs,is.element(projectionTime,c(0,5,20))|(Year==iYr))
  distScns$grp = paste0(distScns$Anthro2023,distScns$aSf)
  distScns$Timeline[distScns$Year==2023]="Final monitoring year"
  distScns$Timeline[distScns$projectionTime==5]="AssessmentYear 2028"
  distScns$Timeline[distScns$projectionTime==20]="AssessmentYear 2043"
  distScns$Timeline[distScns$Year<2023]= paste("Start",distScns$P[distScns$Year<2023],"yrs of monitoring")

  startLevels = unique(distScns$Timeline[distScns$Year<2023])
  distScns$Timeline = factor(distScns$Timeline,levels=c(startLevels,"Final monitoring year","AssessmentYear 2028","AssessmentYear 2043"))

  png(here::here(paste0("figs/distScns",p,".png")),
      height = 4, width = 5.51, units = "in",res=600)
  base=ggplot(distScns,aes(x=Year,y=Anthro,col=Anthro2023,shape=Timeline,group=grp))+geom_line()+geom_point()+
    theme_bw()+xlab("Year")+ylab("Anthropogenic Disturbance")
  print(base)
  dev.off()

  probs = subset(probs,is.element(projectionTime,c(5,20)))

  #plot probability of making an error about true population state
  statusTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Population growth rate")&(Year>(iYr+P-1)))
  statusTrue$trueMean = statusTrue$Mean
  statusTrue$parameter=NULL;statusTrue$type=NULL;statusTrue$Mean=NULL
  setdiff(names(statusTrue),names(probs))

  probs = merge(probs,statusTrue)
  probs$P[probs$st==1]=0

  probs$viableTrue = probs$trueMean>0.99

  probs$wrong = (probs$Mean<=0.99)&probs$viableTrue
  probs$CorrectStatus[probs$wrong]="no"
  probs$CorrectStatus[!probs$wrong]="yes"

  probs$pageLab = paste0("cmult",probs$cmult,"ay",probs$assessmentYrs,"st",probs$st,"ri",probs$ri)
  pages=unique(probs$pageLab)
  unique(probs$currentAnthro)

  for(p in pages){
    #p=pages[1]
    png(here::here(paste0("figs/bands",p,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probs,pageLab==p),aes(x=P,y=Mean,col=CorrectStatus))+geom_point(shape="-",size=3)+
      facet_grid(AssessmentYear~Anthro2023,labeller="label_both")+
      theme_bw()+xlab("years of monitoring")+ylab("Estimated mean population growth rate")
    print(base)
    dev.off()
  }

  ################
  #summarize outcome - proportion wrong

  groupVars = c("Anthro","Anthro2023","AssessmentYear",setdiff(names(scns),c("rQ","sQ","rep")))
  probsSum <- probs %>% group_by(across(groupVars)) %>% summarize(propWrong = mean(wrong))
  probsSum <- subset(probsSum,P>0)
  probsSum$grp = paste(probsSum$st,probsSum$ri)

  table(probsSum$grp)
  probsSum$pageLab = paste0("cmult",probsSum$cmult,"ay",probsSum$assessmentYrs)
  pages=unique(probsSum$pageLab)

  probsSum$RenewalInterval=probsSum$ri
  probsSum$NumCollars = as.factor(probsSum$st)

  for(p in pages){
    png(here::here(paste0("figs/power",p,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLab==p),aes(x=P,y=1-propWrong,linetype=NumCollars,col=RenewalInterval,group=grp))+geom_line()+
      facet_grid(AssessmentYear~Anthro2023,labeller="label_both")+
      theme_bw()+xlab("years of monitoring")+ylab("Probability of correct status assessment")
    print(base)
    dev.off()
  }
  end_time <- Sys.time()
  runTimes[[pp]] = end_time - start_time
  print(runTimes)
}


