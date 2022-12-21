#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
#devtools::load_all(here::here())

library(ggplot2)
source( paste0(app_path,"/CaribouDemoFns.R"))
scn_defaults <- c(eval(formals(fillDefaults)$defList),
                  curYear = eval(formals(fillDefaults)$curYear))

########################
#sensitivity
minimalScn = expand.grid(P=1,st=1,cmult=0,ri=1)

monitoringScns = expand.grid(P=seq(1,20),st=c(30,45,60),cmult=c(2,3,4),ri=c(1,2,3,4,5),assessmentYrs=c(1,3))
stateScns = expand.grid(iA=c(0,20,40,60),rep=seq(1:500))

monitoringScns = expand.grid(P=c(1,2,4,8,16),st=c(30,60),cmult=c(2,3,4),ri=c(1,2,4))
stateScns = expand.grid(iA=c(0,20,40,60),rep=seq(1:100))

monitoringScns = expand.grid(P=c(1,4,8,16),st=c(30,60),cmult=c(3),ri=c(1,2,4))
stateScns = expand.grid(iA=c(0),rep=seq(1:30))

stateScns$sQ=sample(seq(0.025,0.975,0.005),nrow(stateScns),replace=T)
stateScns$rQ = sample(seq(0.025,0.975,0.005),nrow(stateScns),replace=T)
monitoringScns=rbind(monitoringScns,minimalScn)
scns=merge(monitoringScns,stateScns)

scns$N0 = 10000
nrow(scns)
str(scns)
#scns=scns[10,]
#scResults = readRDS("temp.Rds")

#DO - add minimal monitoring.
#DO - deal with wierd prob viability values when pops are very small by (a) adding min pop size condition, and (b) calculate lambda over some number of years around target year
#DO - set up wrapper functions to work with tables of disturbance and monitoring scenarios.
#DO - figure out how to run on cloud.

scResults = runScenario(scns,getKSDists=F)

saveRDS(scResults,"temp.Rds")

probs = subset(scResults$rr.summary.all,(Parameter=="Population growth rate")&(Year>(iYr+P-1)))
probs$sQs = as.factor(probs$sQ)
probs$grp = paste0(probs$rQ,probs$sQ)


#plot probability of making an error about true population state
statusTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Population growth rate")&(Year>(iYr+P-1)))
statusTrue$trueMean = statusTrue$Mean
statusTrue$parameter=NULL;statusTrue$type=NULL;statusTrue$Mean=NULL
setdiff(names(statusTrue),names(probs))

probs = merge(probs,statusTrue)
probs$P[probs$st==1]=0


probs$viableTrue = probs$trueMean>0.99

probs$wrong = (probs$Mean<=0.99)&probs$viableTrue
probs$correct_status[probs$wrong]="no"
probs$correct_status[!probs$wrong]="yes"

pageVars= c("st","smult","iA","ri")
probs$pageLab = paste0("st",probs$st,"cmult",probs$cmult,"ri",probs$ri,"iA",probs$iA)
pages=unique(probs$pageLab)

for(p in pages){
  png(here::here(paste0("figs/sensitivity1Wrong",p,".png")),
      height = 4, width = 7.48, units = "in",res=600)
  base=ggplot(subset(probs,pageLab==p),aes(x=P,y=Mean,col=correct_status,group=grp))+geom_point(shape="-",size=3)+facet_wrap(~Anthro,labeller="label_both")+
    theme_bw()+xlab("years of monitoring")+ylab("Estimated mean population growth rate")
  print(base)
  dev.off()
}

################
#summarize outcome - proportion wrong

groupVars = c("Anthro",setdiff(names(scns),c("rQ","sQ","rep")))
probsSum <- probs %>% group_by(across(groupVars)) %>% summarize(propWrong = mean(wrong))
probsSum <- subset(probsSum,P>0)
probsSum$grp = paste(probsSum$st,probsSum$ri)

table(probsSum$grp)
pageVars= c("smult","iA")
probsSum$pageLab = paste0("cmult",probsSum$cmult,"iA",probsSum$iA)
pages=unique(probsSum$pageLab)

probsSum$RenewalInterval=probsSum$ri
probsSum$NumCollars = as.factor(probsSum$st)

for(p in pages){
  png(here::here(paste0("figs/sensitivity1PowerSummary",p,".png")),
      height = 4, width = 7.48, units = "in",res=600)
  base=ggplot(subset(probsSum,pageLab==p),aes(x=P,y=1-propWrong,linetype=NumCollars,col=RenewalInterval,group=grp))+geom_line()+facet_wrap(~Anthro,labeller="label_both")+
    theme_bw()+xlab("years of monitoring")+ylab("Probability of correct status assessment")
  print(base)
  dev.off()
}

