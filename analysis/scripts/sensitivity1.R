#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
#devtools::load_all(here::here())

########################
#sensitivity
#setName = "s1"
#minimalScn = expand.grid(P=1,st=1,cmult=0,ri=c(1,4),assessmentYrs=3)
#monitoringScns = expand.grid(P=c(1,2,4,8,16),st=c(30,60),cmult=c(2,3,4),ri=c(1,2,4),assessmentYrs=c(1,3))
#stateScns = data.frame(tA=c(0,20,40,60,0,20,40,60),aS=c(0,1,1,1,0,0,0,0),aSf=c(1,1,1,1,0,0,0,0))
#stateScns = merge(stateScns,data.frame(rep=seq(1:100)))

#monitoringScns = expand.grid(P=c(1,4,16),st=c(60),cmult=c(3),ri=c(1),assessmentYrs=c(3))
#stateScns = data.frame(tA=c(0,20,40,60),aS=c(0,1,1,1),aSf=c(1,1,1,1))
#stateScns = merge(stateScns,data.frame(rep=seq(1:10)))

#monitoringScns = expand.grid(P=c(1,4,8,16),st=c(30,60),cmult=c(3),ri=c(1,2,4),assessmentYrs=1)
#stateScns = expand.grid(iA=c(0),rep=seq(1:30))

setName = "s3"
monitoringScns = expand.grid(P=c(1,2,4,8,16,24),st=c(1,15,30,60),cmult=c(3,6,9),ri=c(1,4),assessmentYrs=c(3))
stateScns = data.frame(tA=c(0,20,40,60,0,20,40,60),aS=c(0,1,1,1,0,0,0,0),aSf=c(1,1,1,1,0,0,0,0))
stateScns = merge(stateScns,data.frame(rep=seq(1:200)))

stateScns$sQ=runif(nrow(stateScns),min=0.01,max=0.99)
stateScns$rQ = runif(nrow(stateScns),min=0.01,max=0.99)
#monitoringScns=rbind(monitoringScns,minimalScn)
scns=merge(monitoringScns,stateScns)

scns$iA = pmax(0,scns$tA-(scns$P+1)*scns$aS)
scns$repBatch = ceiling(scns$rep/50)
table(scns$repBatch)
scns$N0 = 1000
nrow(scns)/6
#scns=scns[10,]
#scResults = readRDS("temp.Rds")

scns$pageLab = paste0("cmult",scns$cmult,"ay",scns$assessmentYrs,"aSf",scns$aSf,"repBatch",scns$repBatch)
scns$pageId = as.numeric(as.factor(scns$pageLab))
unique(scns$pageId)

write.csv(scns,paste0("tabs/",setName,".csv"),row.names=F)
pages=unique(scns$pageLab)

#DO - set up wrapper functions to work with tables of disturbance and monitoring scenarios.
#DO - figure out how to run on cloud.
