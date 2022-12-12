#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"


#vary lse, sse, and ssv in factorial array. Outcome of interest is KS distance
lse=seq(1,10,by=2);sse=0.08696*seq(0.2,1,by=0.2);ssv=seq(0.01,0.09,by=0.02)

#Looking for low KS distance from full range of input sims when given almost no info
numObsYrs=c(1);startsByYr = 1;J=1
scns=expand.grid(P=numObsYrs,aSf=0,J=J,st=startsByYr,lse=lse,sse=sse,ssv=ssv)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")

KSAll = subset(scResults$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
addBit = paste0("sQStarts",startsByYr)

makeInterceptPlots(scResults,addBit=addBit,facetVars=c("lse","sse"),loopVars="ssv",whichPlots=c("Adult female survival"))

#Also looking for low KS distance from range of input sims that match the observed data when given lots of observed data
#Case 1 - low survival/recruitment quantile
#Case 2 - high survival/recruitment quantile

numObsYrs=c(15);startsByYr = 300;J=1;cw=200;sQ=0.025;rQ=0.025
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,lse=lse,sse=sse,ssv=ssv)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("sQStarts",startsByYr,"low")
KSLow = subset(scResultsLow$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("lse","sse"),loopVars="ssv",whichPlots=c("Adult female survival"))

numObsYrs=c(15);startsByYr = 300;J=1;cw=200;sQ=0.975;rQ=0.975
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,lse=lse,sse=sse,ssv=ssv)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("sQStarts",startsByYr,"high")
KSHigh = subset(scResultsHigh$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
#source("CaribouDemoFns.R")

makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("ssv","sse"),loopVars="lse",whichPlots=c("Adult female survival"),survLow=0.8)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"

sCols = c("KSDistance","sse","lse","ssv","scn")
KS = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KS,here::here(paste0("tabs/SurvKSAll.csv")))

#png(paste0("figs/SurvKSAll.pdf"),width=10,height=7)
#base = ggplot(KS,aes(x=lse,y=KSDistance,color=sse))+
#  geom_point()+facet_grid(scn~ssv,labeller="label_both",scales="free_y")+theme_bw()
#print(base)
#dev.off()

#Interpretation
#lse <5 is too constraining, does not allow intercept to adjust when presented with local evidence (from rows 2 and 3 of KS dist plot).
#Low ssv and sse give best match to all distribution (row 1 of KS dist plot, SurvsQStarts1ssv0.01.pdf).


if(0){
  #Next - survival slope parameter sensitivity
  #CaribouDemo_v1.15Reduce.R

  numObsYrs=c(2,5,20);startsByYr = 25; iA=90
  scns=expand.grid(P=numObsYrs,sQ=c(0.025,0.5,0.975),iA=iA,st=startsByYr)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"iA",iA)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","sQ"))

  numObsYrs=c(2,5,20);startsByYr = 25; iA=90; bse=1
  scns=expand.grid(P=numObsYrs,sQ=c(0.025,0.5,0.975),iA=iA,st=startsByYr,bse=bse)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"iA",iA,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","sQ"))

  numObsYrs=c(20);startsByYr = 25;J=3; iA=0; aS=4; bse=7;sS=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=J,sQ=c(0.025,0.5,0.975),iA=iA,aS=4,
                   st=startsByYr,bse=bse,sS=sS)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",aS,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sS","sQ"))

  numObsYrs=c(20);startsByYr = 25;J=3; iA=0; aS=4; bse=1;sS=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=J,sQ=c(0.025,0.5,0.975),iA=iA,aS=4,
                   st=startsByYr,bse=bse,sS=sS)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",aS,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sS","sQ"))

  #Results
  #ssv=0.8696: too much variability when no info available
  #sse=0.1: too much variability when no info available
  #lse=1: too much constraint, can't adjust mean even with lots of local data
  #startsByYr: reducing sample size increases variation in observations among years. More difficult to infer differences or narrow CIs even with many years of data.

  ###############
  #Step 2: confirm appropriate prior variability in recruitment intercept using minimal (2) observed data points & 0 fire/anthro covariates. Controlled by priors on l.Saf, phi and sig.Saf.
  #################
  #source("CaribouDemoFns.R")
  numObsYrs=c(2);startsByYr = 25
  scns=expand.grid(P=numObsYrs,rQ=c(0.5),st=startsByYr)
  scResults = runScnSet(scns,eParsIn,simBig)
  print(plotRes(scResults$rr.summary.all, "Recruitment",obs=scResults$obsRec.all,
                lowBound=0,simRange=scResults$sim.all,facetVars=c("P","rQ")))

  numObsYrs=c(2,5,20);startsByYr = 25;lre=1
  scns=expand.grid(P=numObsYrs,rQ=c(0.025,0.5,0.975),st=startsByYr,lre=lre)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"lre",lre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))

  numObsYrs=c(2,5,20);startsByYr = 25;sre=0.1
  scns=expand.grid(P=numObsYrs,rQ=c(0.025,0.5,0.975),st=startsByYr,sre=sre)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"sre",sre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))

  numObsYrs=c(2,5,20);startsByYr = 25
  scns=expand.grid(P=numObsYrs,rQ=c(0.025,0.5,0.975),st=startsByYr)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))

  numObsYrs=c(2,5,20);startsByYr = 25; iA=90
  scns=expand.grid(P=numObsYrs,rQ=c(0.025,0.5,0.975),iA=iA,st=startsByYr)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"iA",iA)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))

  numObsYrs=c(2,5,20);startsByYr = 25; iA=90; bre=1
  scns=expand.grid(P=numObsYrs,rQ=c(0.025,0.5,0.975),iA=iA,st=startsByYr,bre=bre)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"iA",iA,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))

  numObsYrs=c(20);startsByYr = 25;J=2; iA=0; aS=4; bre=7;rS=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=J,rQ=c(0.025,0.5,0.975),iA=iA,aS=4,st=startsByYr,bre=bre,rS=rS)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",aS,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rS","rQ"))

  numObsYrs=c(20);startsByYr = 25;J=2; iA=0; aS=4; bre=1;rS=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=J,rQ=c(0.025,0.5,0.975),iA=iA,aS=4,st=startsByYr,bre=bre,rS=rS)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",aS,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rS","rQ"))

  #TO DO: extend anthro logic to fire.
  #TO DO: make warning fn from example above.
  #Simulating reference range of variability takes some time, so provide option in UI to skip this check.
  #But default should be to complain about parameter combos that give answers outside the range of what has been observed across the country.
  #"Warning: for (anthro/fire) x,x,etc fitted 95% CI for (rec/surv/lambda) does not overlap range of variation simulated from national model.
  #Note: could reduce time for simulating reference range by saving results - they generally don't change.

  #Results
  #sre=0.1: not enough variability when no info available
  #lre=1: too much constraint, can't adjust mean even with lots of local data
  #startsByYr: reducing sample size increases variation in observations among years. More difficult to infer differences or narrow CIs even with many years of data.

  #TO DO: put lambda=1 on lambda plots

  #################
  #TO DO: show effect of changing uncertainty about anthro slope when no previous anthro
  #source("CaribouDemoFns.R")
  anthroSlope=0 #%change in anthro per year
  anthroSlopeFuture=4
  numObsYrs = c(2,5,20) # number of years of data
  numProjectYrs = 20 #number of years to project
  startsByYr = 25; iA=0
  bre=1
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,rQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bre=bre)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))
  #TO DO: this doesn't make sense. Figure out what is wrong...

  bre=7
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,rQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bre=bre)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))

  bre=3
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,rQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bre=bre)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","rQ"))


  anthroSlope=2 #%change in anthro per year
  anthroSlopeFuture=2
  numProjectYrs = 20 #number of years to project
  startsByYr = 25; iA=0
  numObsYrs=20
  bre=3; recSlopeMultiplier=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,rQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bre=bre,rS=recSlopeMultiplier)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rS","rQ"))

  bre=1; recSlopeMultiplier=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,rQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bre=bre,rS=recSlopeMultiplier)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rS","rQ"))

  bre=7; recSlopeMultiplier=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,rQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bre=bre,rS=recSlopeMultiplier)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("rQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bre",bre)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rS","rQ"))

  #Result: bre=3 is a reasonable compromise. Not too much variation in absence of info,
  #and vague enough to allow possibility of some other relationship.

  ############
  #TO DO: show effect of changing uncertainty about anthro slope when no previous anthro
  #source("CaribouDemoFns.R")
  anthroSlope=0 #%change in anthro per year
  anthroSlopeFuture=4
  numObsYrs = c(2,5,20) # number of years of data
  numProjectYrs = 20 #number of years to project
  startsByYr = 25; iA=0
  bse=1
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,sQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bse=bse)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","sQ"))

  bse=5
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,sQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bse=bse)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","sQ"))

  bse=7
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,sQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bse=bse)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("P","sQ"))

  anthroSlope=2 #%change in anthro per year
  anthroSlopeFuture=2
  numProjectYrs = 20 #number of years to project
  startsByYr = 25; iA=0
  numObsYrs=20
  bse=5; sefSlopeMultiplier=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,sQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bse=bse,sS=sefSlopeMultiplier)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sS","sQ"))

  bse=1; sefSlopeMultiplier=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,sQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bse=bse,sS=sefSlopeMultiplier)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sS","sQ"))

  bse=7; sefSlopeMultiplier=c(0,1,2)
  scns=expand.grid(P=numObsYrs,J=numProjectYrs,sQ=c(0.025,0.5,0.975),
                   iA=iA,aS=anthroSlope,aSf=anthroSlopeFuture,
                   st=startsByYr,bse=bse,sS=sefSlopeMultiplier)
  scResults = runScnSet(scns,eParsIn,simBig)
  addBit = paste0("sQStarts",startsByYr,"aS",anthroSlope,"aSf",anthroSlopeFuture,"bse",bse)
  makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sS","sQ"))

}
