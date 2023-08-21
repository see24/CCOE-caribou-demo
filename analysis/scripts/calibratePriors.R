#devtools::load_all(here::here())

scnsApplyAll = data.frame(qMin=0,qMax=0,uMin=0,uMax=0,zMin=0,zMax=0,adjustR=F)
scnsNoData = data.frame(obsYears=1,collarCount=0,cowMult=0,projYears=1)
scnsLow = data.frame(obsYears=15,collarCount=60,cowMult=9,projYears=1,rQuantile=0.025,sQuantile=0.025)
scnsHigh = scnsLow;scnsHigh$rQuantile =0.975;scnsHigh$sQuantile=0.975
########################
#Survival
#vary sIntSEMod, sInterannualVar, and sInterannualVarSE in factorial array. Outcome of interest is KS distance
sIntSEMod=seq(1,10,by=2);sInterannualVar=0.08696*seq(0.2,1,by=0.2);sInterannualVarSE=seq(0.01,0.09,by=0.02)

#Looking for low KS distance from full range of input sims when given almost no info
scnsSInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,sIntSEMod=sIntSEMod,
                     sInterannualVar=sInterannualVar,sInterannualVarSE=sInterannualVarSE)
scnsSInt$SigmaMean = scnsSInt$sInterannualVar
scns=merge(scnsSInt,scnsApplyAll);scns=merge(scns,scnsNoData)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2023)&(Parameter=="Adult female survival"))
addBit = paste0("InterceptAll")
scResults$obs.all = NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("SigmaMean","sIntSEMod"),loopVars="sInterannualVarSE",whichPlots=c("Adult female survival"))

scns=merge(scnsSInt,scnsApplyAll);scns=merge(scns,scnsLow)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("InterceptLow")
KSLow = subset(scResultsLow$ksDists,(Year==2023)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("SigmaMean","sIntSEMod"),loopVars="sInterannualVarSE",
                   whichPlots=c("Adult female survival"),useNational=F)

scns=merge(scnsSInt,scnsApplyAll);scns=merge(scns,scnsHigh)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("InterceptHigh")
KSHigh = subset(scResultsHigh$ksDists,(Year==2023)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("SigmaMean","sIntSEMod"),loopVars="sInterannualVarSE",
                   whichPlots=c("Adult female survival"),survLow=0.8,useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","sInterannualVar","sIntSEMod","sInterannualVarSE","scn")
KS = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KS,here::here(paste0("tabs/SurvKSAll.csv")))

#vary sAnthroSlopeSE to get slope.
sAnthroSlopeSEMod=seq(1,10,by=2)
scnsSSlope=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0, iAnthro = 90,
                      sIntSEMod=5,sInterannualVar=0.034784,sInterannualVarSE=0.03,
                      sAnthroSlopeSEMod=sAnthroSlopeSEMod)
scnsSSlope$sSlopeSEMod=scnsSSlope$sAnthroSlopeSEMod
scns=merge(scnsSSlope,scnsApplyAll);scns=merge(scns,scnsNoData)
scResults = runScenario(scns,survAnalysisMethod = "Exponential",Anthro=90)
KSAll = subset(scResults$ksDists,(Year==2023)&(Parameter=="Adult female survival"))
addBit = paste0("SlopeAll")
scResults$obs.all=NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sSlopeSEMod"),whichPlots=c("Adult female survival"))

sSlopeMod=c(0,1,2)
scns=merge(scnsSSlope,scnsApplyAll);scns=merge(scns,data.frame(sSlopeMod=sSlopeMod))
scns=merge(scns,scnsLow)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=90,survAnalysisMethod = "KaplanMeier")
addBit = paste0("SlopeLow")
KSLow = subset(scResultsLow$ksDists,(Year==2023)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("sSlopeSEMod","sSlopeMod"),whichPlots=c("Adult female survival"),
                   useNational=F)

scns=merge(scnsSSlope,scnsApplyAll);scns=merge(scns,data.frame(sSlopeMod=sSlopeMod))
scns=merge(scns,scnsHigh)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=90,survAnalysisMethod = "KaplanMeier")
addBit = paste0("SlopeHigh")
KSHigh = subset(scResultsHigh$ksDists,(Year==2023)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("sSlopeSEMod","sSlopeMod"),whichPlots=c("Adult female survival"),
                   survLow=0.8,useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","sSlopeSEMod","sSlopeMod","scn")
KSAnthro90 = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KSAnthro90,here::here(paste0("tabs/SurvKSAllAnthro90.csv")))

########################
#recruitment
#vary rIntSEMod, rInterannualVar, and rInterannualVarSE in factorial array. Outcome of interest is KS distance
rIntSEMod=seq(1,7,by=1.5);rInterannualVar=0.46*seq(0.5,1.5,by=0.25);rInterannualVarSE=seq(0.14,0.28,by=0.04)

#Looking for low KS distance from full range of input sims when given almost no info
scnsRInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,rIntSEMod=rIntSEMod,
                     rInterannualVar=rInterannualVar,rInterannualVarSE=rInterannualVarSE)
scnsRInt$SigmaMean = scnsRInt$rInterannualVar
scns=merge(scnsRInt,scnsApplyAll);scns=merge(scns,scnsNoData)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2023)&(Parameter=="Recruitment"))
addBit = paste0("InterceptAll")
scResults$obs.all = NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("SigmaMean","rIntSEMod"),loopVars="rInterannualVarSE",whichPlots=c("Recruitment"))

scns=merge(scnsRInt,scnsApplyAll);scns=merge(scns,scnsLow)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("InterceptLow")
KSLow = subset(scResultsLow$ksDists,(Year==2023)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("SigmaMean","rIntSEMod"),loopVars="rInterannualVarSE",
                   whichPlots=c("Recruitment"),useNational=F)

scns=merge(scnsRInt,scnsApplyAll);scns=merge(scns,scnsHigh)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("InterceptHigh")
KSHigh = subset(scResultsHigh$ksDists,(Year==2023)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("SigmaMean","rIntSEMod"),loopVars="rInterannualVarSE",
                   whichPlots=c("Recruitment"),useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","rInterannualVar","rIntSEMod","rInterannualVarSE","scn")
KS = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KS,here::here(paste0("tabs/RecKSAll.csv")))

#vary rAnthroSlopeSE to get slope.
rAnthroSlopeSEMod=seq(1,7,by=1.5)
scnsRSlope=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0, iAnthro = 90,
                       rIntSEMod=4,rInterannualVar=0.23,rInterannualVarSE=0.22,
                       rAnthroSlopeSEMod=rAnthroSlopeSEMod)
scnsRSlope$rSlopeSEMod=scnsRSlope$rAnthroSlopeSEMod
scns=merge(scnsRSlope,scnsApplyAll);scns=merge(scns,scnsNoData)
scResults = runScenario(scns,survAnalysisMethod = "Exponential",Anthro=90)
KSAll = subset(scResults$ksDists,(Year==2023)&(Parameter=="Recruitment"))
addBit = paste0("SlopeAll")
scResults$obs.all=NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rSlopeSEMod"),whichPlots=c("Recruitment"))

rSlopeMod=c(0,1,2)
scns=merge(scnsRSlope,scnsApplyAll);scns=merge(scns,data.frame(rSlopeMod=rSlopeMod))
scns=merge(scns,scnsLow)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=90,survAnalysisMethod = "KaplanMeier")
addBit = paste0("SlopeLow")
KSLow = subset(scResultsLow$ksDists,(Year==2023)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("rSlopeSEMod","rSlopeMod"),whichPlots=c("Recruitment"),
                   useNational=F)

scns=merge(scnsRSlope,scnsApplyAll);scns=merge(scns,data.frame(rSlopeMod=rSlopeMod))
scns=merge(scns,scnsHigh)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=90,survAnalysisMethod = "KaplanMeier")
addBit = paste0("SlopeHigh")
KSHigh = subset(scResultsHigh$ksDists,(Year==2023)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("rSlopeSEMod","rSlopeMod"),whichPlots=c("Recruitment"),
   useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","rSlopeSEMod","rSlopeMod","scn")
KSAnthro90 = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KSAnthro90,here::here(paste0("tabs/RecKSAllAnthro90.csv")))


