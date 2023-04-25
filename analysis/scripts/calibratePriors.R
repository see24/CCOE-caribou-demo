#devtools::load_all(here::here())

########################
#Survival
#vary lse, sse, and ssv in factorial array. Outcome of interest is KS distance
lse=seq(1,10,by=2);sse=0.08696*seq(0.2,1,by=0.2);ssv=seq(0.01,0.09,by=0.02)

#Looking for low KS distance from full range of input sims when given almost no info
numObsYrs=c(1);startsByYr = 1;J=1
scns=expand.grid(P=numObsYrs,aSf=0,J=J,st=startsByYr,lse=lse,sse=sse,ssv=ssv)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
addBit = paste0("sQStarts",startsByYr)
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("lse","sse"),loopVars="ssv",whichPlots=c("Adult female survival"))

numObsYrs=c(15);startsByYr = 45;J=1;cw=200;sQ=0.025;rQ=0.025
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,lse=lse,sse=sse,ssv=ssv)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("sQStarts",startsByYr,"low")
KSLow = subset(scResultsLow$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("lse","sse"),loopVars="ssv",
                   whichPlots=c("Adult female survival"),useNational=F)

numObsYrs=c(15);startsByYr = 45;J=1;cw=200;sQ=0.975;rQ=0.975
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,lse=lse,sse=sse,ssv=ssv)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("sQStarts",startsByYr,"high")
KSHigh = subset(scResultsHigh$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("ssv","sse"),loopVars="lse",
                   whichPlots=c("Adult female survival"),survLow=0.8,useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","sse","lse","ssv","scn")
KS = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KS,here::here(paste0("tabs/SurvKSAll.csv")))

#vary bse to get slope.
bse=seq(1,10,by=2);sS=c(1);iA=90

numObsYrs=c(1);startsByYr = 1;J=1
scns=expand.grid(P=numObsYrs,aSf=0,J=J,st=startsByYr,bse=bse,sS=sS,iA=iA)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
addBit = paste0("sQStarts",startsByYr,"Anthro",iA)
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("bse"),whichPlots=c("Adult female survival"))

sS=c(0,1,2)
numObsYrs=c(15);startsByYr = 45;J=1;cw=200;sQ=0.025;rQ=0.025
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,bse=bse,sS=sS,iA=iA)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=iA,survAnalysisMethod = "KaplanMeier")
addBit = paste0("sQStarts",startsByYr,"Anthro",iA,"low")
KSLow = subset(scResultsLow$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("bse","sS"),whichPlots=c("Adult female survival"),
                   useNational=F)

numObsYrs=c(15);startsByYr = 45;J=1;cw=200;sQ=0.975;rQ=0.975
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,bse=bse,sS=sS,iA=iA)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=iA,survAnalysisMethod = "KaplanMeier")
addBit = paste0("sQStarts",startsByYr,"Anthro",iA,"high")
KSHigh = subset(scResultsHigh$ksDists,(Year==2019)&(Parameter=="Adult female survival"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("bse","sS"),whichPlots=c("Adult female survival"),
                   survLow=0.8,useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","bse","sS","scn")
KSAnthro90 = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KSAnthro90,here::here(paste0("tabs/SurvKSAllAnthro90.csv")))


########################
#recruitment
#vary lre, sre, and srv in factorial array. Outcome of interest is KS distance
lre=seq(1,10,by=2);sre=0.46*seq(0.5,1.5,by=0.25);srv=seq(0.14,0.28,by=0.04)

#Looking for low KS distance from full range of input sims when given almost no info
numObsYrs=c(1);startsByYr = 1;J=1;cw=0
scns=expand.grid(P=numObsYrs,aSf=0,J=J,st=startsByYr,cw=cw,lre=lre,sre=sre,srv=srv)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2019)&(Parameter=="Recruitment"))
addBit = paste0("rQStarts",startsByYr)
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("lre","sre"),loopVars="srv",whichPlots=c("Recruitment"))

numObsYrs=c(15);startsByYr = 45;J=1;cw=startsByYr*3;sQ=0.025;rQ=0.025
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,lre=lre,sre=sre,srv=srv)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("rQStarts",startsByYr,"low")
KSLow = subset(scResultsLow$ksDists,(Year==2019)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("lre","sre"),loopVars="srv",
                   whichPlots=c("Recruitment"),useNational=F)

numObsYrs=c(15);startsByYr = 45;J=1;cw=startsByYr*3;sQ=0.975;rQ=0.975
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,lre=lre,sre=sre,srv=srv)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=0,survAnalysisMethod = "KaplanMeier")
addBit = paste0("rQStarts",startsByYr,"high")
KSHigh = subset(scResultsHigh$ksDists,(Year==2019)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("lre","sre"),loopVars="srv",
                   whichPlots=c("Recruitment"),useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","sre","lre","srv","scn")
KS = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KS,here::here(paste0("tabs/RecKSAll.csv")))

###############
#vary bse to get slope.
bre=seq(1,7,by=1.5);rS=c(1);iA=90

numObsYrs=c(1);startsByYr = 1;J=1;cw=0
scns=expand.grid(P=numObsYrs,aSf=0,J=J,st=startsByYr,cw=cw,bre=bre,rS=rS,iA=iA)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2019)&(Parameter=="Recruitment"))
addBit = paste0("rQStarts",startsByYr,"Anthro",iA)
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("bre"),whichPlots=c("Recruitment"))

rS=c(0,1,2)
numObsYrs=c(15);startsByYr = 45;J=1;cw=startsByYr*3;sQ=0.025;rQ=0.025
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,bre=bre,rS=rS,iA=iA)
scResultsLow = runScenario(scns,quants=c(0.025,0.025),Anthro=iA,survAnalysisMethod = "KaplanMeier")
addBit = paste0("rQStarts",startsByYr,"Anthro",iA,"low")
KSLow = subset(scResultsLow$ksDists,(Year==2019)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsLow,addBit=addBit,facetVars=c("bre","rS"),whichPlots=c("Recruitment"),
                   useNational=F)

numObsYrs=c(15);startsByYr = 45;J=1;cw=startsByYr*3;sQ=0.975;rQ=0.975
scns=expand.grid(P=numObsYrs,aSf=0,J=J,sQ=sQ,rQ=rQ,st=startsByYr,cw=cw,bre=bre,rS=rS,iA=iA)
scResultsHigh = runScenario(scns,quants=c(0.975,0.975),Anthro=iA,survAnalysisMethod = "KaplanMeier")
addBit = paste0("rQStarts",startsByYr,"Anthro",iA,"high")
KSHigh = subset(scResultsHigh$ksDists,(Year==2019)&(Parameter=="Recruitment"))
makeInterceptPlots(scResultsHigh,addBit=addBit,facetVars=c("bre","rS"),whichPlots=c("Recruitment"),
                   useNational=F)

KSAll$scn = "All"
KSLow$scn = "Low"
KSHigh$scn = "High"
sCols = c("KSDistance","bre","rS","scn")
KSAnthro90 = rbind(subset(KSAll,select=sCols),subset(KSLow,select=sCols),subset(KSHigh,select=sCols))
write.csv(KSAnthro90,here::here(paste0("tabs/RecKSAllAnthro90.csv")))
