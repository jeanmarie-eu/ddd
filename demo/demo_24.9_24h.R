rm(list=ls())

library(ddd)

fromPeriod     <- "2000090106"
toPeriod       <- "2014123106"
timeResolution <- "daily"
catchment      <- "Tingvatn"
pathData       <- paste0(.libPaths()[1],"/ddd/data/")
fileData       <- "val_24.9_24hptq_kal.txt"
pathParam      <- paste0(.libPaths()[1],"/ddd/data/")
fileParam      <- "best_par_24.9_24h.txt"
FIGURE         <- TRUE

pathResults <- normalizePath(file.path("~/",paste0("dddRes_",format(Sys.time(), "%Y-%m-%d-%H-%M",tz="GMT"))),mustWork = FALSE)

dir.create(pathResults, showWarnings = FALSE, recursive = TRUE)

ddd<- start(namefield="ddd",namespace="ddd",pathRes=pathResults)

timePeriod <- date(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod,format="YY,MM,DD,HH",saveDate=saveDate)

obs(ddd=ddd,pathPrecip=pathData,filenamePrecip=fileData,pathTemp=pathData,filenameTemp=fileData,pathQ=pathData,filenameQ=fileData)

param(ddd=ddd,method=methodParam,path=pathParam,filename=fileParam)

model(ddd=ddd,method=methodModel,Timeresinsec=timePeriod$Timeresinsec,inputParam=ddd$inputParam$values())

init(ddd=ddd,Timeresinsec=timePeriod$Timeresinsec,q1=ddd$Q$values()[1])

results <- simulation(ddd=ddd,timePeriod=timePeriod,saveDate=saveDate,pathRes=pathResults)

if (FIGURE) {
  graph.ts(dateTS = timePeriod$dateTS,
           precip = rowMeans(as.matrix(ddd$precipLZ$values()),na.rm=TRUE),
           q      = results$simulation[,8],
           q2     = results$simulation[,7])
}
