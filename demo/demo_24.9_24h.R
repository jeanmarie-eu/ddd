rm(list=ls())

library(ddd)

fromPeriod     <- "2000090106"
toPeriod       <- "2014123106"
timeResolution <- "daily"
catchment      <- "Tingvatn"
pathData       <- paste0(.libPaths()[1],"/ddd/data/")
fileData       <- "val_24.9_24hptq_kal.txt"
methodParam    <- "processedNVE"
pathParam      <- paste0(.libPaths()[1],"/ddd/data/")
fileParam      <- "best_par_24.9_24h.txt"
methodModel    <- "processedNVE"
saveDate       <- "2013042406"
FIGURE         <- TRUE

pathResults <- normalizePath(file.path("~/",paste0("dddRes_",format(Sys.time(), "%Y-%m-%d-%H-%M",tz="GMT"))),mustWork = FALSE)

dir.create(pathResults, showWarnings = FALSE, recursive = TRUE)

ddd<- ddd::start(namefield="ddd",namespace="ddd",pathRes=pathResults)

timePeriod <- ddd::date(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod,format="YY,MM,DD,HH",saveDate=saveDate)

ddd::obs(ddd=ddd,pathPrecip=pathData,filenamePrecip=fileData,pathTemp=pathData,filenameTemp=fileData,pathQ=pathData,filenameQ=fileData)

ddd::param(ddd=ddd,method=methodParam,path=pathParam,filename=fileParam)

ddd::model(ddd=ddd,method=methodModel,Timeresinsec=timePeriod$Timeresinsec,inputParam=ddd$inputParam$values())

ddd::init(ddd=ddd,Timeresinsec=timePeriod$Timeresinsec,q1=ddd$Q$values()[1])

results <- ddd::simulation(ddd=ddd,timePeriod=timePeriod,saveDate=saveDate,pathRes=pathResults)

if (FIGURE) {
  graph.ts(dateTS = timePeriod$dateTS,
           precip = rowMeans(as.matrix(ddd$precipLZ$values()),na.rm=TRUE),
           q      = results$simulation[,8],
           q2     = results$simulation[,7])
}
