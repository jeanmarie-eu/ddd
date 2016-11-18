#ddd(fromPeriod="2000090106",toPeriod="2014123106",timeResolution="daily",catchment="Tingvatn",
#    pathData=paste0(.libPaths()[1],"/ddd/data/"),
#    fileData="val_24.9_24hptq_kal.txt",
#    pathParam=paste0(.libPaths()[1],"/ddd/data/"),
#    fileParam="best_par_24.9_24h.txt",
#    FIGURE=TRUE)


rm(list=ls())

library(ddd)

ddd<- start(namefield="ddd",namespace="ddd",pathRes="~/test/")

timePeriod <- date(timeResolution="daily",fromPeriod="2000090106",toPeriod="2014123106",format="YY,MM,DD,HH",saveDate=NULL)

pathData=paste0(.libPaths()[1],"/ddd/data/")
fileData="val_24.9_24hptq_kal.txt"

obs(ddd=ddd,pathPrecip=pathData,filenamePrecip=pathData)


temp <- as.matrix(tmp[,15:24])
q <- as.vector(tmp[,25])
missingValues <- -10000
q[q==missingValues] <- NA
scaob   <- NA

pathParam=paste0(.libPaths()[1],"/ddd/data/")
fileParam="best_par_24.9_24h.txt"
param(ddd=ddd,method="processedNVE",path=pathParam,filename=fileParam)

model(ddd=ddd,method="processedNVE",Timeresinsec=timePeriod$Timeresinsec,inputParam=ddd$inputParam$values())

init(ddd=ddd,Timeresinsec=timePeriod$Timeresinsec,q1=q[1])
