rm(list=ls())

library(ddd)

main(fromPeriod="2000090106",
     toPeriod="2014123106",
     timeResolution="daily",
     catchment="Tingvatn",
     pathData=paste0(.libPaths()[1],"/ddd/data/"),
     fileData="val_24.9_24hptq_kal.txt",
     pathParam=paste0(.libPaths()[1],"/ddd/data/"),
     fileParam="best_par_24.9_24h.txt",
     FIGURE=TRUE)
