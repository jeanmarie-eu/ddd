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
pathData <- normalizePath(file.path(pathData),mustWork = FALSE)
tmp <- utils::read.table(normalizePath(file.path(pathData,fileData),mustWork = FALSE),sep="\t")
precip <- as.matrix(tmp[,5:14])
temp <- as.matrix(tmp[,15:24])
q <- as.vector(tmp[,25])
missingValues <- -10000
q[q==missingValues] <- NA
scaob   <- NA

pathParam=paste0(.libPaths()[1],"/ddd/data/")
fileParam="best_par_24.9_24h.txt"
param(ddd=ddd,method="processedNVE",path=pathParam,filename=fileParam)

model(ddd=ddd,method="processedNVE",Timeresinsec=timePeriod$Timeresinsec,inputParam=ddd$inputParam$values())

#init(Timeresinsec=timePeriod$Timeresinsec,ddd=ddd,q1=q[1])

Timeresinsec <- timePeriod$Timeresinsec
q1 <- q[1]

  D_ci <- 2

  # A-UH
  ddd$uh$do("init.UH",args=list(method="processed",
                                Timeresinsec=Timeresinsec,
                                modelLayer=ddd$model$values()$modelLayer,
                                modelRiver=ddd$model$values()$modelRiver,
                                modelMAD=ddd$model$values()$modelMAD))
  ddd$uh$save(name="init")

  # B-SNOW
  ddd$snow$do("init.snow",args=list(method="manual",
                                    isoil=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),gisoil=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),
                                    spd=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),wcd=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),
                                    sca=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),nsno=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),
                                    alfa=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),ny=rep(0,ddd$model$values()$modelPrecipLZ$nbLevelZone),snowfree=0))
  ddd$snow$save(name="init")


  # C-SNOW RESERVOIR
  ddd$snowReservoir$do("init.snowReservoir",args=list(method="manual",snomag=0,swe_h=0,middelsca=0,snofritt=0))
  ddd$snowReservoir$save(name="init")


  # D-SOIL MOISURE
  ddd$soilMoisture$do("init.soilMoisture",args=list(method="manual",waterSoil=0,waterGlaciatedSoil=0,waterGlaciers=0,Z=0))
  ddd$soilMoisture$save(name="init")


  # F- SATURATION LAYER
  ddd$ddistAll$do("init.ddistAll",args=list(method= "manual",
                                            S     = (-1)*D_ci,   # dD/dt = -dS/dt
                                            ddistx = NULL,
                                            ddist  = rep(1/ddd$model$values()$modelLayer$NoL,ddd$model$values()$modelLayer$NoL) ))
  ddd$ddistAll$save(name="init")


  # E- SOIL DISCHARGE: SLOPES AND BOGS
  ddd$soilDischarge$do("init.soilDischarge",args=list(method="processed",
                                                     MAD=ddd$model$values()$modelSoilDischarge$MAD,
                                                     q1=q1,
                                                     D=D_ci,
                                                     Timeresinsec=Timeresinsec,
                                                     modelArea=ddd$model$values()$modelArea,
                                                     modelLayer=ddd$model$values()$modelLayer,
                                                     modelRiver=ddd$model$values()$modelRiver,
                                                     modelBog=ddd$model$values()$modelBog,
                                                     layerUH=ddd$uh$values()$layerUH,
                                                     ddistAll = ddd$ddistAll$values(),
                                                     UHriver=ddd$uh$values()$UHriver))
  ddd$soilDischarge$save(name="init")


  # G- GROUNDWATER ZONE (saturated zone with volume S)
  ddd$groundwater$do("init.groundwater",args=list(method="processed",
                                                  Timeresinsec=Timeresinsec,
                                                  UHMAD=ddd$uh$values()$UHMAD,
                                                  MAD=ddd$model$values()$modelSoilDischarge$MAD,
                                                  modelArea=ddd$model$values()$modelArea,
                                                  modelSaturation=ddd$model$values()$modelSaturation,
                                                  modelLayer=ddd$model$values()$modelLayer))
  ddd$groundwater$save(name="init")


  # I- RECEIVED SOIL MOISTURE (from precipitation and melting snow and bogs)
  ddd$soilWater$do("init.soilWater",args=list(method="manual",
                                              Ea    = NULL,
                                              G     = 0.2*ddd$groundwater$values()$M,
                                              X     = NULL,
                                              Eabog = NULL,
                                              Gbog  = 0.95*ddd$groundwater$values()$M,
                                              Xbog  = NULL))
  ddd$soilWater$save(name="init")
