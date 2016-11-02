
 #################################################################################################
 # Purpose of this script:                                                                       #
 # Run the hydrological model DDD (Distance Distribution Dynamics).                              #
 # For a specific period of time, and a specific catachement                                     #
 # PROCESS: For every timesteps                                                                  #
 #          A- UPDATE SNOW: INPUT OF RAIN, SNOWMELT AND SNOW                                     #
 #          B- UPDATE WATER VOLUME IN SOIL MOISTURE: Z(t)                                        #
 #          C- UPDATE SOIL WATER: EA(t), G(t), X(t) for soil and bogs                            #
 #          D- UPDATE ddistAll: ddistx, ddist and S                                              #
 #          E- UPDATE SOIL DISCHARGE: D(t), qsimut                                               #
 #          F- UPDATE GROUNDWATER: MAGKAP,M,LAYERS                                               #
 #          G- SAVE THE SIMULATION                                                               #
 # OUTPUT (list)                                                                                 #
 # - simulation                                                                                  #
 # - Layers                                                                                      #
 # SAVED FILES:                                                                                  #
 # - csv-files of the output are located at the path related to pathResults                      #
 # - the whole variables of the specific timestep are located                                    #
 #   in the folder named as the selected date. rda-files are:                                    #
 #    snow,soilMoisture,soilWater,soilDischarge,ddistAll,groundwater                             #
 #################################################################################################

 #################################################################################################
 # WARNING:
 # The aim of this script was to compare the output of this simulation
 # with the one produced by using the script of Thomas Skaugen (TS)
 # We are not expecting the output to be realistic. Indeed:
 # 1-The parameters of the model are calibrated for daily runoffs simulation.
 # 2-Our simulation run on a hourly timestep
 # 3-Some parameters provided by TS were settle to zero
 # 4-The precipitation and temperature variables come from one station close but out of the catchemnt
 #################################################################################################


rm(list=ls())
library(ddd)


###################################################################################################
###################################################################################################
## MAIN PARAMETERS                                                                               ##
## THESE ARE THE PARAMETERS THAT DRIVE THE ENTIRE SCRIPT                                         ##
## - period of simulation and time resolution                                                    ##
## - catchment of interest                                                                       ##
## - how to get the ddd model (method and paths)                                                 ##
## - method of the initial condition                                                             ##
## - path of the results (simulation and layers timeseries)                                      ##
## - date when the saving of all the variables is requested                                      ##
##   rem: the output of the simulation (simulation and layers) is always saved                   ##
###################################################################################################
###################################################################################################

# PERIOD OF SIMULATION
timeResolution="hourly" #"daily", "three-hourly", "hourly",...
fromPeriod="2016080200"
toPeriod="2016092923"

# WARNING:
# So far the timeResolution and the preiod of simulation have to match
# the ones from the observations

# CATCHMENT OF INTEREST
catchment <- "Narsjo"
# WARNING:
# So far this variable do not affect anything

# PATHS OF THE METEORLOGICAL AND HYDROLOGICAL VARIABLES
pathPrecip    <- paste0(.libPaths()[1],"/ddd/data/")
pathTemp      <- paste0(.libPaths()[1],"/ddd/data/")
pathDischarge <- paste0(.libPaths()[1],"/ddd/data/")

# PARAMETERS AND MODELS
# How to get the models parameters
methodParam <- "processedNVE"
pathParam <- paste0(.libPaths()[1],"/dddModel/data/")
fileParam <- "paramNVE.txt"

# How to get the model Parameters
methodModel <- "processedNVE"


# INITIAL CONDITIONS
MAD_ci <- 2.48                   # Mean annual discharge, Measured
D_ci <-2                         # Represents the potential volume of water that is needed for complete saturation

# if 1: update from sattelite derived SCA; if 0: do not
#UP <- 0 frozen to 0


# PATH RESULTS
pathResults   <- "~/"
pathResults   <- paste0(pathResults,"dddRes_",format(Sys.time(), "%Y-%m-%d-%H-%M",tz="GMT"),"/")
dir.create(pathResults, showWarnings = FALSE)

# DATE TO BE SAVED
saveDate <- "2016082318"



###################################################################################################
###################################################################################################
## INFORMATION RELATED TO THE TIME                                                               ##
## - Time resolution in seconds                                                                  ##
## - nbStep                                                                                      ##
## - date timeserie                                                                              ##
## - indice of the timestep to be saved                                                          ##
###################################################################################################
###################################################################################################
timePeriod <- ddd::date(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod,format="YY,MM,DD,HH",saveDate=saveDate)
# OUTPUT (list)
# Timeresinsec
# nbStep
# seqPeriod: date format POSIXct
# dateTS
# indiceSave



###################################################################################################
###################################################################################################
## GET METEOROLOGICAL AND HYDROLOGICAL VARIABLES                                                 ##
## - Precipitation Timeserie                                                                     ##
## - Temperature Timeserie                                                                       ##
## - Runoff Timeserie                                                                            ##
## - Snow Coverage Timeserie observed by satellite                                               ##
###################################################################################################
###################################################################################################

# WARNING:
# This part has to be improved in order to make the process nicer and more automatic
# So far the stations and the catchement are not dependant from each other
precAndTemp <- read.csv(paste0(pathPrecip,"station8880.csv"),header=TRUE)
whAndq <- read.csv(paste0(pathDischarge,"dischargeNarsjo.csv"),header=TRUE)
precAndTemp[precAndTemp==-999]<-NA
whAndq[whAndq==-999]<-NA

# precipitation
precip <- precAndTemp[,5]

# temperature
temp <- precAndTemp[,6]

# runoff
q <- whAndq[,6]

# WARNING:
# Snow coverage from satellite is not yet taken into account
scaob   <- NA
#scaobx <- -9999
#dag <- as.Date(paste(ptqinn$yr[i],"-",ptqinn$mnt[i],"-",ptqinn$day[i],sep=""))
#datoform <- format(dag,"%Y.%m.%d")
#if(length(which(scaob$dagsdato == datoform)) > 0){
#  target <- which(scaob$dagsdato == datoform)
#  if(scaob[target,3] < 5.0){
#    scaobx <- scaob[target,idim+3]/100
#  }
#}



###################################################################################################
###################################################################################################
## GET THE MODEL OF ddd WITH ALL THE CHOSEN PARAMETERS                                           ##
## THE MODEL OF ddd GATHER SEVERAL MODELS                                                        ##
##     - modelk                                                                                  ##
##     - modelSoilMoisture                                                                       ##
##     - modelSoilWater                                                                          ##
##     - modelSoil                                                                               ##
##     - modelSaturation                                                                         ##
##     - modelLayer                                                                              ##
##     - modelRiver                                                                              ##
##     - modelBog                                                                                ##
##     - modelMAD                                                                                ##
##     - modelSnow                                                                               ##
##     - modelTempLZ                                                                             ##
##     - modelPrecipLZ                                                                           ##
##     - modelArea                                                                               ##
###################################################################################################
###################################################################################################

# GET PARAMETERS
inputParam <- ddd::getParam(method=methodParam,path=pathParam, filename=fileParam,SAVE=TRUE,pathResults=pathResults)

# GET MODELS
models <- ddd::getModel(method=methodModel,inputParam=inputParam,Timeresinsec=timePeriod$Timeresinsec,SAVE=TRUE,pathResults=pathResults)

# OUTPUT: (list)
#     - modelk
#     - modelSoilMoisture
#     - modelSoilWater
#     - modelSoil
#     - modelSaturation
#     - modelLayer
#     - modelRiver
#     - modelBog
#     - modelMAD
#     - modelSnow
#     - modelTempLZ
#     - modelPrecipLZ
#     - modelArea


###################################################################################################
###################################################################################################
## SIMULATION INITIALIZING: Initial conditions                                                   ##
## PROCESS:                                                                                      ##
##          A- UH: get UH river, UH Layer (saturation layers) and UH MAD                         ##
##          B- snow (isoil,gisoil,spd,wcd,sca,nsno,alfa,ny,snowfree)                             ##
##          C- snow reservoir (snomag,swe_h,middelsca,snofritt)                                  ##
##          D- soil moisture (waterSoil,waterGlacialSoil,waterGlaciers,Z)                        ##
##          E- soil discharge (D,qsimutX,qsimX                                                   ##
##          F- soil water (G,X,Ea) (soil and bogs)                                               ##
##          G- soil discharge (D,qsimutx,qsimX)                                                  ##
##          H- ddistx, ddist, S                                                                  ##
##          I- groundwater (Magkap, M, Layers)                                                   ##
## - UH                                                                                          ##
## - snow                                                                                        ##
## - snowReservoire                                                                              ##
## - soilMoisture                                                                                ##
## - soilDischarge                                                                               ##
## - soilWater                                                                                   ##
## - ddistAll                                                                                    ##
## - groundwater                                                                                 ##
###################################################################################################
###################################################################################################

# A-UH
UH <- ddd::init.UH(method="processed",Timeresinsec=timePeriod$Timeresinsec,modelLayer=models$modelLayer,modelRiver=models$modelRiver,modelMAD=models$modelMAD)
# OUTPUT (list)
# - UHriver
# - layerUH
# - UHMAD

# B-SNOW
snow <- ddd::init.snow(method="manual",
                  isoil=rep(0,models$modelPrecipLZ$nbLevelZone),gisoil=rep(0,models$modelPrecipLZ$nbLevelZone),
                  spd=rep(0,models$modelPrecipLZ$nbLevelZone),wcd=rep(0,models$modelPrecipLZ$nbLevelZone),
                  sca=rep(0,models$modelPrecipLZ$nbLevelZone),nsno=rep(0,models$modelPrecipLZ$nbLevelZone),
                  alfa=rep(0,models$modelPrecipLZ$nbLevelZone),ny=rep(0,models$modelPrecipLZ$nbLevelZone),snowfree=0)
# parameters to be putted into the parameter files
# UP   # DEFAULT: 0
# OUTPUT (list)
# - isoil
# - gisoil
# - spd
# - wcd
# - sca
# - nsno
# - alfa
# - ny
# - snowfree

# C-SNOW RESERVOIR
snowReservoir <-ddd::init.snowReservoir(method="manual",snomag=0,swe_h=0,middelsca=0,snofritt=0)
# OUTPUT (list)
# - snomag
# - swe_h
# - middelsca
# - snofritt

# D-SOIL MOISURE
soilMoisture <- ddd::init.soilMoisture(method="manual",waterSoil=0,waterGlaciatedSoil=0,waterGlaciers=0,Z=0)
# OUTPUT (list)
# - waterSoil
# - waterGlaciatedSoil
# - waterGlaciers
# - Z

# E- SOIL DISCHARGE: SLOPES AND BOGS
soilDischarge <- ddd::init.soilDischarge(method="processed",
                                    MAD=models$modelSoilDischarge$MAD,
                                    q1=q[1],
                                    D=D_ci,
                                    Timeresinsec=timePeriod$Timeresinsec,
                                    modelArea=models$modelArea,
                                    modelLayer=models$modelLayer,
                                    modelRiver=models$modelRiver,
                                    modelBog=models$modelBog,
                                    layerUH=UH$layerUH,
                                    UHriver=UH$UHriver)
# OUTPUT (list)
# - D: moisture in mm, derived from the approx. mean annual dicharge (MAD)
# - qsimutX: Discharge with the contributions from both slopes and bogs (in m3/s) )
# - qsimX:


# F- SATURATION LAYER
ddistAll <- ddd::init.ddistAll(method= "manual",
                           S     = (-1)*D_ci,   # dD/dt = -dS/dt
                           ddistx = NULL,
                           ddist  = rep(1/models$modelLayer$NoL,models$modelLayer$NoL) )


# G- GROUNDWATER ZONE (saturated zone with volume S)
groundwater <- ddd::init.groundwater(method="processed",
                                 Timeresinsec=timePeriod$Timeresinsec,
                                 UHMAD=UH$UHMAD,
                                 MAD=MAD_ci,
                                 modelArea=models$modelArea,
                                 modelSaturation=models$modelSaturation,
                                 modelLayer=models$modelLayer)
# OUTPUT (list)
# - Magkap
# - M: Groundwater Storage Capacity (GSC)
# - Layers: Layers of saturation


# I- RECEIVED SOIL MOISTURE (from precipitation and melting snow and bogs)
soilWater <- ddd::init.soilWater(method="manual",
                             Ea    = NULL,
                             G     = 0.2*groundwater$M,
                             X     = NULL,
                             Eabog = NULL,
                             Gbog  = 0.95*groundwater$M,
                             Xbog  = NULL)
# OUTPUT (list)
# - Ea, actual evapotranspiration on soil
# - G, received soilmoisture from precipitation and snowmel
# - X, excess water
# - Eabog, actual evapotranspiration on bogs
# - Gbog, received soilmoisture from precipitation and snowmel from bogs
# - Xbog, excess water



###################################################################################################
###################################################################################################
## SIMULATION                                                                                    ##
## PROCESS: For every timesteps                                                                  ##
##          A- UPDATE SNOW: INPUT OF RAIN, SNOWMELT AND SNOW                                     ##
##          B- UPDATE WATER VOLUME IN SOIL MOISTURE: Z(t)                                        ##
##          C- UPDATE SOIL WATER: EA(t), G(t), X(t) for soil and bogs                            ##
##          D- UPDATE SOIL DISCHARGE: D(t), qsimut                                               ##
##          E- UPDATE GROUNDWATER: MAGKAP,M,LAYERS                                               ##
##          F- UPDATE ddistAll: ddist, ddisx, S                                                  ##
##          G- SAVE THE SIMULATION                                                               ##
## OUTPUT (list)                                                                                 ##
## - simulation                                                                                  ##
## - Layers                                                                                      ##
## SAVED FILES:                                                                                  ##
## - csv-files of the output are located at the path related to pathResults                      ##
## - the whole variables of the specific timestep are located                                    ##
##   in the folder named as the selected date. rda-files are:                                    ##
##    snow,soilMoisture,soilWater,soilDischarge,ddistAll,groundwater                             ##
###################################################################################################
###################################################################################################
results <- ddd::do.simTS(timePeriod      = timePeriod,
                         q               = q,
                         precip          = precip,
                         temp            = temp,
                         scaob           = scaob,
                         UH              = UH,
                         snow            = snow,
                         snowReservoir   = snowReservoir,
                         soilMoisture    = soilMoisture,
                         soilWater       = soilWater,
                         soilDischarge   = soilDischarge,
                         ddistAll        = ddistAll,
                         groundwater     = groundwater,
                         models          = models,
                         pathResults     = pathResults,
                         saveDate        = saveDate)



###################################################################################################
###################################################################################################
## GRAPHE                                                                                        ##
## Timeserie of the precipitation (blue)                                                         ##
## Timeserie of the simulated runoff (black)                                                     ##
## Timeserie of the observed runoff (red):                                                       ##
###################################################################################################
###################################################################################################
ddd::graph.ts(dateTS = timePeriod$dateTS,
            precip = precip,
            q      = results$simulation[,8],
            q2     = results$simulation[,7])
