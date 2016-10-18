
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
fromPeriod="20160802"
toPeriod="20160929"
timeResolution="hourly" #"daily", "three-hourly", "hourly",...
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

# MODEL of ddd: HOW TO GET IT.
getModel <- "build" # "load", "source"
pathModel <- paste0(.libPaths()[1],"/dddModel/data/") # path for either the parameters or the already built model

# INITIAL CONDITIONS
initialCondition <- "default" # "load", "source"
pathInitCond  <- NULL         # if load or source, then a patth is needed
initCondMAD <- 2.48                   # Mean annual discharge, Measured
initCondD <-2                         # Represents the potential volume of water that is needed for complete saturation

# PATH RESULTS
#pathResults   <- "/your/path/"
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

# GET MODELS
models <- ddd::init.getModel(getModel=getModel,path=pathModel,Timeresinsec=timePeriod$Timeresinsec,SAVE=TRUE,pathResults=pathResults)
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
## SIMULATION INITIALIZING:                                                                      ##
## PROCESS:                                                                                      ##
##          A- init.UH: get UH river, UH Layer (saturation layers) and UH MAD                    ##
##          B- get or process the initial conditions for:                                        ##
##            i- snow (isoil,gisoil,spd,wcd,sca,nsno,alfa,ny,snowfree)                           ##
##            ii- snow reservoir (snomag,swe_h,middelsca,snofritt)                               ##
##            iii- soil moisture (waterSoil,waterGlacialSoil,waterGlaciers,Z)                    ##
##            iv- soil water (G,X,Ea) (soil and bogs)                                            ##
##            iv- soil discharge (D,qsimutx,qsimX)                                               ##
##            v- ddistx, ddist, S                                                                ##
##            vi- groundwater (Magkap, M, Layers)                                                ##
## OUTPUT (list)                                                                                 ##
## - snow                                                                                        ##
## - snowReservoire                                                                              ##
## - soilMoisture                                                                                ##
## - soilWater                                                                                   ##
## - soilDischarge                                                                               ##
## - ddistAll                                                                                    ##
## - groundwater                                                                                 ##
###################################################################################################
###################################################################################################

# A- init UH River, UH Layer and UH MAD
UH <- ddd::init.UH(Timeresinsec=timePeriod$Timeresinsec,modelLayer=models$modelLayer,modelRiver=models$modelRiver,modelMAD=models$modelMAD)
# OUTPUT (list)
# - UHriver
# - layerUH
# - UHMAD

# B- Iniital condition, with default method
initCond <- ddd::init.initialCondition(initialCondition="default",
                          path=pathInitCond,
                          MAD=initCondMAD,
                          q1=q[1],
                          Timeresinsec=timePeriod$Timeresinsec,
                          nbLevelZone=models$modelPrecipLZ$nbLevelZone,
                          NoL=models$modelLayer$NoL,
                          D=initCondD,
                          UHriver=UH$UHriver,
                          layerUH=UH$layerUH,
                          UHMAD=UH$UHMAD,
                          modelArea=models$modelArea,
                          modelLayer=models$modelLayer,
                          modelRiver=models$modelRiver,
                          modelBog=models$modelBog,
                          modelSaturation=models$modelSaturation)
# OUTPUT (list)
# - snow,
# - snowReservoir
# - soilMoisture
# - soilWater
# - soilDischarge
# - ddistAll
# - groundwater


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
                     snow            = initCond$snow,
                     snowReservoir   = initCond$snowReservoir,
                     soilMoisture    = initCond$soilMoisture,
                     soilWater       = initCond$soilWater,
                     soilDischarge   = initCond$soilDischarge,
                     ddistAll        = initCond$ddistAll,
                     groundwater     = initCond$groundwater,
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
