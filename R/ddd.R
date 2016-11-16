#' ddd general simulation
#'
#' General simulation of the hydrological model
#' @param fromPeriod "YYYYmmddHH"
#' @param toPeriod "YYYYmmddHH"
#' @param timeResolution "daily", "three-hourly", "hourly"
#' @param catchment name of the catchement. Not in used yet
#' @param pathData path of the data file (ptq)
#' @param fileData name of the data file (ptq)
#' @param pathParam path of the parameters file
#' @param fileParam name of the parameters file (ptq)
#' @param methodParam method how to get the param. Default is "processedNVE"
#' @param methodModel method how to get the model. Default is "processedNVE"
#' @param pathResults path of the results. Default is HOME
#' @param D_ci initial condition for the potential volume of water that is needed for complete saturation. Default is 2
#' @param saveDate full data of this date will be saved
#' @param FIGURE Boolean to plot a basic plot. Default is FALSE
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' ddd()
#' }

ddd <- function(fromPeriod=NULL,toPeriod=NULL,timeResolution=NULL,catchment="catchment",pathData=NULL,fileData=NULL,pathParam=NULL,fileParam=NULL,methodParam="processedNVE",methodModel="processedNVE",pathResults="~/",D_ci=2,saveDate=NULL,FIGURE=FALSE){

  if ( (is.null(fromPeriod)) &&
       (is.null(toPeriod)) &&
       (is.null(timeResolution)) &&
       (is.null(pathData)) &&
       (is.null(fileData)) &&
       (is.null(pathParam)) &&
       (is.null(fileParam)) ) {
         stop("Error: arguments 'fromPeriod','toPeriod','timeResolution','pathData','fileData','pathParam','fielParam' must be provided")
  } else {

     pathResults <- normalizePath(file.path(pathResults),mustWork = FALSE)
     dir.create(pathResults, showWarnings = FALSE, recursive = TRUE)

     ###################################################################################################
     ###################################################################################################
     ## INFORMATION RELATED TO THE TIME                                                               ##
     ## - Time resolution in seconds                                                                  ##
     ## - nbStep                                                                                      ##
     ## - date timeserie                                                                              ##
     ## - indice of the timestep to be saved                                                          ##
     ###################################################################################################
     ###################################################################################################
     timePeriod <- date(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod,format="YY,MM,DD,HH",saveDate=saveDate)
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
     pathData <- normalizePath(file.path(pathData),mustWork = FALSE)
     tmp <- utils::read.table(normalizePath(file.path(pathData,fileData),mustWork = FALSE),sep="\t")

     # precip
     precip <- as.matrix(tmp[,5:14])

     # temp
     temp <- as.matrix(tmp[,15:24])

     #runoff
     q <- as.vector(tmp[,25])
     missingValues <- -10000
     q[q==missingValues] <- NA

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
     inputParam <- getParam(method=methodParam,path=pathParam, filename=fileParam,SAVE=TRUE,pathResults=pathResults)

     # GET MODELS
     models <- getModel(method=methodModel,inputParam=inputParam,Timeresinsec=timePeriod$Timeresinsec,SAVE=TRUE,pathResults=pathResults)

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
     UH <- init.UH(method="processed",Timeresinsec=timePeriod$Timeresinsec,modelLayer=models$modelLayer,modelRiver=models$modelRiver,modelMAD=models$modelMAD)
     # OUTPUT (list)
     # - UHriver
     # - layerUH
     # - UHMAD

     # B-SNOW
     snow <- init.snow(method="manual",
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
     snowReservoir <-init.snowReservoir(method="manual",snomag=0,swe_h=0,middelsca=0,snofritt=0)
     # OUTPUT (list)
     # - snomag
     # - swe_h
     # - middelsca
     # - snofritt

     # D-SOIL MOISURE
     soilMoisture <- init.soilMoisture(method="manual",waterSoil=0,waterGlaciatedSoil=0,waterGlaciers=0,Z=0)
     # OUTPUT (list)
     # - waterSoil
     # - waterGlaciatedSoil
     # - waterGlaciers
     # - Z

     # E- SOIL DISCHARGE: SLOPES AND BOGS
     soilDischarge <- init.soilDischarge(method="processed",
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
     ddistAll <- init.ddistAll(method= "manual",
                                S     = (-1)*D_ci,   # dD/dt = -dS/dt
                                ddistx = NULL,
                                ddist  = rep(1/models$modelLayer$NoL,models$modelLayer$NoL) )


     # G- GROUNDWATER ZONE (saturated zone with volume S)
     groundwater <- init.groundwater(method="processed",
                                      Timeresinsec=timePeriod$Timeresinsec,
                                      UHMAD=UH$UHMAD,
                                      MAD=models$modelSoilDischarge$MAD,
                                      modelArea=models$modelArea,
                                      modelSaturation=models$modelSaturation,
                                      modelLayer=models$modelLayer)
     # OUTPUT (list)
     # - Magkap
     # - M: Groundwater Storage Capacity (GSC)
     # - Layers: Layers of saturation


     # I- RECEIVED SOIL MOISTURE (from precipitation and melting snow and bogs)
     soilWater <- init.soilWater(method="manual",
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
     results <- do.simTSv2(timePeriod      = timePeriod,
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
     if (FIGURE) {
       graph.ts(dateTS = timePeriod$dateTS,
                   precip = rowMeans(precip,na.rm=TRUE),
                   q      = results$simulation[,8],
                   q2     = results$simulation[,7])

     }

  }

  return(TRUE)


}
