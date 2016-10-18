#' ddd.init.initialCondition
#'
#' The funtion \code{init.initialCondition()} initializes all the processess:
#'  i- snow (isoil,gisoil,spd,wcd,sca,nsno,alfa,ny,snowfree)
#'  ii- snow reservoir (snomag,swe_h,middelsca,snofritt)
#'  iii- soil moisture (waterSoil,waterGlacialSoil,waterGlaciers,Z)
#'  iv- soil water (G,X,Ea) (soil and bogs)
#'  v- soil discharge (D,qsimutx,qsimX)
#'  vi- ddistx, ddist, S
#'  vii- groundwater (Magkap, M, Layers)
#' @param initialCondition method for the initialization, "load", "source", "default"
#' @param path path of the directory where to find the two files
#' @param MAD value of the Mean Annual Discharge
#' @param q1 first value of the runoff timeserie
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param nbLevelZone number of level zone
#' @param NoL number of saturation layers
#' @param D volume of the unsaturated zone
#' @param UHriver Unit Hydrograp of the river
#' @param layerUH Unit Hydrograph of the saturation layers
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param modelArea list of parameters about the area
#'  list(totarea,slopesriverarea,nobognoglacarea,bogarea)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param,NoL)
#' @param modelRiver list of parameters about the river
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelBog list of parameters of the bog
#'  list(maxL,speed,nbStepsDelay,z,dist,param)
#' @param modelSaturation list of parameters of the saturation
#'  list(gtcel,CapacityUpperLevel=2000 ,mLam,varLam,distr)
#' @return The output is a list of the seven main blocks that will be process along the simulation:
#'  - snow
#'  - snowReservoir
#'  - soilMoisture
#'  - soilWater
#'  - soilDischarge
#'  - ddistAll
#'  - groundwater
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init.initialCondition()
#' }
init.initialCondition <-function(initialCondition,
                                     path=NULL,
                                     MAD=NULL,
                                     q1=NULL,
                                     Timeresinsec=NULL,
                                     nbLevelZone=NULL,
                                     NoL=NULL,
                                     D=NULL,
                                     UHriver=NULL,
                                     layerUH=NULL,
                                     UHMAD=NULL,
                                     modelArea=NULL,
                                     modelLayer=NULL,
                                     modelRiver=NULL,
                                     modelBog=NULL,
                                     modelSaturation=NULL){



  ######################################
  # Load rda files                     #
  ######################################
  initialCondition.load<-function(path) {

    load(paste0(path,"snow.rda"))
    load(paste0(path,"snowReservoir.rda"))
    load(paste0(path,"soilMoisture.rda"))
    load(paste0(path,"soilWater.rda"))
    load(paste0(path,"soilDischarge.rda"))
    load(paste0(path,"ddistAll.rda"))
    load(paste0(path,"groundwater.rda"))

    res <- list( snow = snow,
                 snowReservoir = snowReservoir,
                 soilMoisture = soilMoisture,
                 soilWater = soilWater,
                 soilDischarge = soilDischarge,
                 ddistAll = ddistAll,
                 groundwater = groundwater)
  }


  #######################################
  # Load a R source file                #
  #######################################
  initialCondition.source<-function(path) {

    source(paste0(path,"initialCondition.R"),local=TRUE)

    res <- list( snow = snow,
                 snowReservoir = snowReservoir,
                 soilMoisture = soilMoisture,
                 soilWater = soilWater,
                 soilDischarge = soilDischarge,
                 ddistAll = ddistAll,
                 groundwater = groundwater)
  }


  #############################
  # DEFAULT INITIAL CONDITION #
  #############################
  initialCondition.default<-function(MAD,q1,Timeresinsec,nbLevelZone,NoL,D,
                                     UHriver,layerUH,UHMAD,
                                     modelArea,modelLayer,modelRiver,modelBog,modelSaturation){

    if ( (!is.null(MAD)) && (!is.null(q1)) && (!is.null(Timeresinsec)) &&
         (!is.null(nbLevelZone)) && (!is.null(NoL)) && (!is.null(D)) &&
         (!is.null(UHriver)) && (!is.null(layerUH)) && (!is.null(UHMAD)) &&
         (!is.null(modelArea)) && (!is.null(modelLayer)) && (!is.null(modelRiver)) &&
         (!is.null(modelBog)) && (!is.null(modelSaturation))
         ) {

          # SNOW
          snow <- list(isoil=rep(0,nbLevelZone),        # isoil:
                       gisoil=rep(0,nbLevelZone),       # gisoil:
                       spd=rep(0,nbLevelZone),          # spd:    swe BV
                       wcd=rep(0,nbLevelZone),          # wcd:    fritt vann i snopakke BV
                       sca=rep(0,nbLevelZone),          # sca:    snow coverage
                       nsno=rep(0,nbLevelZone),         # nsno:   antall hendelser
                       alfa=rep(0,nbLevelZone),         # alfa:   alfa parameter i nedbor gamma fordeling
                       ny=rep(0,nbLevelZone),           # ny:     ny parameter i nedbor gamma fordeling
                       snowfree = 0)

          # parameters to be putted into the parameter files
          # UP   # DEFAULT: 0


          # SNOW RESERVOIR
          snowReservoir <- list(snomag       = 0,
                                swe_h        = 0,
                                middelsca    = 0,
                                snofritt     = 0)


          # SOIL MOISURE
          soilMoisture <- list(waterSoil        = 0,
                               waterGlacialSoil = 0,
                               waterGlaciers    = 0,
                               Z                = 0)


          # SOIL DISCHARGE: SLOPES AND BOGS
          soilDischarge <- init.soilDischarge(MAD=MAD,
                                                  q1=q1,
                                                  D = D,
                                                  Timeresinsec=Timeresinsec,
                                                  modelArea=modelArea,
                                                  modelLayer=modelLayer,
                                                  modelRiver=modelRiver,
                                                  modelBog=modelBog,
                                                  layerUH=layerUH,
                                                  NoL=NoL,
                                                  UHriver=UHriver)
          # OUTPUT (list)
          # - D: moisture in mm, derived from the approx. mean annual dicharge (MAD)
          # - qsimutX: Discharge with the contributions from both slopes and bogs (in m3/s) )
          # - qsimX:


        # SATURATION LAYER
         ddistAll <- list( S      = (-1)*D,     # dD/dt = -dS/dt
                           ddistx = NULL,                       # ddistx:
                           ddist  = rep(1/NoL,NoL) )             # ddist:


         # GROUNDWATER ZONE (saturated zone with volume S)
         groundwater <- init.groundWater(Timeresinsec=Timeresinsec,
                                            UHMAD=UHMAD,
                                            MAD=MAD,
                                            area=modelArea$totarea,
                                            modelSaturation=modelSaturation,
                                            modelLayer=modelLayer)
         # OUTPUT (list)
         # - Magkap
         # - M: Groundwater Storage Capacity (GSC)
         # - Layers: Layers of saturation


         # RECEIVED SOIL MOISTURE (from precipitation and melting snow and bogs)
         soilWater <- list(Ea      = NULL,                       # actual evapotranspiration on soil
                           G       = 0.2*groundwater$M,          # received soilmoisture from precipitation and snowmel
                           X       = NULL,                       # excess water
                           Eabog   = NULL,                       # actual evapotranspiration on bogs
                           Gbog    = 0.95*groundwater$M,         # received soilmoisture from precipitation and snowmel from bogs
                           Xbog    = NULL)                       # excess water


          res <- list( snow = snow,
                       snowReservoir = snowReservoir,
                       soilMoisture = soilMoisture,
                       soilWater = soilWater,
                       soilDischarge = soilDischarge,
                       ddistAll = ddistAll,
                       groundwater = groundwater)


    } else stop("NULL arguments in parameters")



   return(res)

  }



   #############################
   # DIFFERENT CHOICES         #
   #############################
   res <- switch(initialCondition,
     "default"   = initialCondition.default(MAD=MAD,q1=q1,Timeresinsec=Timeresinsec,nbLevelZone=nbLevelZone,NoL=NoL,D=D,UHriver=UHriver,layerUH=layerUH,UHMAD=UHMAD,modelArea=modelArea,modelLayer=modelLayer,modelRiver=modelRiver,modelBog=modelBog,modelSaturation=modelSaturation),
     "load"      = initialCondition.load(path=path),
     "source"    = initialCondition.source(path=path),
     (message=paste0("Invalid method of initial condition:", initialCondition,".")))

   return(res)

}
