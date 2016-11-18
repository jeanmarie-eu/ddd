#' State of the hydrological processes
#'
#' The function does the following:
#'  -state of Snow
#' - state of water volume in Soil Moisture: Z(t)
#' - state of Soil Water: EA(t), G(t), X(t) for soil and bogs
#' - state of Soil Discharge: D(t), runoff
#' - state of ddistAll: ddist, ddisx, S
#' - state of Groundwater: MAGKAP,M,Saturation Layers
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param precipX precipitation value
#' @param tempX temperature value
#' @param scaobX observed snow coverage
#' @param snowX snow
#' @param soilMoistureX soilMoisture
#' @param snowReservoirX snowReservoir
#' @param soilWaterX soilWater
#' @param UH UH
#' @param ddistAllX ddistAll
#' @param soilDischargeX soilDischarge
#' @param groundwaterX groundwater
#' @param modelPrecipLZ list of parameters about PrecipLZ
#'  list(nbLevelZone,Plr,hfelt,midmetp)
#' @param modelTempLZ list of parameters about TempLZ
#'  list(nbLevelZone,Tlr,hfelt,midmett)
#' @param modelSnow list of parameters about Snow
#'  list(nbLevelZone,unitsnow,n0,Ws,TS,CX,CFR,CGLAC,gca,UP)
#' @param modelSoilMoisture list of parameters about soilMoisture
#'  list(gtcel,CapacityUpperLevel=2000,mLam,varLam,distr)
#' @param modelSoil list of parameters about Soil
#'  list(glacfrac)
#' @param modelBog list of parameters about Bog
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelET list of parameters about Evapotranspiration
#'  list(cea)
#' @param modelSoilWater list of parameters about Soil water
#'  list(R)
#' @param modelArea list of parameters about Area
#' list(totarea,slopesriverarea,nobognoglacarea,bogarea)
#' @param modelLayer list of parameters about Layers
#' list(maxL,speed,nbStepsDelay,z,distr,param,NoL)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.stateX()
#' }
do.stateX <-function(Timeresinsec,
                     precipX,
                     tempX,
                     scaobX,
                     snowX,
                     soilMoistureX,
                     snowReservoirX,
                     soilWaterX,
                     UH,
                     ddistAllX,
                     soilDischargeX,
                     groundwaterX,
                     modelPrecipLZ,
                     modelTempLZ,
                     modelSnow,
                     modelSoilMoisture,
                     modelSoil,
                     modelBog,
                     modelET,
                     modelSoilWater,
                     modelArea,
                     modelLayer){




  ################################################################################
  ################################################################################
  ## 1- UPDATE SNOW: Input of rain, snowmelt and snow                           ##
  ## OUTPUT (list)
  ##  - isoil                                                                   ##
  ##  - gisoil                                                                  ##
  ##  - spd                                                                     ##
  ##  - wcd                                                                     ##
  ##  - sca                                                                     ##
  ##  - nsno                                                                    ##
  ##  - alfa                                                                   ##
  ##  - ny                                                                      ##
  ##  - snowfree                                                                ##
  ################################################################################
  ################################################################################

  # update the variables related to snow
  snowUpdate <- do.snow(htempX=tempX,hprecipX=precipX,scaobX=scaobX,snowX=snowX,modelSnow=modelSnow,modelPrecipLZ=modelPrecipLZ)
  # OUTPUT (list)
  # - gisoil
  # - isoil
  # - spd
  # - wcd
  # - sca
  # - nsno
  # - alfa
  # - ny
  # - snowfree



  ###############################################################################################
  ###############################################################################################
  ## 2- UPDATE WATER VOLUME IN SOIL MOISTURE: Z(t):
  ##    Actual water volume present in the soil moisture zone (Skaugen et Onof, 2013)
  ## PROCESS:
  ##          A-Compute the amount of water in the different soils, for each level zone:
  ##             i-on non-glaciated soils
  ##             ii- on glaciated soils
  ##             iii- on glaciers
  ##             iv- NOT YET on bogs
  ##          B- compute the water volume: "Surface runoff"
  ## OUTPUT (list)
  ## - waterSoil
  ## - waterGlaciatedSoil
  ## - waterGlaciers
  ## - waterVolume (Z(t))
  ###############################################################################################
  ###############################################################################################

  soilMoistureUpdate <- do.soilMoisture(isoil=snowUpdate$isoil,gisoil=snowUpdate$gisoil,swgt=modelSoilMoisture$swgt,gwgt=modelSoilMoisture$gwgt,snowfree=snowUpdate$snowfree,glacfrac=modelSoil$glacfrac)
  # OUTPUT (list)
  # - waterSoil
  # - waterGlaciatedSoil
  # - waterGlaciers
  # - Z: water Volume


  ###############################################################################################
  ###############################################################################################
  ## 3- UPDATING SOIL WATER: dZ/dt= G(t) - X(t)- Ea(t)
  ## PROCESS:
  ##          A- Update snow reservoir
  ##          B- Update evapotranspitration
  ##          C- Update soil water content:
  ##             i- Update Actual evapotranspiration
  ##             ii- Update soil moisture G(t)
  ##             iii- Update excess water X(t)
  ##          D- Update Bog water content:
  ##             i- Update Actual evapotranspiration for bog
  ##             ii- Update bog moisture Gbog(t)
  ##             iii- Update bog excess water Xbog(t)
  ## OUTPUT (list)
  ## - SNOW RESERVOIR
  ## - Ea(t)
  ## - G(t)
  ## - X(t)
  ## - Eabog(t)
  ## - Gbog(t)
  ## - Xbog(t)
  ###############################################################################################
  ###############################################################################################

  # 3.1- UPDATE THE SNOW RESERVOIR
  sr <- do.snow.reservoir(snow=snowUpdate)
  snowReservoirUpdate <- list(snomag       = sr$snomag,
                              swe_h        = sr$swe_h,
                              middelsca    = sr$middelsca,
                              snofritt     = sr$snofritt)


  # 3.2- UPDATE EVAPOTRANSPIRATION: Ep(t)
  eatempUpdate <- do.evapotranspiration(htemp=tempX,sca=snowUpdate$sca)
  # OUTPUT (list)
  # - eatemp


  # 3.3- UPDATE SOIL WATER CONTENT FOR SOIL AND BOGS
  sw <- do.soilWater(eatemp=eatempUpdate$eatemp,
                         cea=modelET$cea,
                         M=groundwaterX$M,
                         D=soilDischargeX$D,
                         G=soilWaterX$G,
                         middelsca=snowReservoirUpdate$middelsca,
                         R=modelSoilWater$R,
                         Z=soilMoistureUpdate$Z,
                         Gbog=soilWaterX$Gbog,
                         Zbog=soilMoistureUpdate$waterSoil)


  soilWaterUpdate <- list(Ea           = sw$Ea,
                          G            = sw$G,
                          X            = sw$X,
                          Eabog        = sw$Eabog,
                          Gbog         = sw$Gbog,
                          Xbog         = sw$Xbog)



  ###################################################################################################
  ###################################################################################################
  ## 4- UPDATING SOIL DISCHARGE                                                                    ##
  ## PROCESS:                                                                                      ##
  ##         A- Saturation Level:                                                                  ##
  ##            i- Informs on current capacity for each level in mm.                               ##
  ##            ii-Effect of Excess water                                                          ##
  ##            iii- updating S (for all sub surface layers, NOT overland flow layer)              ##
  ##         B- Soil Discharge:                                                                    ##
  ##            i- Soil discharge of slopes                                                        ##
  ##            ii- Soil discharge of bogs                                                         ##
  ##            iii- Accumulated discharge with a dynamic                                          ##
  ## OUTPUT (list)                                                                                 ##
  ##  - D: represent the potential volume of water that is needed for complete saturation          ##
  ##  - qsimux: discharge at time t                                                                ##
  ##  - qsimX: Aggregated discharge                                                                ##
  ###################################################################################################
  ###################################################################################################

  # SATURATION LEVEL: ddist
  # WARNING ONLY FOR SOIL, NOT FOR BOGS
  ddistAllUpdate <- do.groundwater.ddistAll(Layers=groundwaterX$Layers,Magkap=groundwaterX$Magkap,nbStepsDelay=modelLayer$nbStepsDelay,X=soilWaterUpdate$X)
  # OUTPUT (list)
  # - ddistx
  # - ddist
  # - S


  # SOIL DISCHARGE: SLOPES AND BOGS
  sd <- do.soilDischarge(Timeresinsec = Timeresinsec,
                          layerUH = UH$layerUH,
                          ddistAll = ddistAllUpdate,
                          UHriver = UH$UHriver,
                          waterContent = soilWaterUpdate$X/1000,        #/1000 -> mm
                          area = modelArea$slopesriverarea,
                          modelBog = modelBog,
                          waterContentBog = soilWaterUpdate$Xbog/1000,  #/1000 -> mm
                          areabog = modelArea$bogarea,
                          qsimX = soilDischargeX$qsimX)
   # OUPTUT (list)
   # - qsimutx ( sum(qslopes + qbogs))
   # - qsimX (shifted and aggreagated)

   soilDischargeUpdate <- list(D       = (-1)*ddistAllUpdate$S,
                               qsimutx = sd$qsimutx,
                               qsimX      = sd$qsimX)


   ###################################################################################################
   ###################################################################################################
   ## 5- UPDATING GROUNDWATER                                                                       ##
   ## PROCESS:                                                                                      ##
   ##         UPDATE SATURATED LAYERS                                                               ##
   ## OUTPUT (list)                                                                                 ##
   ##  - Magkap                                                                                     ##
   ##  - M                                                                                          ##
   ##  - Layers                                                                                     ##
   ###################################################################################################
   ###################################################################################################

   # SATURATION LEVEL: Layers
   # WARNING ONLY FOR SOIL, NOT FOR BOGS
   gr <- do.groundwater.Layers(NoL=modelLayer$NoL,
                                Layers=groundwaterX$Layers,
                                ddist=ddistAllUpdate$ddist,
                                X=soilWaterUpdate$X,
                                layerUH=UH$layerUH,
                                nbStepsDelay=modelLayer$nbStepsDelay)
   # OUPTUT (list)
   # - Layers

   groundwaterUpdate <- list(Magkap = groundwaterX$Magkap,
                             M      = groundwaterX$M,
                             Layers = gr$Layers)
   # - Magkap
   # - M: Groundwater Storage Capacity (GSC)
   # - Layers: Layers of saturation



  ######################################
  # RESULTS
  ######################################
  res <- list( snow             = snowUpdate,
               soilMoisture     = soilMoistureUpdate,
               snowReservoir    = snowReservoirUpdate,
               soilWater        = soilWaterUpdate,
               ddistAll         = ddistAllUpdate,
               soilDischarge    = soilDischargeUpdate,
               groundwater      = groundwaterUpdate
              )
  return(res)

}
