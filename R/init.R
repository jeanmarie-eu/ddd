#' Initialize the hydrological processes
#'
#' The function does the following:
#'  -initialize UH
#' - initialize snow
#' - initialize snowReservoir
#' - initialize soil moisture
#' - initialize soil discharge
#' - initialize ddistAll
#' - initialize grounwater
#' - initialize soil water
#' @param ddd ddd object
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param q1 initial runoff
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init()
#' }
init <-function(ddd,Timeresinsec,q1){


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
                                            ddist  = rep(1/ddd$model$modelLayer$NoL,ddd$model$modelLayer$NoL) ))
  ddd$ddistAll$save(name="init")


  # E- SOIL DISCHARGE: SLOPES AND BOGS
  ddd$soilDischarge$do("init.soilDischarge",args=list(method="processed",
                                                     MAD=ddd$model$values()$modelSoilDischarge$MAD,
                                                     q1=q[1],
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

  invisible()

}
