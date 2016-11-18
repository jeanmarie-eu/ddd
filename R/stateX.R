#' State of the hydrological processes
#'
#' The function does the following:
#'  -state of Snow
#' - state of water volume in Soil Moisture: Z(t)
#' - state of Soil Water: EA(t), G(t), X(t) for soil and bogs
#' - state of Soil Discharge: D(t), runoff
#' - state of ddistAll: ddist, ddisx, S
#' - state of Groundwater: MAGKAP,M,Saturation Layers
#' @param ddd ddd object
#' @param temperature temperature input
#' @param precipitation precipitation input
#' @param scaob snow coverage input
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' stateX()
#' }
stateX <-function(ddd,temperature,precipitation,scaob,Timeresinsec){

  ddd$snow$do("stateX.snow",args=list(htempX=temperature,
                                      hprecipX=precipitation,
                                      scaobX=scaob,
                                      snowX=ddd$snow$values(),
                                      modelSnow=ddd$model$values()$modelSnow,
                                      modelPrecipLZ=ddd$model$values()$modelPrecipLZ))

  ddd$soilMoisture$do("stateX.soilMoisture",args=list(isoil=ddd$snow$values()$isoil,
                                                      gisoil=ddd$snow$values()$gisoil,
                                                      swgt=ddd$model$values()$modelSoilMoisture$swgt,
                                                      gwgt=ddd$model$values()$modelSoilMoisture$gwgt,
                                                      snowfree=ddd$snow$values()$snowfree,
                                                      glacfrac=ddd$model$values()$modelSoil$glacfrac))

  ddd$snowReservoir$do("stateX.snowReservoir",args=list(snow=ddd$snow$values()))

  ddd$evapotranspiration$do("stateX.evapotranspiration",args=list(htemp=temperature,sca=ddd$snow$values()$sca))

  ddd$soilWater$do("stateX.soilWater",args=list(eatemp=ddd$evapotranspiration$values()$eatemp,
                                                cea=ddd$model$values()$modelET$cea,
                                                M=ddd$groundwater$values()$M,
                                                D=ddd$soilDischarge$values()$D,
                                                G=ddd$soilWater$values()$G,
                                                middelsca=ddd$snowReservoir$values()$middelsca,
                                                R=ddd$model$values()$modelSoilWater$R,
                                                Z=ddd$soilMoisture$values()$Z,
                                                Gbog=ddd$soilWater$values()$Gbog,
                                                Zbog=ddd$soilMoisture$values()$waterSoil))

  ddd$ddistAll$do("stateX.ddistAll",args=list(Layers=ddd$groundwater$values()$Layers,
                                              Magkap=ddd$groundwater$values()$Magkap,
                                              nbStepsDelay=ddd$model$values()$modelLayer$nbStepsDelay,
                                              X=ddd$soilWater$values()$X))

  ddd$soilDischarge$do("stateX.soilDischarge",args=list(Timeresinsec = Timeresinsec,
                                                      layerUH = ddd$uh$values()$layerUH,
                                                      ddistAll = ddd$ddistAll$values(),
                                                      UHriver = ddd$uh$values()$UHriver,
                                                      waterContent = ddd$soilWater$values()$X/1000,        #/1000 -> mm
                                                      area = ddd$model$values()$modelArea$slopesriverarea,
                                                      modelBog = ddd$model$values()$modelBog,
                                                      waterContentBog = ddd$soilWater$values()$Xbog/1000,  #/1000 -> mm
                                                      areabog = ddd$model$values()$modelArea$bogarea,
                                                      qsimX = ddd$soilDischarge$values()$qsimX))

   ddd$groundwater$do("stateX.groundwater",args=list(NoL=ddd$model$values()$modelLayer$NoL,
                                                   Layers=ddd$groundwater$values()$Layers,
                                                   ddist=ddd$ddistAll$values()$ddist,
                                                   X=ddd$soilWater$values()$X,
                                                   layerUH=ddd$uh$values()$layerUH,
                                                   nbStepsDelay=ddd$model$values()$modelLayer$nbStepsDelay,
                                                   Magkap=ddd$groundwater$values()$Magkap,
                                                   M=ddd$groundwater$values()$M))

  invisible()

}
