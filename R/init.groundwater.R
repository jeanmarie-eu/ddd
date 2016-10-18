#' init

#' The funtion initializes the main features of the groundwater:
#' - The maximum capacity of each staturation level,
#' - the Groundwater Storage Capacity (GSC),
#' - The saturation layers
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param MAD value of the Mean Annual Discharge
#' @param area in squared meters
#' @param modelSaturation list of parameters about the saturation
#'  list(gtcel,CapacityUpperLevel,mLam,varLam,distr)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param,NoL)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init.groundWater()
#' }

init.groundWater <-function(Timeresinsec,UHMAD,MAD,area,modelSaturation,modelLayer){

  res <- dddGroundwater::init(Timeresinsec=Timeresinsec,UHMAD=UHMAD,MAD=MAD,area=area,modelSaturation=modelSaturation,modelLayer=modelLayer)

  return(res)
}
