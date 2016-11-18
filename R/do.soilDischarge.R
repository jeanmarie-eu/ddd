#' state of Soil Discharge
#'
#' The function \code{stateX()} process the soil discharge:
#' 1- process the soil discharge of slopes
#' 2- process the soil discharge of bogs
#' 3- process the accumulated discharge with a dynamic
#' Unit is in m3/s
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param layerUH Unit Hydrograph of the saturation layers
#' @param ddistAll states of each saturation level
#' @param UHriver Unit Hydrograp of the river
#' @param waterContent excess of water in millimeters over Slopes
#' @param area in squared meters
#' @param modelBog list of parameters of the bog
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param waterContentBog excess of water in millimeters over Bogs
#' @param areabog in squared meters
#' @param qsimX soil discharge value to take into account while doing the accumulation
#' @return The output is a list of i- the discharge at the time step and ii- the accumulatied discharge
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.soilDischarge()
#' }

do.soilDischarge <-function(Timeresinsec,
                             layerUH,
                             ddistAll,
                             UHriver,
                             waterContent,
                             area,
                             modelBog,
                             waterContentBog,
                             areabog,
                             qsimX) {

  res <- dddSoilDischarge::stateX.soilDischarge(Timeresinsec=Timeresinsec,layerUH=layerUH,ddistAll=ddistAll,UHriver=UHriver,waterContent=waterContent,
                                  area=area,modelBog=modelBog,waterContentBog=waterContentBog,areabog=areabog,
                                  qsimX=qsimX)
  return(res)
}
