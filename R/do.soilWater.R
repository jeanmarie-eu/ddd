#' state of Soil Water
#'
#' The function processes the soil water content both for soils and bogs.
#' The process follows the expression dZ/dt= G(t) - X(t)- Ea(t) in Skaugen, Peerebom and Nilsson (2015).
#' There are hence three steps:
#'  i- it processes the actual evapotranspiration Ea(t)
#'  ii- it processes the soil moisture G(t)
#'  iii- it processes the excess water X(t)
#' @param eatemp evapotranspiration
#' @param cea Degree day factor for evpotranspiration(mm/degree/day)
#' @param M Groundwater Storage Capacity (GSC)
#' @param D the potential volumee of water that is needed for complete saturation at time t
#' @param G Volume of received moisture input of rain and snowmelt a time t
#' @param middelsca average snow coverage over the level zones
#' @param R Ratio defining field capacity (fracion of D)
#' @param Z Actual water volume present in the soil moisture zone
#' @param Gbog Volume of received moisture input of rain and snowmelt a time t on Bogs
#' @param Zbog Actual water volume present on bogs
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.soilWater()
#' }

do.soilWater <- function(eatemp,cea,M,D,G,middelsca,R,Z,Gbog,Zbog){
  res <- dddSoilWater::stateX.soilWater(eatemp=eatemp,cea=cea,M=M,D=D,G=G,middelsca=middelsca,R=R,Z=Z,Gbog=Gbog,Zbog=Zbog)
  return(res)
}
