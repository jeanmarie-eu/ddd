#' Initialize information related to Soil Moisture
#'
#' Initialize the information related to ssoil Moisture
#' @param method method for the initialization, "load", "source", "manual", "procecessed"
#' @param path directory where to get the files
#' @param Ea actual evapotranspiration
#' @param G Volume of received moisture input of rain and snowmelt
#' @param X excess water
#' @param Eabog actual evapotranspiration on bog
#' @param Gbog Volume of received moisture input of rain and snowmelt a time t on Bogs
#' @param Xbog excess water on bog
#' @param eatemp evapotranspiration
#' @param cea Degree day factor for evpotranspiration(mm/degree/day)
#' @param M Groundwater Storage Capacity (GSC)
#' @param D the potential volumee of water that is needed for complete saturation
#' @param middelsca average snow coverage over the level zones
#' @param R Ratio defining field capacity (fracion of D)
#' @param Z Actual water volume present in the soil moisture zone
#' @param Zbog Actual water volume present on bogs#' @keywords soilWater
#' @export
#' @examples
#' \dontrun{
#' init.soilWater()
#' }
init.soilWater <-function(method=NULL,path=NULL,Ea=NULL,G=NULL,X=NULL,Eabog=NULL,Gbog=NULL,Xbog=NULL,
                          eatemp=NULL,cea=NULL,M=NULL,D=NULL,middelsca=NULL,R=NULL,Z=NULL,Zbog=NULL){

   res <- dddSoilWater::init.soilWater(method=method,path=path,Ea=Ea,G=G,X=X,Eabog=Eabog,Gbog=Gbog,Xbog=Xbog,
                             eatemp=eatemp,cea=cea,M=M,D=D,G=G,middelsca=middelsca,R=R,Z=Z,Zbog=Zbog)

   return(res)

}
