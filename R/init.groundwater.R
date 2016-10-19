#' init

#' The funtion initializes the main features of the groundwater:
#' @param method method for the initialization, "load", "source", "manual"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param Magkap Magkap
#' @param M  Groundwater Storage Capacity (GSC)
#' @param Layers saturation layers
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param MAD value of the Mean Annual Discharge
#' @param modelArea
#' list(totarea,slopesriverarea,nobognoglacarea,bogarea)
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

init.groundwater <-function(method=NULL,path=NULL,Magkap=NULL,M=NULL,Layers=NULL,Timeresinsec=NULL,UHMAD=NULL,MAD=NULL,modelArea=NULL,modelSaturation=NULL,modelLayer=NULL){

  res <- dddGroundwater::init.groundwater(method=method,path=path,Magkap=Magkap,M=M,Layers=Layers,Timeresinsec=Timeresinsec,UHMAD=UHMAD,MAD=MAD,modelArea=modelArea,modelSaturation=modelSaturation,modelLayer=modelLayer)

  return(res)
}
