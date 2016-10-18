#' Initializing the soil discharge

#' The funtion initializes the soil discharge
#' Unit is in m3/s
#' @param MAD Mean Annual Discharge values
#' @param q1 first value of the runoff timeserie
#' @param D volume of the unsaturated zone
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelArea list of parameters about the area
#'  list(totarea,slopesriverarea,nobognoglacarea,bogarea)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param,NoL)
#' @param modelRiver list of parameters about the river
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelBog list of parameters of the bog
#'  list(maxL,speed,nbStepsDelay,z,dist,param)
#' @param layerUH Unit Hydrograph of the saturation layers
#' @param NoL number of level zone
#' @param UHriver Unit Hydrograp of the river
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init.soilDischarge()
#'}
init.soilDischarge <-function(MAD,q1,
                                  D,
                                  Timeresinsec,
                                  modelArea,
                                  modelLayer,
                                  modelRiver,
                                  modelBog,
                                  layerUH,
                                  NoL,
                                  UHriver){

  res <- dddSoilDischarge::init(MAD=MAD,q1=q1,D=D,Timeresinsec=Timeresinsec,
                               modelArea=modelArea,modelLayer=modelLayer,
                               modelRiver=modelRiver,modelBog=modelBog,
                               layerUH=layerUH,NoL=NoL,UHriver=UHriver)
  return(res)

}
