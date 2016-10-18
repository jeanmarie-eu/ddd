#' Unit Hydrograph initializing
#'
#' The funtion initialize the main Unit Hydrographs:
#' - The UH of the river,
#' - the UH of the layers
#' - The UH of the Mean Annual Discharge (MAD)
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param, NoL)
#' @param modelRiver list of parameters about the river
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelMAD list of parameters about the Mean Annual Discharge
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init.UH()
#' }
init.UH <-function(Timeresinsec,modelLayer,modelRiver,modelMAD){
   res   <- dddUH::init(Timeresinsec,modelLayer,modelRiver,modelMAD)
   return(res)
}
