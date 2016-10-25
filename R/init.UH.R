#' Unit Hydrograph initializing
#'
#' The funtion initialize the main Unit Hydrographs:
#' - The UH of the river,
#' - the UH of the layers
#' - The UH of the Mean Annual Discharge (MAD)
#' @param method method for the initialization, "load", "source", "processed"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param, NoL)
#' @param modelRiver list of parameters about the river
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelMAD list of parameters about the Mean Annual Discharge
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param SAVE Save the results, Boolean
#' @param pathResults Path of the results. By default: $HOME
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init.UH()
#' }
init.UH <-function(method=NULL,path=NULL,Timeresinsec=NULL,modelLayer=NULL,modelRiver=NULL,modelMAD=NULL,SAVE=FALSE,pathResults="~/"){
   res   <- dddUH::init.UH(method=method,Timeresinsec=Timeresinsec,modelLayer=modelLayer,modelRiver=modelRiver,modelMAD=modelMAD,SAVE=SAVE,pathResults=pathResults)
   return(res)
}
