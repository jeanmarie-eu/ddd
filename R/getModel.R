#' Get the models to run the hydrological model
#'
#' Get all the models to run ddd
#' Two options:
#' - build from a set of parameters
#' - load from a rda file
#' @param method Method to get the model parameters: "buildNVE" or "load"
#' @param path Directory where to get the files
#' @param inputParam List input parameters
#' @param Timeresinsec Time resolution of the process in second (1hour: 3600s, ... etc)
#' @param SAVE Save the results, Boolean
#' @param pathResults Path of the results. By default: $HOME
#' @return a list of all the models used to run ddd
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' getModel()
#' }

getModel <-function(method=NULL,path=NULL,inputParam=NULL,Timeresinsec=NULL,SAVE=FALSE,pathResults="~/"){
  models <- dddModel::getModel(method=method,path=path,inputParam=inputParam,Timeresinsec=Timeresinsec,SAVE=SAVE,pathResults=pathResults)
  return(models)
}
