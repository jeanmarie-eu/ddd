#' Get the models to run the hydrological model
#'
#' Get all the models
#' Three options:
#' - build from a set of parameters
#' - load from a rda file
#' - load from a R-file
#' @param getModel method to get the model parameters ("build", "load" or "source")
#' @param path directory where to get the files
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param SAVE Boolean to save the current model
#' @param pathResults path to save the results
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' getModel()
#' }

getModel <-function(getModel,path,Timeresinsec=NULL,SAVE=FALSE,pathResults="~"){
  models <- dddModel::getModel(getModel=getModel,path=path,Timeresinsec=Timeresinsec)

  pathModel <- paste0(pathResults,"model/")
  dir.create(pathModel, showWarnings = FALSE)
  if (SAVE) do.call("save",list(obj="models", file=paste0(pathModel,"models.rda")))
  return(models)


}
