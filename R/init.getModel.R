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
#' init.getModel()
#' }

init.getModel <-function(getModel,path,Timeresinsec=NULL,SAVE=FALSE,pathResults=NULL){
  models <- dddModel::getModel(getModel=getModel,path=path,Timeresinsec=Timeresinsec)
  if (SAVE) do.call("save",list(obj="models", file=paste0(pathResults,"models.rda")))
  return(models)


}
