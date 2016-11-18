#' Build the models of the hydrological processes
#'
#' The function does the following:
#'  -get the input parameters
#' - build the models
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param ddd ddd object
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' model()
#' }
model <-function(Timeresinsec,ddd){

  # GET PARAMETERS
  ddd$inputParam$do("getParam",args=list(method=methodParam,path=pathParam, filename=fileParam))
  ddd$inputParam$save(name="inputParam"))

  # GET MODELS
  ddd$model$do("getModel",args=list(method=methodModel,inputParam=inputParam,Timeresinsec=Timeresinsec)
  ddd$model$save(name="model"))

  invisible()

}
