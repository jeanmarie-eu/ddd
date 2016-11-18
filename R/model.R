#' Build the models of the hydrological processes
#'
#' Build the models of the hydrological processes
#' @param ddd ddd object
#' @param method "manual", "load","processedNVE"
#' @param ... parameters related to the method
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' model()
#' }
model <-function(ddd,method,...){

  # GET MODELS
  ddd$model$do("getModel",args=list(method=method,...))
  ddd$model$save(name="model")

  invisible()

}
