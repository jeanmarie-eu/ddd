#' Get the input parameters of the hydrological processes
#'
#' Get the input parameters of the hydrological processes
#' @param ddd ddd object
#' @param method "manual", "load","processedNVE"
#' @param ... parameters related to the method
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' param()
#' }
param <-function(ddd,method,...){

  # GET PARAMETERS
  ddd$inputParam$do("getParam",args=list(method=method,...))
  ddd$inputParam$save(name="inputParam")

  invisible()

}
