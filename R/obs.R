#' Get the observations
#'
#' Get the observations
#' @param ddd ddd object
#' @param pathPrecip path of the precipitation observation
#' @param filenamePrecip name of the precipitation file
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' obs()
#' }
obs <-function(ddd,pathPrecip,filenamePrecip){

  ddd$PrecipLZ$do("getPrecipLZ",args=list(method="none",path=pathPrecip,filename=filenamePrecip))
  ddd$PrecipLZ$save(name="obs")

  invisible()

}
