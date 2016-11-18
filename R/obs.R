#' Get the observations
#'
#' Get the observations
#' @param ddd ddd object
#' @param pathPrecip path of the precipitation observation
#' @param filenamePrecip name of the precipitation file
#' @param pathTemp path of the temperature observation
#' @param filenameTemp name of the temperature file
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' obs()
#' }
obs <-function(ddd,pathPrecip,filenamePrecip,pathTemp,filenameTemp){

  ddd$precipLZ$do("getPrecipLZ",args=list(method="processedNVE",path=pathPrecip,filename=filenamePrecip))
  ddd$precipLZ$save(name="obs")

  ddd$tempLZ$do("getTempLZ",args=list(method="processedNVE",path=pathTemp,filename=filenameTemp))
  ddd$tempLZ$save(name="obs")

  ddd$Q$do("getQ",args=list(method="processedNVE",path=pathTemp,filename=filenameTemp))
  ddd$Q$save(name="obs")

  invisible()

}
