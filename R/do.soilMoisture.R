#' state of Soil Moisture
#'
#' The function provides the key features of the soil Moisture.
#' 1- Compute the water amount of precipitation and snowmelt, for each level zone:
#' i-on non-glaciated soils
#' ii- on glaciated soils
#' iii- on glaciers
#' iv- NOT YET on bogs
#' 2- compute the Z(t): Actual water volume present in the soil moisture zone (Skaugen et Onof, 2013)
#' @param isoil precipitation and/or snowmelt from the elevation zones, vector(level zone)
#' @param gisoil glaciermelt from the elevation zones, vector(level zone)
#' @param bisoil bog melt from the elevation zones, vector(level zone)
#' @param swgt weights for input for each elevation zone
#' @param gwgt weights for input for each elevation zone
#' @param snowfree weights for input for each elevation zone
#' @param glacfrac fraction of glaciers area
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.soilMoisture()
#' }
do.soilMoisture <- function(isoil,gisoil,bisoil=NULL,swgt,gwgt,snowfree,glacfrac) {
  res <- dddSoilMoisture::stateX.soilMoisture(isoil=isoil,gisoil=gisoil,bisoil=bisoil,swgt=swgt,gwgt=gwgt,snowfree=snowfree,glacfrac=glacfrac)
  return(res)
}
