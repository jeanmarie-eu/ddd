#' Initialize information related to Soil Moisture
#'
#' Initialize the information related to Soil Moisture
#' @param method method for the initialization, "load", "source", "manual", "procecessed"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param waterSoil amount of water in soil (in mm)
#' @param waterGlaciatedSoil amount of water in glaciated soil (in mm)
#' @param waterGlaciers amount of water in glaciers (in mm)
#' @param Z waterVolume, input from rain, snow and glaciers (in mm)
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
#' init.soilMoisture()
#' }

init.soilMoisture <-function(method=NULL,path=NULL,waterSoil=NULL,waterGlaciatedSoil=NULL,waterGlaciers=NULL,Z=NULL,
               isoil=NULL,gisoil=NULL,bisoil=NULL,swgt=NULL,gwgt=NULL,snowfree=NULL,glacfrac=NULL){

  res <- dddSoilMoisture::init.soilMoisture(method=method,path=path,waterSoil=waterSoil,waterGlaciatedSoil=waterGlaciatedSoil,waterGlaciers=waterGlaciers,Z=Z,
                               isoil=isoil,gisoil=gisoil,bisoil=bisoil,swgt=swgt,gwgt=gwgt,snowfree=snowfree,glacfrac=glacfrac)

  return(res)
}
