#' Precipitation for each elevation zone
#'
#' The function processes the precipitation for each elevation zone
#' @param v precipitation value (scalar)
#' @param modelPrecipLZ
#'  list(nbLevelZone,Plr,hfelt,midmetp)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.precipLZ()
#' }

do.precipLZ <- function(v,modelPrecipLZ) {
  res <- dddPrecipLZ::stateX(v=v,modelPrecipLZ=modelPrecipLZ)
  return(res)
}
