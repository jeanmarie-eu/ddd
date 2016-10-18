#' Computes the evapotranspiration
#'
#'  Computes the evapotranspiration.
#' @param htemp  temperature for each level Zone
#' @param sca snow coverage for each level zone
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.evapotranspiration()
#' }

do.evapotranspiration <- function(htemp,sca) {
  res <- dddEvapotranspiration::stateX(htemp=htemp,sca=sca)
  return(res)
}
