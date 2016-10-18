#' Temperature for each elevation zone
#'
#' The function \code{stateX()} process the temperature for each elevation zone
#' @param v temperature value (scalar)
#' @param modelTempLZ
#'  list(nbLevelZone,Tlr,hfelt,midmett)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.tempLZ()
#' }

do.tempLZ <- function(v,modelTempLZ) {
  res <- dddTempLZ::stateX(v=v,modelTempLZ=modelTempLZ)
  return(res)
}
