#' state of Snow Reservoir
#'
#' Compute the snow reservoir
#' @param snow snow
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.snow.reservoir()
#' }

do.snow.reservoir <- function(snow) {
  res <- dddSnow::stateX.snowReservoir(snow=snow)
  return(res)
}
