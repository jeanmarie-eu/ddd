#' States, capacity of saturation layers and deficit
#'
#' The function has three tasks:
#' - it informs on current capacity for each level in mm.
#' - it computes the states (in mm) of each saturation level
#' - it computes the deficit S (for all sub surface layers, NOT overland flow layer)
#' @param Layers matrix describing the groundwater; the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @param Magkap Maximaum capacity
#' @param nbStepsDelay delay-steps for each level
#' @param X excess water value
#' @return The output is a list providing the three main features for groundwater( ddistx, ddist, S)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.groundwater.ddistAll()
#' }
do.groundwater.ddistAll <-function(Layers,Magkap,nbStepsDelay,X){
  res <- dddGroundwater::ddistAll(Layers=Layers,Magkap=Magkap,nbStepsDelay=nbStepsDelay,X=X)
  return(res)
}
