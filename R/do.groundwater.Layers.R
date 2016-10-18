#' Saturation layers
#' The function updates the saturation levels.
#' It computes the runoff event that occurs in each of layer, shift the layer by one timestep and then update the layer value with the runoff value
#' @param NoL number of level zone
#' @param Layers matrix describing the groundwater; the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @param ddist states (in mm) of each saturation level
#' @param X xcess water
#' @param layerUH Unit Hydrograph of the layer
#' @param nbStepsDelay delay-steps of each layer
#' @return The output is a matrix describing the groundwater.
#'  the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.groundwater.Layers()
#' }
do.groundwater.Layers <-function(NoL,Layers,ddist,X,layerUH,nbStepsDelay){
  res <- dddGroundwater::grd.layers(NoL=NoL,Layers=Layers,ddist=ddist,X=X,layerUH=layerUH,nbStepsDelay=nbStepsDelay)
  return(res)
}
