#' state of the information related to snow
#'
#' Update the information related to snow
#' @param htempX temperature for each elevation zone
#' @param hprecipX precipitation for each elevation zone
#' @param scaobX snow coverage observation
#' @param snowX snow
#' @param modelSnow
#'  list(nbLevelZone,unitsnow,n0,Ws,TS,CX,CFR,CGLAC,gca,UP)
#' @param modelPrecipLZ
#'  list(nbLevelZone,Plr,hfelt,midmetp)
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.snow()
#' }

do.snow <- function(htempX,hprecipX,scaobX,snowX,modelSnow,modelPrecipLZ) {
  res <- dddSnow::stateX(htempX=htempX,hprecipX=hprecipX,scaobX=scaobX,snowX=snowX,modelSnow=modelSnow,modelPrecipLZ=modelPrecipLZ)
  return(res)
}
