#' ddistAll initialization

#' The funtion initializes the main features of the groundwater:
#' @param method method for the initialization, "load", "source", "manual"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param S deficit
#' @param ddistx current capacity of each level (in mm)
#' @param ddist states of each saturation level
#' @keywords groundwater
#' @export
#' @examples
#'\dontrun{
#' init.ddistAll())
#' }

init.ddistAll <-function(method=NULL,path=NULL,S=NULL,ddistx=NULL,ddist=NULL){
  res <- dddGroundwater::init.ddistAll(method=method,path=path,S=S,ddistx=ddistx,ddist=ddist)
  return(res)
}
