#' Initialize information related to snow
#'
#' Initialize the information related to snow
#' @param method method for the initialization, "load", "manual"
#' @param path directory where to get the files, in used when method is "load"
#' @param snomag snow mag, NEED MORE EXPLANATION
#' @param swe_h swe_h, NEED MORE EXPLANATION
#' @param middelsca averaged sca
#' @param snofritt snowfree, NEED MORE EXPLANATION
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' init.snowReservoir()
#' }

init.snowReservoir <-function(method=NULL,path=NULL,snomag=NULL,swe_h=NULL,middelsca=NULL,snofritt=NULL){

  res <- dddSnow::init.snowReservoir(method=method,snomag=snomag,swe_h=swe_h,middelsca=middelsca,snofritt=snofritt)

  return(res)
}
