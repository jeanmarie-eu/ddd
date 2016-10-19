#' Get and process the parameters
#'
#' Get and process the parameters from a R-source file.
#' Two files are required:
#' - one for the calibrated parameters (paramCalibrated.R)
#' - one for the GIS-related parameters (paramGIS.R)
#' The files are present in the data folder of this package.
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param path path of the directory where to find the two files
#' @return The output is a list of processed parameters to be build the model
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' getParam()
#' }
getParam <-function(Timeresinsec,path){
   res   <- dddModel::getParam(Timeresinsec=Timeresinsec,path=path)
   return(res)

}
