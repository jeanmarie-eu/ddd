#' ddd general simulation
#'
#' Main function for the simulation of the hydrological model ddd
#' @param ddd ddd object
#' @param fromPeriod "YYYYmmddHH"
#' @param toPeriod "YYYYmmddHH"
#' @param timeResolution "daily", "three-hourly", "hourly"
#' @param catchment name of the catchement. Not in used yet
#' @param pathData path of the data file (ptq)
#' @param fileData name of the data file (ptq)
#' @param pathParam path of the parameters file
#' @param fileParam name of the parameters file (ptq)
#' @param methodParam method how to get the param. Default is "processedNVE"
#' @param methodModel method how to get the model. Default is "processedNVE"
#' @param pathResults path of the results. Default is HOME
#' @param D_ci initial condition for the potential volume of water that is needed for complete saturation. Default is 2
#' @param saveDate full data of this date will be saved
#' @param FIGURE Boolean to plot a basic plot. Default is FALSE
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' main()
#' }

main <- function(ddd,fromPeriod=NULL,toPeriod=NULL,timeResolution=NULL,catchment="catchment",pathData=NULL,fileData=NULL,pathParam=NULL,fileParam=NULL,methodParam="processedNVE",methodModel="processedNVE",pathResults="~/",D_ci=2,saveDate=NULL,FIGURE=FALSE){

  if ( (is.null(ddd)) &&
       (is.null(fromPeriod)) &&
       (is.null(toPeriod)) &&
       (is.null(timeResolution)) &&
       (is.null(pathData)) &&
       (is.null(fileData)) &&
       (is.null(pathParam)) &&
       (is.null(fileParam)) ) {
         stop("Error: arguments 'ddd','fromPeriod','toPeriod','timeResolution','pathData','fileData','pathParam','fielParam' must be provided")
  } else {

     ddd<- start(namefield="ddd",namespace="ddd",pathRes=pathResults)

     pathResults <- normalizePath(file.path(pathResults,paste0("dddRes_",format(Sys.time(), "%Y-%m-%d-%H-%M",tz="GMT"))),mustWork = FALSE)
     dir.create(pathResults, showWarnings = FALSE, recursive = TRUE)

     timePeriod <- date(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod,format="YY,MM,DD,HH",saveDate=saveDate)

     obs(ddd=ddd,pathPrecip=pathData,filenamePrecip=fileData,pathTemp=pathData,filenameTemp=fileData,pathQ=pathData,filenameQ=fileData)

     param(ddd=ddd,method=methodParam,path=pathParam,filename=fileParam)

     model(ddd=ddd,method=methodModel,Timeresinsec=timePeriod$Timeresinsec,inputParam=ddd$inputParam$values())

     init(ddd=ddd,Timeresinsec=timePeriod$Timeresinsec,q1=ddd$Q$values()[1])

     results <- simulation(ddd=ddd,timePeriod=timePeriod,saveDate=saveDate,pathRes=pathResults)

     if (FIGURE) {
       graph.ts(dateTS = timePeriod$dateTS,
                precip = rowMeans(as.matrix(ddd$precipLZ$values()),na.rm=TRUE),
                q      = results$simulation[,8],
                q2     = results$simulation[,7])

     }

  }

  return(TRUE)


}
