#' ddd.date
#'
#' ddd.date
#' @param timeResolution choice between "daily","hourly","minute","second"
#' @param fromPeriod "YYYYmmddHHMMSS"
#' @param toPeriod "YYYYmmddHHMMSS"
#' @param format conversion into a vector "YYMMDDHH" or a matrix "YY,MM,DD,HH"
#' @param saveDate "YYYYmmddHHMMSS"
#' @keywords ddd
#' @export
#' @examples
#' res <- date(timeResolution="hourly",
#'             fromPeriod="20150215",
#'             toPeriod="20150218",
#'             format="YY,MM,DD,HH")
#' head(res)

date <-function(timeResolution,fromPeriod,toPeriod,format,saveDate=NULL){

   Timeresinsec <- timeManip::insec(timeResolution=timeResolution)
   tmp <- timeManip::dateTimeSerie(timeResolution,fromPeriod,toPeriod)
   nbStep <- tmp$nbStep
   seqPeriod <- tmp$seqPeriod
   dateTS <- timeManip::convertFormat(date=seqPeriod,format="YY,MM,DD,HH")

   if (!is.null(saveDate)){
     saveDatePOSIXct <- base::strptime(saveDate,"%Y%m%d%H",tz="GMT")
     indiceSave <- c()
     for (i in 1:length(saveDate)){
       indiceSave <- c(indiceSave,which(seqPeriod==saveDatePOSIXct))
     }
   } else indiceSave <- NULL


   res <- list(Timeresinsec = Timeresinsec,
                nbStep = nbStep,
                seqPeriod = seqPeriod,
                dateTS = dateTS,
                indiceSave = indiceSave )
   return(res)

}
