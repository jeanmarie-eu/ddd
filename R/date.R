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
#'             fromPeriod="2015021500",
#'             toPeriod="2015021823",
#'             format="YY,MM,DD,HH")
#' head(res)

date <-function(timeResolution,fromPeriod,toPeriod,format,saveDate=NULL){

   Timeresinsec <- timeManip::insec(timeResolution=timeResolution)
   tmp <- timeManip::timeserie(timeResolution,fromPeriod,toPeriod,precision="hourly")
   nbStep <- tmp$nbStep
   seqPeriod <- tmp$seqPeriod
   dateTS <- timeManip::YYYYmmddHH_m(tmp$seqPeriod)

   if (!is.null(saveDate)){
     saveDatePOSIXct <- base::strptime(saveDate,"%Y%m%d%H",tz="GMT")
     indiceSave <- c()
     for (i in 1:length(saveDate)){
       if (length(which(seqPeriod==saveDatePOSIXct))) {
         indiceSave <- c(indiceSave,which(seqPeriod==saveDatePOSIXct))
       } else indiceSave <- c(indiceSave,-1)

     }
   } else indiceSave <- NULL


   res <- list(Timeresinsec = Timeresinsec,
                nbStep = nbStep,
                seqPeriod = seqPeriod,
                dateTS = dateTS,
                indiceSave = indiceSave )
   return(res)

}
