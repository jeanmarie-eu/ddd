#' plot of the timeseries
#'
#' Very simple timeserie chart
#' @param dateTS POSIXct
#' @param precip precipitation timeserie
#' @param q runoff timeserie
#' @param q2 runoff timeserie
#' @param minq minimum runoff value
#' @param maxq maximum runoff value
#' @param minprecip minimum precipitation value
#' @param maxprecip maximum precipitation values
#' @param name name of the chart
#' @param path path of the saved chart. If NULL, plot on screen
#' @param width_mm width of the chart
#' @param height_mm height of the chart
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' graph.ts()
#' }
graph.ts <- function(dateTS,
                        precip,
                        q,
                        q2 = NULL,
                        minq = NULL,
                        maxq = NULL,
                        minprecip = NULL,
                        maxprecip = NULL,
                        name = "precip-discharge timeserie",
                        path = NULL,
                        width_mm = 180,
                        height_mm = 180) {

  if (!is.null(path)){
     dir.create(paste(path, "figures",sep=""), showWarnings = FALSE)
     grDevices::tiff(filename = paste(path,"figures/",name,".tif",sep=""), width = width_mm, height = height_mm,
     units = "mm", pointsize = 12,
     compression = "lzw",
     bg = "transparent", res = 300)
   graphics::par(fig=c(0,0.8,0,1))
  } else grDevices::dev.new()

  minq <- ifelse(is.null(minq),min(q,q2,na.rm=TRUE),minq)
  maxq <- ifelse(is.null(maxq),max(q,q2,na.rm=TRUE),maxq)
  minprecip <- ifelse(is.null(minprecip),min(precip,na.rm=TRUE),minprecip)
  maxprecip <- ifelse(is.null(maxprecip),max(precip,na.rm=TRUE),maxprecip)

  if (!(inherits(dateTS, "POSIXct"))) {
    # we assume a matrix with 4 columns: YY MM DD HH
    tmp <- dateTS
    dateTS <- strptime(paste(tmp[,1],tmp[,2],tmp[,3],tmp[,4],sep="-"),format = "%Y-%m-%d-%H", tz="UTC")
  }

  graphics::plot(dateTS,q,type="l",col="black",ylim=c(minq,maxq),axes=TRUE,xlab="dates", ylab=expression("m"^3~"/s") )
  graphics::par(new=TRUE)
  if (!is.null(q2)){
     graphics::plot(dateTS,q2,type="l",col="red",ylim=c(minq,maxq),axes=FALSE,xlab="", ylab="" )
     graphics::par(new=TRUE)
  }
  graphics::plot(dateTS,precip,type="l",ylim=c(maxprecip,minprecip),axes=FALSE,xlab="", ylab="",col="blue")
  graphics::box()
  graphics::axis(4)
  graphics::mtext("mm/h",side=4)

  if (!is.null(path)) dev.off()

  return(TRUE)
}
