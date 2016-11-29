#' Simulation
#'
#' Simulation of the hydrological model.
#'
#' @param timePeriod data timeserie and its related information
#' @param ddd ddd object
#' @param saveDate date to be saved if any
#' @param pathRes path of the results for the csv file
#' @param nameRes filename of the results
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.simTSv2()
#' }
simulation <-function(ddd,timePeriod,saveDate,pathRes,nameRes="simulation"){


if ( (!is.null(timePeriod)) && (!is.null(ddd))) {

  # simulation to be recorded
  simresult<-c()

  #for each timesteps of the period of simulation
  for (i in 1:timePeriod$nbStep){

     # Input variables
     hprecip <- unlist(ddd$precipLZ$values()[i,])
     htemp   <- unlist(ddd$tempLZ$values()[i,])
     scaob   <- unlist(ddd$scaob$values()) #NA

     if (!is.na(htemp) && (!is.na(hprecip)) ){

       #########################################################################
       # PURPOSE: UPDATING THE FOLLOWING INFORMATION                          ##
       ## 1- UPDATE SNOW: INPUT OF RAIN, SNOWMELT AND SNOW                    ##
       ## 2- UPDATE WATER VOLUME IN SOIL MOISTURE: Z(t)                       ##
       ## 3- UPDATE SOIL WATER: EA(t), G(t), X(t)                             ##
       ## 4- UPDATE SOIL DISCHARGE: D(t), qsimut                              ##
       ## 5- UPDATE ddistAll <-LIST(SP)                                       ##
       ## 6- UPDATE GROUNDWATER <- LIST(MAGKAP,M,LAYERS)                      ##
       #########################################################################
       stateX(ddd=ddd,temperature=htemp,precipitation=hprecip,scaob=scaob,Timeresinsec=timePeriod$Timeresinsec)

       # Save simulation
       simresult <- rbind(simresult,
                          c(timePeriod$dateTS[i,1],
                            timePeriod$dateTS[i,2],
                            timePeriod$dateTS[i,3],
                            timePeriod$dateTS[i,4],
                            mean(hprecip,na.rm=TRUE),
                            mean(htemp,na.rm=TRUE),
                            ddd$Q$values()[i],
                            ddd$soilDischarge$values()$qsimX[1],
                            ddd$snowReservoir$values()$middelsca,
                            ddd$snowReservoir$values()$snomag,
                            (ddd$groundwater$values()$M-ddd$soilDischarge$values()$D),
                            ddd$soilDischarge$values()$D,
                            ddd$soilWater$values()$G,
                            ddd$soilWater$values()$Ea,
                            ddd$soilWater$values()$X,
                            ddd$soilWater$values()$Gbog,
                            ddd$soilWater$values()$Eabog,
                            ddd$soilWater$values()$Xbog,
                            ddd$soilMoisture$values()$Z,
                            ddd$soilMoisture$values()$waterGlaciers,
                            (ddd$soilMoisture$values()$waterGlaciatedSoil+ddd$soilMoisture$values()$waterGlaciers)
                          ))

     } else {

       # WARNING:
       # No computation when either the precipiatation or the temperature is missing
       simresult <- rbind(simresult,
                          c(timePeriod$dateTS[i,1],timePeriod$dateTS[i,2],timePeriod$dateTS[i,3],timePeriod$dateTS[i,4],
                            NA,NA,ddd$Q$values()[i],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
                          )

     }

     if (!is.null(timePeriod$indiceSave)) {
       if (i == timePeriod$indiceSave) ddd$save(saveDate)
     }


  }
  colnames(simresult)<-c("year","month","day","hour","mhprec","mhtemp","qobs","qsim","middelsca","snowmag","M-D","D","G","Ea","X","Gbog","Eabog","Xbog","Z","waterGlacier","waterGSoilAndGlac")
  csv_eol <- ifelse(grepl("mingw", R.Version()$platform), "\n", "\r\n")
  write.csv(simresult, file = normalizePath(file.path(pathRes,nameRes,".csv"),mustWork = FALSE),row.names = FALSE,eol=csv_eol)

  results <- list(simulation = simresult)

    } else stop("NULL arguments in parameters")


  return(results)

}
