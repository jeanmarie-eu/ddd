#' Simulation
#'
#' Simulation of the hydrological model
#' @param timePeriod data timeserie and its related information
#' @param q runoff timeserie
#' @param precip precipitation timeserie
#' @param temp temperature timeserie
#' @param scaob observed snow coverage timeserie
#' @param snow snow
#' @param snowReservoir snowReservoir
#' @param soilMoisture soilMoisture
#' @param soilWater soilWater
#' @param UH UH
#' @param soilDischarge soilDischarge
#' @param ddistAll ddistAll
#' @param groundwater groundwater
#' @param models models
#' @param pathResults path to save the results
#' @param saveDate date to be saved if any
#' @keywords ddd
#' @export
#' @examples
#' \dontrun{
#' do.simTS()
#' }
do.simTS <-function(timePeriod,
                  q,
                  precip,
                  temp,
                  scaob,
                  snow,
                  snowReservoir,
                  soilMoisture,
                  soilWater,
                  UH,
                  soilDischarge,
                  ddistAll,
                  groundwater,
                  models,
                  pathResults,
                  saveDate){


if ( (!is.null(timePeriod)) && (!is.null(q)) && (!is.null(precip)) &&
     (!is.null(temp)) && (!is.null(scaob)) && (!is.null(UH)) &&
     (!is.null(snow)) && (!is.null(snowReservoir)) && (!is.null(soilMoisture)) &&
     (!is.null(soilWater)) && (!is.null(soilDischarge)) && (!is.null(ddistAll)) &&
     (!is.null(groundwater)) && (!is.null(models)) && (!is.null(pathResults))
     ) {

  # simulation to be recorded
  simresult<-c()

  #for each timesteps of the period of simulation
  for (i in 1:timePeriod$nbStep){

     # Input variables
     hprecip <- do.precipLZ(v=precip[i],modelPrecipLZ=models$modelPrecipLZ)
     htemp   <- do.tempLZ(v=temp[i],modelTempLZ=models$modelTempLZ)

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
       stateX <- do.stateX(Timeresinsec      = timePeriod$Timeresinsec,
                            precipX           = hprecip,
                            tempX             = htemp,
                            scaobX            = scaob,
                            snowX             = snow,
                            soilMoistureX     = soilMoisture,
                            snowReservoirX    = snowReservoir,
                            soilWaterX        = soilWater,
                            UH                = UH,
                            ddistAllX         = ddistAll,
                            soilDischargeX    = soilDischarge,
                            groundwaterX      = groundwater,
                            modelPrecipLZ     = models$modelPrecipLZ,
                            modelTempLZ       = models$modelTempLZ,
                            modelSnow         = models$modelSnow,
                            modelSoilMoisture = models$modelSoilMoisture,
                            modelSoil         = models$modelSoil,
                            modelBog          = models$modelBog,
                            modelET           = models$modelET,
                            modelSoilWater    = models$modelSoilWater,
                            modelArea         = models$modelArea,
                            modelLayer        = models$modelLayer)

       # Updating variables
       snow             <- stateX$snow
       snowReservoir    <- stateX$snowReservoir
       soilMoisture     <- stateX$soilMoisture
       soilWater        <- stateX$soilWater
       soilDischarge    <- stateX$soilDischarge
       ddistAll         <- stateX$ddistAll
       groundwater      <- stateX$groundwater

       # Save simulation
       simresult <- rbind(simresult,
                          c(timePeriod$dateTS[i,1],
                            timePeriod$dateTS[i,2],
                            timePeriod$dateTS[i,3],
                            timePeriod$dateTS[i,4],
                            mean(hprecip,na.rm=TRUE),
                            mean(htemp,na.rm=TRUE),
                            q[i],
                            soilDischarge$qsimX[1],
                            snowReservoir$middelsca,
                            snowReservoir$snomag,
                            (groundwater$M-soilDischarge$D),
                            soilDischarge$D,
                            soilWater$G,
                            soilWater$Ea,
                            soilWater$X,
                            soilWater$Gbog,
                            soilWater$Eabog,
                            soilWater$Xbog,
                            soilMoisture$Z,
                            soilMoisture$waterGlaciers,
                            (soilMoisture$waterGlaciatedSoil+soilMoisture$waterGlaciers)
                          ))

     } else {

       # WARNING:
       # No computation when either the precipiatation or the temperature is missing
       simresult <- rbind(simresult,
                          c(timePeriod$dateTS[i,1],timePeriod$dateTS[i,2],timePeriod$dateTS[i,3],timePeriod$dateTS[i,4],
                            NA,NA,q[i],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
                          )

     }

     if (!is.null(timePeriod$indiceSave)) {
       if (i == timePeriod$indiceSave) {
         pathDate <- normalizePath(file.path(pathResults,saveDate),mustWork = FALSE)
         dir.create(pathDate,showWarnings = FALSE)
         do.call("save", list(obj="snow", file=normalizePath(file.path(pathDate,"snow.rda"),mustWork = FALSE)))
         do.call("save", list(obj="snowReservoir", file=normalizePath(file.path(pathDate,"snowReservoir.rda"),mustWork = FALSE)))
         do.call("save", list(obj="soilMoisture", file=normalizePath(file.path(pathDate,"soilMoisture.rda"),mustWork = FALSE)))
         do.call("save", list(obj="soilWater", file=normalizePath(file.path(pathDate,"soilWater.rda"),mustWork = FALSE)))
         do.call("save", list(obj="soilDischarge", file=normalizePath(file.path(pathDate,"soilDischarge.rda"),mustWork = FALSE)))
         do.call("save", list(obj="ddistAll", file=normalizePath(file.path(pathDate,"ddistAll.rda"),mustWork = FALSE)))
         do.call("save", list(obj="groundwater", file=normalizePath(file.path(pathDate,"groundwater.rda"),mustWork = FALSE)))
       }
     }


  }
  colnames(simresult)<-c("year","month","day","hour","mhprec","mhtemp","qobs","qsim","middelsca","snowmag","M-D","D","G","Ea","X","Gbog","Eabog","Xbog","Z","waterGlacier","waterGSoilAndGlac")
  write.csv(simresult, file = normalizePath(file.path(pathResults,"simulation.csv"),row.names = FALSE)

  results <- list(simulation = simresult)


    } else stop("NULL arguments in parameters")


  return(results)

}
