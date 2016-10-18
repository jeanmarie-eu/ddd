library(ddd)


#####################################################
#####################################################
## READ PARAMETERS
#####################################################
#####################################################

# paths
pathParam     <- paste0(.libPaths()[1],"/dddModel/data/")
pathPrecip    <- paste0(.libPaths()[1],"/ddd/data/")
pathTemp      <- paste0(.libPaths()[1],"/ddd/data/")
pathDischarge <- paste0(.libPaths()[1],"/ddd/data/")
#pathResults   <- "/your/path/"

# Dates, etc
fromPeriod="20160802"
toPeriod="20160929"
timeResolution="hourly" #"daily", "three-hourly", "hourly",...
catchment <- "Narsjo"

# Initial conditions
initialCondition <- NULL

# Dates to be saved
saveDate <- "20160823"

#catchment <- "16.75"
#modstate <- 0     #Run model with state: modstate = 1 , no state: modstate = 0
#savestate <- 0    #Save  state: yes = 1, no = 0




##############################################################################################
##############################################################################################
## DATE related informamtion
## Time resolution in seconds
## nbStep
## date timeserie
##############################################################################################
##############################################################################################
time <- ddd.date(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod,format="YY,MM,DD,HH")
# OUTPUT (list)
# Timeresinsec
# nbStep
# dateTS


##############################################################################################
##############################################################################################
## GET METEOROLOGICAL AND RUNOFF VALUES
##############################################################################################
##############################################################################################
precAndTemp <- read.csv(paste0(pathPrecip,"8880.csv"),header=TRUE)
whAndq <- read.csv(paste0(pathDischarge,"dischargeNarsjo.csv"),header=TRUE)
precAndTemp[precAndTemp==-999]<-NA
whAndq[whAndq==-999]<-NA

# precipitation
precip <- precAndTemp[,5]

# temperature
temp <- precAndTemp[,6]

# snow coverage from satellite
scaob   <- NA

# runoff
q <- whAndq[,6]


################################
#scaobx <- -9999
#dag <- as.Date(paste(ptqinn$yr[i],"-",ptqinn$mnt[i],"-",ptqinn$day[i],sep=""))
#datoform <- format(dag,"%Y.%m.%d")
#if(length(which(scaob$dagsdato == datoform)) > 0){
#  target <- which(scaob$dagsdato == datoform)
#  if(scaob[target,3] < 5.0){
#    scaobx <- scaob[target,idim+3]/100
#  }
#}
################################



##############################################################################################
##############################################################################################
## PARAMETERS READING AND MODELS BUILDING-UP
## - Read parameters files
## - filling-in the models with the right parameters
##     - modelk
##     - modelSoilMoisture
##     - modelSoilWater
##     - modelSoil
##     - modelSaturation
##     - modelLayer
##     - modelRiver
##     - modelBog
##     - modelMAD
##     - modelSnow
##     - modelTempLZ
##     - modelPrecipLZ
##     - modelArea
##############################################################################################
##############################################################################################

# READ PARAMETERS AND COMPUTE SOME
prm <- ddd.param(Timeresinsec=time$Timeresinsec, path=pathParam)


###########################---------------------->

modstate <- 0     #Run model with state: modstate = 1 , no state: modstate = 0
savestate <- 0    #Save  state: yes = 1, no = 0
UP <- 0           # if 1: update from sattelite derived SCA; if 0: do not



#ptqinn <- read.table(ptq.file)
#navn <- list("yr","mnt","day","hr","p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","q") # i tilfelle av h�ydesoneverdier
#names(ptqinn)[1:25] <-navn[1:25]                                                                                                                                # i tilfelle av h�ydesoneverdier

days <- time$nbStep
savest <-  days - 75    #save state at this time (a value of i) , state file "state_dd_mm_yyyy_hh"

#-------------------------------------preprocess start------------------------------------------
#-----------------------------------------------------------------------------------------------



#####################VARIABLES FOR 10 ELEVATION ZONES###########################################
isoil<- vector("numeric",10)# precipitation and snowmelt from the elevation zones
gwgt <- vector("numeric",10)# weights for glaciermelt pr each elevation zone
swgt <- vector("numeric",10)# weights for input to soils pr each elevation zone NB bogs are not part of this yet
gisoil <- vector("numeric",10)#glaciermelt from the elevation zones
spd<- vector("numeric",10)
swe_h <- vector("numeric",10)#SWE pr altitude level
wcd<- vector("numeric",10)
sca<- vector("numeric",10)
nsno<- vector("numeric",10)
alfa<- vector("numeric",10)
ny <- vector("numeric",10)
hprecip<- vector("numeric",10)
htemp<- vector("numeric",10)
hfelt<- vector("numeric",10)
snowfree <- vector("numeric",10)  #Indicator variable 1 is sca  less than gca 0 if sca > gca
tempvec <- vector("numeric",5) # temperature vector  to save in statefile



#initialisering
isoil<- 0.0
gisoil[1:10] <-0.0
snowfree[1:10] <- 0.0
spd[1:10]<- 0.0
wcd[1:10]<- 0.0
sca[1:10]<- 0.0
nsno[1:10]<- 0.0
alfa[1:10]<- 0.0
ny[1:10] <- 0.0
hprecip[1:10]<- 0.0
htemp[1:10]<- 0.0
hfelt[1:10]<-0.0

#Innlesing av parametere
hfelt <-prm$hfelt
pro <-prm$Ws
#vp1 <-prm$vp1
#vp2 <-prm$vp2
#vt1 <-prm$vt1
#vt2 <-prm$vt2
#hst1 <-prm$hst1
#hst2 <-prm$hst2
hfeltmid <-prm$hfeltmid
tgrad <-prm$Tlr
pgrad <-prm$Plr
pkorr <-prm$Pc
skorr <-prm$Sc
TX <-prm$TX
TS <-prm$TS
CX <-prm$CX
CFR <-prm$CFR
CGLAC <-prm$CGLAC
a0 <-prm$a0
d <-prm$d
Timeresinsec <-time$Timeresinsec
MAD <-prm$MAD
totarea <-prm$totarea
maxLbog <- prm$maxLbog
midLbog <-prm$midLbog
bogfrac <-prm$bogfrac
zsoil <-prm$zsoil
zbog <-prm$zbog
NoL <-prm$NoL
cea <- prm$cea
R <-prm$R
Gshape <-prm$Gsh
Gscale <-prm$Gsc
GshInt <-prm$GshInt
GscInt <-prm$GscInt
cvHBV <- prm$CV
dummy3 <- prm$dummy3
rv <-prm$rv
midFl <-prm$midFL
stdFl <-prm$stdFL
maxFl<-prm$maxFL
maxDl <-prm$maxDL
gtcel <- prm$gtcel
midDL <- prm$midDL
glacfrac <- prm$glacfrac
midGl <- prm$midGl
stdGl <- prm$stdGl
maxGl <- prm$maxGl
#g1    <- prm$g1
#g2    <- prm$g2
#g3    <- prm$g3
#g4    <- prm$g4
#g5    <- prm$g5
#g6    <- prm$g6
#g7    <- prm$g7
#g8    <- prm$g8
#g9    <- prm$g9
#g10   <- prm$g10

tprm <- c(CX,TS,TX,pkorr, pro, tgrad, pgrad, cea) #parameters of your choosing to be written to the R2 file

ddist <- vector("numeric", NoL)
ddistx <- vector("numeric", NoL)
ddistxtemp <-vector("numeric", NoL)
aktMag <- vector("numeric", NoL)

#Constants
lpdel <- prm$lpdel
R <- prm$R    #from litterature (Skaugen and Onof, 2014)
hson <- prm$nbLevelZone   #elevation zones

#Merging assumed Glacier river network  with observed river network, assume independent normal distributions
maxFl <- maxFl+ maxGl
midFl <-midFl + midGl  #mean of flowlength distribution
stdFl <-stdFl + stdGl

area <-prm$nobognoglacarea    #ikke myr- eller breareal Disse tre landskapsklasser har forskjellig distance distributions
area2 <- prm$slopesriverarea           #area in which we have hillslope process and rivernetwork processes
bogarea <- prm$bogarea               #myrareal
glacarea <-prm$glacarea             #breareal
elevarea <- prm$elevarea               #area pr elevationzone
gca <- prm$gca #c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10) # fraction of glacier pr elevation zone, fraction of glacier covered area pr elevation zone
soilca <-1-gca
#determines weights used to estimate the average glaciermelt. Finds the fraction of glaciers in each elevation zone in relation to total glacierarea
gwgt<-prm$gwgt
swgt<-prm$swgt #Finds the fraction of soils (and bogs) in each elevation zone in relation to total soil (and bog) area


#Subsurface celerities----------------------------------------------------------------------
k <- vector("numeric",NoL)
ddist<- vector("numeric",NoL)

k[1:NoL]<- 0.0
ddist[1:NoL] <- 1/NoL

dp <- 1/(NoL-1) #Oveland flow level (nol=1], fixed celerity and extremely high capacity(2000 mm)
probvec <- vector("numeric", NoL)            #alle leves and overland flow level
for(i in 1:(NoL-1))probvec[i+1] <-i*dp-dp/2  #celerities are estimated at center of level, hence dp/2
probvec <- 1-probvec
probvec[1] <-gtcel   #Quantile in celerity distribution for overland flow. Now to be calibrated or fixed

k[1:NoL] <-qgamma(probvec[1:NoL],Gshape,1/Gscale)*midDL/Timeresinsec
#0.021426756 0.019533547 0.009983096 0.005265099 0.001820293
# JML PACKAGE: models$modelLayer$speed
# IDENTIC VALUES

bogspeed <- k[1]*1.0    #Same celerity as overland flow
# models$nodelBog$speed
# IDENTIC VALUE

antHorlag <- trunc(maxDl/(k[NoL]*Timeresinsec))+1  # +1 includes the final day ogs�NB only in soils part
# 245
# JML PACKAGE: models$modelLayer$speed[NoL]
# IDENTIC VALUE

UHlayer <- matrix(0.0, ncol=antHorlag,nrow=NoL)    # UHlayer [1,] is the fastest level of saturation

antBogsteps <- trunc(maxLbog/(bogspeed*Timeresinsec))+1 #Number of timsteps delay in Bogs
# JML PACKAGE: models$modelBog$nbStepsDelay
# IDENTIC VALUE

UHbog <- vector("numeric",antBogsteps)                  #Vector for Bog unit hydrograph

D <- MAD*Timeresinsec/(area2) # Initial moisture in mm, derived from the approx. mean annual dicharge (MAD)
# 8.530162e-05
#Estimate Unithydrographs for hillslopes based on celerities and distance distributions,exponentially distributed

AntL <- length(k)
layerUH <- matrix(0.0, ncol=antHorlag, nrow =AntL)
e1ascl <- vector("numeric", AntL)
nodays <- vector("numeric", AntL)
#Scale the moments of the distribution
e1ascl[1:AntL] <-(midDL/k[1:AntL])/Timeresinsec #gir antall d�gn, oppl�sning i d�gn antall d�gn/Timreresinsec det tar for middelavstanden
nodays[1:AntL] <- trunc(maxDl/k[1:AntL]/Timeresinsec)+1 #runder opp til antall dager
#  21  23  45  85 245
# JML PACKAGES: models$modelLayernbStepsDelay
# IDENTIC VALUE

#stores the pdf #exponentiellt UH  UH er 1/e1ascl*exp(-(1/e1ascl)*t)/sum_for_alle_t(1/e1ascl*exp(-(1/e1ascl)*t))
for (i in 1: AntL)layerUH[i,1:nodays[i]] <- zsoil+(1-zsoil)*dexp(0:(nodays[i]-1),1/e1ascl[i])
layerUH[1:AntL,] <- layerUH[1:AntL,]/rowSums(layerUH[1:AntL,])
# -> cf layerUH.txt
# JML PACKAGES: UH$layerUH
# IDENTIC VALUES

#Rutine calculates  runoff in mm/timestep
AntL <- length(ddist)
EunHyd <- vector("numeric",antHorlag) #Superposisjonert UH fra alle hastigheter, skal vektes med ddist, fuktfordelingen
faktor <- ddist[1:AntL]*layerUH[1:AntL,]
EunHyd <- colSums(faktor)
initUHslopes <- EunHyd
# -> cf initUHslopes.txt
# PACKAGES JML: IDENTIC VALUES

#Scale the moments of the distribution
escl <-(midLbog/bogspeed)/Timeresinsec #Parameter i "time travel distrbution"

#stores the pdf #exponentiellt UH  UH is 1/e1ascl*exp(-(1/e1ascl)*t)/sum_for_alle_t(1/e1ascl*exp(-(1/e1ascl)*t))
UHbog[1:antBogsteps] <- zbog+(1-zbog)*dexp(0:(antBogsteps-1),1/escl)
#renormalise sucha that the sum is unity
UHbog[1:antBogsteps] <- UHbog[1:antBogsteps]/sum(UHbog)
#[1] 0.11972173 0.09305354 0.07500926 0.06280010 0.05453912 0.04894956
# [7] 0.04516754 0.04260855 0.04087707 0.03970552 0.03891282 0.03837647
#[13] 0.03801356 0.03776800 0.03760186 0.03748944 0.03741337 0.03736191
#[19] 0.03732708 0.03730352
# JML PACKAGES: IDENTIC VALUES


if(bogarea <=0){
  UHbog <-1
}

#-------------------------------start UHriv-------------------------------------
#river unit hydrograph #hastighet i elv,   For river, normally distributed
#number of time units in river routing, rounded up. If set equal to one day, time is actually less

nodaysRiv <- trunc(maxFl/rv/Timeresinsec)+1
#11
timeres <-seq(0,(nodaysRiv-1),1)# hourly resolution on riverrouting hydrogram

#Scale the moments of the distribution
midFlscl <-midFl/rv/Timeresinsec
#4.662329
stdFlscl <-stdFl/rv/Timeresinsec
 #3.21016

UHriver <- vector("numeric", nodaysRiv)
UHriver[1:nodaysRiv] <- 0.0
UHriver <- dnorm(timeres,midFlscl,stdFlscl)#lager pdf'en

#scale to give unity sum = 1
if(nodaysRiv == 1) UHriver <-1.0 #implies no river routing

UHriver<-UHriver/sum(UHriver)
#[1] 0.04741637 0.07101393 0.09651956 0.11905387 0.13326871 0.13538461
#[7] 0.12481503 0.10442895 0.07929236 0.05463844 0.03416817
# JML PACKAGES: UH$UHriver
# IDENTICAL VALUES

noDT <- length(UHriver)


#-------------------------------end UHriv-------------------------------------
#Convolve UH of hillsopes and bogs with that of the river
#convolusjon, total UH for slopes and river. Rout discharge from glaciers,
#soils and bogs through river network
#convolusjon, total UH for bogs and river, Fixed, does
#not change, is either on or off .Rout discharge from glaciers, soils and bogs through river network
initTOTUH <-  convolve(initUHslopes,UHriver, type="o")
#-> cf initTOTUH.txt
# PACKAGE JML: IDENTICAL VALUES


BogRiverUH <- convolve(UHbog,UHriver, type="o")
#-> cf BogRiverUH.txt
# PACKAGE JML: IDENTICAL VALUES

nodaysvector <- vector("numeric",NoL)   #vector med number of timesteps for each level
nodaysvector[1:NoL] <- trunc(maxDl/(k[1:NoL]*Timeresinsec))+1   #calculates timestep for each level
#21  23  45  85 245

vectorlengde <- antHorlag+noDT-1
qsimutx <-vector("numeric",vectorlengde)  #vector for runoff i m3/s
qsimut <-vector("numeric",vectorlengde)   #vector for runoff in mm
qBogut <-vector("numeric",antBogsteps)    #vector for runoff from wetlands bogs
QRivx <- vector("numeric",vectorlengde)   #all qsimutvectors are stored antHorlag timesteps back

#Grroundwater, one dimensional=horisontalt/days; 2dim levels, 1 fastest, NoL slowest#
Layers <- matrix(0.00, ncol=antHorlag,nrow=NoL)

#-----------------------------------start----------------------------------------------------
mLam <- GshInt*GscInt
# 0.05922
varLam <- GshInt*(GscInt)^2 #Yevjevich p.145
# 0.00213192
cvLam <-varLam^0.5/mLam
# 0.7796812
meanIntk <- mLam*midDL/Timeresinsec#middel hastighet beregent med Integrated Celerity
# 0.00621481

### WARNING ROUND IS DIFFERENT FROM TRUNC. JML ALWAYS USE TRUNC
### HOMOGENEIZATION: TRUC HAS BEEN ALWAYS USED
#antBox <- round(maxDl/(meanIntk*Timeresinsec))+1 #lengde p� gjennomsnittsUH
# 73
# JML PACKAGE: VALUE = 72 BECAUSE TRUNC INSTEAD OF ROUND

antBox <- trunc(maxDl/(meanIntk*Timeresinsec))+1 #lengde p� gjennomsnittsUH ### WARNING: ROUND IS DIFFERENT OF TRUNC. JML ALWAYS USE TRUNC
# 72
# JML PACKAGE: OK

UH_MAD <- vector("numeric", antBox)


#UH_MAD <- makesingleUH_nugget(antBox,meanIntk, Timeresinsec, midDL, maxDl, 0)
UHvec <- vector("numeric",antBox)

escl <-(midDL/meanIntk)/Timeresinsec    #Bug fixing!!!! her stod det k!!!
UHvec[1:antBox] <- 0+(1-0)*dexp(0:(antBox-1),1/escl)
UHvec[1:antBox] <- UHvec[1:antBox]/sum(UHvec)
UH_MAD <- UHvec
# -> cf UH_MAD.txt

#Enhetshydrogram for MAD
sRes <-  vector("numeric", antBox) # saturation sum
#qRes <-  vector("numeric", antBox) #runoff sum

StSt<- (1000*MAD*Timeresinsec)/(totarea*1) # Steady state Input eq. output in mm
# 0.07506542

#qRes <- StSt*UH_MAD
#for( i in 2:(antBox-1))
#{
#  qRes[i:antBox] <- qRes[i:antBox] + StSt*UH_MAD[1:(antBox-(i-1))]
#}
sRes[1] <-0
sRes[2:antBox] <- StSt*UH_MAD[2:antBox]

for(i in 3: antBox)
{
  sRes[i:antBox] <- sRes[i:antBox] + StSt*UH_MAD[i:antBox]
}
mRes <- sum(sRes)
# 1.153287

Fact <- mLam/mRes
stdRes <- (varLam/Fact^2)^0.5 #se Haan p.51

GshRes <-mRes^2/stdRes^2
GscRes <-stdRes^2/mRes

MLev <-seq(1/(NoL-1),1,1/(NoL-1)) #Quantiles  to calculate reservoir levels
MLev[NoL-1] <- gtcel # quantile for start overland flow
Res_prob <- vector("numeric", NoL-1)

#calculates the reservoir levels associated with quantiles Middel er GshRes*GscRes
Res_prob <- qgamma(MLev,GshRes,1/GscRes)
# 0.4952557 0.9298602 1.5716238 2.3499924
# JML PACKAGES: IDEM

ssRes1 <- vector("numeric", NoL)
ssRes1[1:NoL] <- 0.0
ssRes1[1] <-2000 # capacity of overland flow level

for (i in 2:(NoL-1))
{
  ssRes1[i] <-Res_prob[NoL-i+1]-Res_prob[(NoL-i)]
}
ssRes1[NoL] <- Res_prob[1]# capasity for the first slowest level

Magkap <- ssRes1[1:NoL]
# 2000.0000000    0.7783686    0.6417636    0.4346045    0.4952557
# JML PACKAGES: IDEM

GST <- Res_prob[NoL-1]

#browser()
# INITIALIZING RESULTS
qberegn <-vector("numeric",days) #for � beregne R2
simresult <- matrix(0.0, days,21)# matrix which into the results are written
Layersres<-array(0,dim=c(time$nbStep,NoL,nodaysvector[NoL]))
Magkapres <- matrix(0.0, days,(4+NoL))# matrix which into the results are written
ddistxres <- matrix(0.0, days,(4+NoL))# matrix which into the results are written
ddistres <- matrix(0.0, days,(4+NoL))# matrix which into the results are written
Stres <- matrix(0.0, days,(4+1))# matrix which into the results are written

if(q[1] > 0) {
  MAD1 <- q[1]
} else MAD1 <- MAD

D <- MAD1*Timeresinsec/(area2)
# 0.0001377552
# PACKAGE JML: IDENTICAL VALUE

qsimutx <- ((D*area)/Timeresinsec)*initTOTUH  #discharge in m3/s
# -> cf qsimutx.txt
# JML PACKAGES: inside function ddd.init.soilDischarge()
# IDENTICAL VALUES

####################################
####################################
####################################

qBogut <- ((D*bogarea)/Timeresinsec)* BogRiverUH #discharge in m3/s from bogs routed through the river
# cf qbogut.txt

#Here the contributions from the bogs qBogut, to qsimutx
qsimutx[1:antBogsteps] <- qsimutx[1:antBogsteps]+qBogut[1:antBogsteps]
#qmmx <- (qsimutx[1]*Timeresinsec*1000/totarea)  #in mm/day

#Middelh�yde metstasjoner
midmetp <- prm$midmetp
midmett <-  prm$midmett

#Lognormal snow
#snofrac <-vector("numeric",9)
#xnorm <- vector("numeric",9)
#sdist <- vector("numeric",9)

#snofrac[1:9]<- c(0.01,0.04,0.1,0.2,0.3,0.2,0.1,0.04,0.01) #arealfraction,  sums to unity
#xnorm[1:9]<-c(2.7,1.88,1.3,0.85,0.0,-0.85,-1.3,-1.88,-2.7)
#spdist <- 20.0    #threshold snow mm  for distributing snow lognormally
#xvol <- 0.0
#sdist[1:9] <- exp(xnorm[1:9]*cvHBV)
#xvol <- sum(sdist[1:9]*snofrac[1:9])
#sdist[1:9] <- sdist[1:9]/xvol

#Initiering of states in the snowroutine
#spdvec <- vector("numeric",9)
#wcdvec <- vector("numeric",9)
#scavec <- vector("numeric",9)
#spdtt  <- vector("numeric",9)
#Gamma snow
unitsnow <- 0.1
n0 <- unitsnow*a0
# 2.2941

if(modstate ==1) load(tilst.file)  #Run with state

#initialverdier
if (modstate==0)
{  sm <- 0.2*GST              #  Soilmoisture

   smbog <-0.95*GST           #  Saturation of Bogs

   # groundwater initial value
   for (i in 1:NoL) Layers[i,1:nodaysvector[i]] <- 1.0/nodaysvector[i]
   # --> cf Layers.txt
   # JML PACKAGES: IDEM

   QRivx[1:vectorlengde] <- qsimutx                             #discharge in m3/s
   spd[1:10]<- 0.0                                              # gamma SD
   wcd[1:10]<- 0.0
   sca[1:10]<- 0.0
   nsno[1:10]<- 0.0
   alfa[1:10]<- 0.0
   ny[1:10] <- 0.0
   #dspd <- matrix(0.0,10,9)                                     #Snow water equivalent (SWE) Lognormal
   #dwcd <- matrix(0.0,10,9)                                     #liquid water in snowpack    Lognormal
   #dsca <- matrix(0.0,10,9)                                     #snowcovered area            Lognormal
   totdef <-2.0
}
isoilx <- 0

############################################################################
#                                                                          #
#simulation starts here.............. days is no. time steps               #
############################################################################
for (i in 1:days)
{

  # Input variables
  hprecip <- ddd.precipLZ(v=precip[i],modelPrecipLZ=models$modelPrecipLZ)
  # [1] 0.19605 0.19688 0.19795 0.19882 0.19930 0.19995 0.20058 0.20137 0.20259
  # [10] 0.20803
  htemp   <- ddd.tempLZ(v=temp[i],modelTempLZ=models$modelTempLZ)
  # [1]  25.806375  22.758200  18.828625  15.633550  13.870750  11.483625
  # [7]   9.169950   6.268675   1.788225 -18.190175

  if (!is.na(htemp) && (!is.na(hprecip)) ){
    meanprecip <-mean(hprecip[1:hson])
    meantemp <- mean(htemp)


      for(idim in 1:hson)#elevation zones
      {

        if(htemp[idim] > TX)
        {
          PR <-hprecip[idim]*pkorr
          PS <- 0.0
        }
        else
        {
          PS <-hprecip[idim]*skorr
          PR <-0.0
        }

        MW <- CX*(htemp[idim]-TS)        # snow melt, degreeday melting
        MWGLAC <- CGLAC*(htemp[idim]-TS) #glacier melt, degreeday melting
        if(htemp[idim] < TS)
        {
          MW <- MW*CFR #negative melt is turned into snow
          MWGLAC <-0.0
        }

    #--------------------------------------------------------------------------------------------------------------
       #Calls snowdistribution routine

        #scaobx <- -9999
        #dag <- as.Date(paste(ptqinn$yr[i],"-",ptqinn$mnt[i],"-",ptqinn$day[i],sep=""))
        #datoform <- format(dag,"%Y.%m.%d")
        #if(length(which(scaob$dagsdato == datoform)) > 0)
        #{
        #  target <- which(scaob$dagsdato == datoform)
        #  if(scaob[target,3] < 5.0)
        #  {
        #    scaobx <- scaob[target,idim+3]/100
        #  }
        #}

       #frasno <- snoHBV(PR,PS,MW,pro,sdist,spdist,snofrac,spdvec,wcdvec,scavec)
       frasno <-dddSnow.snogamma(PR,PS,MW,sca[idim],scaobx=NA,spd[idim],wcd[idim],pro,nsno[idim],alfa[idim],ny[idim],a0,n0,a0,d, UP)
        gisoil[idim] <- MWGLAC
        isoil[idim] <- frasno$isoil  #precipitation and snowmelt
         isoil[idim] <- frasno$isoil  #precipitation and snowmelt
         spd[idim] <- frasno$spd
         wcd[idim] <- frasno$wcd
         sca[idim] <- frasno$sca
    #     print(paste("ppa (sca) fra frasno:", sca[idim],"idim",idim))
         nsno[idim] <- frasno$nsno
         alfa[idim] <- frasno$alfa
         ny[idim]<-frasno$ny
         if (spd[idim] > 20000)
         {
          spd[idim] <-20000
          print("justert sn�mag, vi lager isbreer. Denne speeder opp kalibreringen")
         }
          if(sca[idim] < gca[idim]) {
             snowfree[idim] <- 1.0
          } else snowfree[idim] <-0.0#test for glaciermelt,  no melt if snowcovered, snowfree[idim] =0.0
      } #for 1 (idim) to hsno elevation zones
      #> gisoil
      #[1] 141.935063 125.170100 103.557438  85.984525  76.289125  63.159938
      #[7]  50.434725  34.477713   9.835238   0.000000
      #> isoil
      # [1] 0.19605 0.19688 0.19795 0.19882 0.19930 0.19995 0.20058 0.20137 0.20259
      # [10] 0.00000
      #> spd
      # [1] 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.7
      #> wcd
      # [1] 0 0 0 0 0 0 0 0 0 0
      #> sca
      # [1] 0 0 0 0 0 0 0 0 0 1
      #> nsno
      # [1]  0  0  0  0  0  0  0  0  0 17
      # > alfa
      # [1] 22.94100 22.94100 22.94100 22.94100 22.94100 22.94100 22.94100 22.94100
      # [9] 22.94100  1.44194
      # > ny
      # [1] 2.294100 2.294100 2.294100 2.294100 2.294100 2.294100 2.294100 2.294100
      # [9] 2.294100 0.144194
      # > snowfree
      # [1] 0 0 0 0 0 0 0 0 0 0

      #Snowreservoir
      snomag <- mean(sca[1:hson]*spd[1:hson]) #mean catchment SWE ,must multiply with SCA to have arealvalues
      swe_h[1:hson] <- sca[1:hson]*spd[1:hson] #SWE pr. elevation zone
      middelsca <- sum(sca[1:hson])/hson
      snofritt <- 1-middelsca
      # > snomag
      # [1] 0.17
      # > swe_h
      # [1] 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.7
      #> middelsca
      # [1] 0.1
      #> snofritt
      # [1] 0.9

      #Estimate isoil from all elevation zones
      misoil <-sum(isoil*swgt)  #snowmelt and rain on soils. isoil is weighted by the fraction of soils  in relation to soil an bog area pr elevation band
      m_r_onglac <- sum(isoil*gwgt) #snowmelt and rain from glaciated area. isoil is weighted by the fraction of glaciers pr elevation band in relation to total glaciated area
      #glacier melt (gisoil) in mm this timestep. m_r_onglac + outglac is total output from glacier
      outglac <-sum(gisoil[1:hson]*gwgt[1:hson]*snowfree[1:hson])#gwgt because it is going to be scaled by glacfrac later on
      #> misoil
      # [1] 0.160133
      #> m_r_onglac
      # [1] 0
      # > outglac
      # [1] 0


      #if(i == 330)browser()
      #computes evapotranspiration as a function of SCA
      if(snofritt > 0.05) {
        eatemp <- sum(htemp[1:round(hson*snofritt)])/round(hson*snofritt)
      } else eatemp <- sum(htemp[1:hson])/hson # mean of temperature at snowfree elevation bands
      #call to the soil- and groundwater routine
      #> eatemp
      # [1] 13.95644

      toMSD <- misoil*(1-(glacfrac))+glacfrac*(outglac+m_r_onglac) # input from rain, snow and glaciers
      #> toMSD
      # [1] 0.160133
      #########################
      # WARNING JM FIND 0.160133
      # isoil ok, VERIFIER swgt ???
      ########################

    ################################################################################
    # Groundwater states                                                            #
    ################################################################################
      Unsat <-max(totdef,0)#state of unsaturated zone, it can etiher be zero (complete saturation) or positive, the deficit
      # > Unsat
      # [1] 0.6027409

      R <- 0.3
      outx <- 0.0

      if (eatemp > 0.0){
        ea<-cea*eatemp*((GST-Unsat+sm)/GST)*(1.0-middelsca)
      } else {
        ea <-0.0
      }
      #>  ea
      # [1] 1.0884


      sm <- sm-ea
      if (sm < 0) sm <- 0
      #> sm
      # [1] 0

      # rat  <- (G+Z)/D
      rat <- ifelse(Unsat > 0, (sm+toMSD)/Unsat, 1) #taking into account that D can be zero or even negative (overland flow), complete saturation.
      #  totdef m� tillates negativ da dette betyr overland flow.
      #rat <- (sm+toMSD)/totdef #denne blir null hvis totdef =0, dvs full metning FEIL!
      #>       rat
      # [1] 0.2975557


      if(rat > R){
        # X  <- (G+Z)-R*D
        outx <-(sm+toMSD)-R*Unsat
    #    outx <-(rat-R)*totdef #Excess water released to runoff
        sm <-R*Unsat
      } else {
        outx <- 0
        sm <- sm+toMSD
      }
      #>     sm
      # [1] 0.179349
      #> outx
      #[1] 0


      #   Bog_routine <- function(isoil,temp,sncov,Zbog,cea,M)

      if (eatemp > 0.0){
        #eabog<-cea*temp*((M-Zbog)/M)*(1.0-sncov)
        eabog<-cea*eatemp*(smbog/GST)*(1.0-middelsca)
      } else {
        eabog <-0.0
      }
      #> eabog
      # [1] 1.094912e-06



      #------------------------------------------------------------------------
      #     Updating soilmoisture sm caused by evapotranspiration
      #------------------------------------------------------------------------

      smbog <- smbog-eabog
      if (smbog < 0) smbog <- 0
      #> smbog
      # [1] 6.627927e-07

      #browser()
      #------------------------------------------------------------------------------------------------------
      # Distribute water to horisontal slabs, i,.e estimating retention of moistureinput to sm and runoff
      # The theory is that saturated and unsaturated sone share the same volume, and are hence the complement
      # of each other
      #------------------------------------------------------------------------------------------------------
      # smx/GSTdef is if this fraction that determines the release of input to runoff (the layers)


      # rat  <- (G+Z)/D
      Bograt <- (smbog+misoil)/1
      #>       Bograt
      # [1] 0.1793497

      # i(f rat > R)
      if(Bograt > GST) {
        # X  <- (G+Z)-R*D
        outbog <-(smbog+misoil)-GST*1 #Excess water released to runoff
        #toqestbog <- BX
        smbog <-GST
      } else {
        outbog <- 0
        smbog <- smbog+misoil
      }
      #>       smbog
      # [1] 0.1793497
      #> outbog
      # [1] 0

      #if(i==500)browser()
      #Here we have to distribute the moisture from the soil- and groundwater routine.
      #The distribution to the different levels will be a function of their antHorlag timelags back
      #This implies that for each level, we will need to define a vector where each reponse is kept antHorlag back
      #The storage capacity for each of the layers are one/NoL'th to the total groundwater storage capacity (GST or M) (water balance wise)
      #So wehave a total storage capacity GST wich is a tuning parameter.

      ddist[1:NoL] <-0.0 #Initializes distribution of input to levels
      ddistx[1:NoL] <- 0.0
      aktMag[1:NoL] <- 0.0

      #Below are the states (in mm) for each saturation level
      for (j in NoL:1)
      {
        #state after this timestep water is gone. amount of water  in mm, minus current timestep
        aktMag[j] <-sum(Layers[j,2:nodaysvector[j]])
        # ddistx informs on current capacity for each level in mm.
        if (aktMag[j] < Magkap[j]) ddistx[j] <- Magkap[j]- aktMag[j]
      }
      #> ddistx
      #[1] 1999.048    0.000    0.000    0.000    0.000


      redoutx <- outx   #reduction in input

      for (j in NoL:1) # NB NOL is the slowest level 1 is the fastest
      {
        if(redoutx > 0.0)
        {
          differ <- ddistx[j]-redoutx
          if (differ < 0)  {#i.e. deficit in level j is less than input,i.e. outx(redoutx) > deficit
            #divided by original outx. Tells us the fraction of input needed for this level
            ddist[j] <-ddistx[j]/outx
            redoutx <- redoutx-ddistx[j]# input is reduced correspondingly
          } else if (differ >= 0) {# i.e deficit in level j is greater than input
            if (j < NoL) ddist[j]  <-  1.0 - sum(ddist[(j+1):NoL])
            if (j==NoL) ddist[j] <-1.0
            # we allow for level 1 to receive the rest in case input is greater than capacity in levels
            if(j >1) ddist[(j-1):1] <-0.0
            redoutx <- 0.0
          }
          #print(paste("Differ",differ,"redoutx",redoutx,"j",j,"ddist[j]",ddist[j],"ddistx",ddistx[j]))
        }
      }
      #> ddist
      # [1] 0 0 0 0 0



      #-----------------------------end inputdist------------------------------------------------------------

      #updating the deficit (for all sub surface layers, NOT overland flow layer)
      ddistxtemp <- ddistx-outx*ddist # the deficit is reduced by input
      totdef <- sum(ddistxtemp[2:NoL]) # This may be negative if input outstrips deficits
      # > totdef
      # [1] 0


      AntL <- length(ddist)
      #Superpositioned UH for all levels, will be weighted by ddist, the distribution of input
      EunHyd <- vector("numeric",antHorlag)
      faktor <- ddist[1:AntL]*layerUH[1:AntL,]
      UHhslp <- colSums(faktor)

      #For River is already computed outside the loop
      #convolusjon, total UH for slopes and river, varies in time  due to differences in saturation
      TOTUH <-convolve(UHhslp,UHriver, type="o")
      # vector of 0

      #discharge in m3/s is the response for todays input. This is a vector  giving todays runoff and antHorlag + nodaysRiv ahead
      qsimutx <- (((outx/1000)*area2)/Timeresinsec)*TOTUH
      # vector of 0

      #If outx er NULL we will not have a total convoluted UH for this timestep
      # this vector is stored and added to previous vectors. NB TOTUH is a vector

      #response from Bogs
      if(bogarea > 0){
        qBogut <- (((outbog/1000)*bogarea)/Timeresinsec)*BogRiverUH
      } else {
         qBogut[1:length(BogRiverUH)] <-0 #Bidrag fra myrene denne dagen
      }
      # vector of 0

      qsimutx[1:antBogsteps] <- qsimutx[1:antBogsteps]+qBogut[1:antBogsteps]#adding contribution from bogs
      # vector of 0

      #qmmx <- (qsimutx*Timeresinsec*1000/totarea)  #same vector in mm/day


      #Updating the saturation levels
      for (j in 1: NoL)
      {
        qlayer <-ddist[j]*outx*layerUH[j,]    #finds response  in mm!!!!  for the actual layer, en vektor
        if(nodaysvector[j] >1)
        {
          Layers[j,(1:nodaysvector[j]-1)] <- Layers[j,2:nodaysvector[j]] # flytter the level of the matrix one timestep ahead
          Layers[j,nodaysvector[j]] <-0.0
          Layers[j,1:nodaysvector[j]] <- Layers[j,1:nodaysvector[j]] + qlayer[1:nodaysvector[j]] # update with todays event
        }
        if(nodaysvector[j] ==1)Layers[j,1:nodaysvector[j]] <- qlayer
      }

      #Routing in river network

      if(vectorlengde >1)
      {
        QRivx[1:(vectorlengde-1)] <- QRivx[2:vectorlengde]
        QRivx[vectorlengde] <-0.0
        QRivx <- QRivx +qsimutx
      }
      if(vectorlengde ==1)QRivx <-qsimutx

      QRD <-0.0

      if (vectorlengde >1) QRD <- QRivx[1]
      if(vectorlengde ==1)QRD <- QRivx
      #>     QRD
      #[1] 0.02802629

      #if(i==200)browser()
      #Assigning outdata to vector, one vector for each timestep
      simresult[i, 1:21] <- c(time$dateTS[i,1],
                              time$dateTS[i,2],
                              time$dateTS[i,3],
                              time$dateTS[i,4],
                              meanprecip,
                              meantemp,
                              q[i],
                              QRD,
                              middelsca,
                              snomag,
                              (GST-totdef),
                              totdef,
                              sm,
                              ea,
                              outx,
                              smbog,
                              eabog,
                              outbog,
                              toMSD,
                              outglac,
                              (m_r_onglac+outglac))

      qberegn[i] <-QRD # Simulated runoff for current timestep

      Layersres[i,,] <- Layers


      #---------------------------------------------------------------------------------------------------

      if(i == (savest) && savestate == 1) # Skal lagre tilstandene SWE, Z og Layers etc i rdata fil
      {

        Monthlast<-time$dateTS[i,2]
        Daylast<-time$dateTS[i,3]
        Yearlast<-time$dateTS[i,1]
        Hourlast<-time$dateTS[i,4]
        tilsdate<-c(Yearlast,Monthlast,Daylast,Hourlast)
        print(tilsdate)
        savest <- 0
        tilst <-c("sm", "dspd","dwcd","dsca", "Layers", "QRivx","smbog", "totdef","tilsdate")
        save(list=tilst, file = tilst.file2)
      }
      #---------------------------------------------------------------------------------------------

  } else {
    # WARNING:
    # No computation when either the precipiatation or the temperature is missing
    simresult[i, 1:21] <- c(time$dateTS[i,1],
                            time$dateTS[i,2],
                            time$dateTS[i,3],
                            time$dateTS[i,4],
                            NA,
                            NA,
                            q[i],
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA
                         )

     qberegn[i] <-NA

     Layersres[i,,] <- matrix(NA,NoL,nodaysvector[NoL])


     #---------------------------------------------------------------------------------------------------

     if(i == (savest) && savestate == 1) # Skal lagre tilstandene SWE, Z og Layers etc i rdata fil
     {

       Monthlast<-time$dateTS[i,2]
       Daylast<-time$dateTS[i,3]
       Yearlast<-time$dateTS[i,1]
       Hourlast<-time$dateTS[i,4]
       tilsdate<-c(Yearlast,Monthlast,Daylast,Hourlast)
       print(tilsdate)
       savest <- 0
       tilst <-NA
       save(list=tilst, file = tilst.file2)
     }
     #---------------------------------------------------------------------------------------------
  }



} # end of loop for number of timesteps in timeseries


colnames(simresult)<-c("year","month","day","hour","mhprec","mhtemp","qobs","qsim","middelsca","snowmag","M-D","D","G","Ea","X","Gbog","Eabog","Xbog","Z","waterGlacier","waterGSoilAndGlac")
write.csv(simresult, file = "simTS.csv",row.names = FALSE)
write.csv(Layersres, file = "LayersresTS.csv",row.names = FALSE)


dddGraph.ts(dateTS=time$dateTS,
            precip=precip,
            q=simresult[,8],
            q2 = simresult[,7])
