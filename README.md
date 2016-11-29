# ddd

[![Travis-CI Build Status](https://travis-ci.org/nexModeling/ddd.svg?branch=master)](https://travis-ci.org/nexModeling/ddd)

This R-package makes it easy to run the Distance Distribution Dynamics (ddd) hydrological model.
This package is based on the work of different authors:
   - Skaugen and Onof (2013), [DOI: 10.1002/hyp.9968](http://onlinelibrary.wiley.com/doi/10.1002/hyp.9968/abstract)
   - Skaugen, Peerebom and Nulsson (2015), [DOI: 10.1002/hyp.10315](http://onlinelibrary.wiley.com/doi/10.1002/hyp.10315/full)
   - Skaugen and Weltzien (2016), [DOI: 10.5194/tc-10-1947-2016](http://www.the-cryosphere.net/10/1947/2016/)

## Installation

```R
# install.packages("devtools")
devtools::install_github("nexModeling/ddd")
```

## Usage

```R
library(ddd)

main(fromPeriod="2000090106",
     toPeriod="2014123106",
     timeResolution="daily",
     catchment="Tingvatn",
     pathData=paste0(.libPaths()[1],"/ddd/data/"),
     fileData="val_24.9_24hptq_kal.txt",
     pathParam=paste0(.libPaths()[1],"/ddd/data/"),
     fileParam="best_par_24.9_24h.txt",
     FIGURE=TRUE)
```

## Dependencies

The ddd R-package works as a holding package that drives a set of packages:

- [dddModel](https://nexmodeling.github.io/dddModel/)
- [dddEvapotranspiration](https://nexmodeling.github.io/dddEvapotranspiration/)
- [dddPrecipLZ](https://nexmodeling.github.io/dddPrecipLZ/)
- [dddTempLZ](https://nexmodeling.github.io/dddTempLZ/)
- [dddScaOb](https://nexmodeling.github.io/dddScaOb/)
- [dddQ](https://nexmodeling.github.io/dddQ/)
- [dddSnow](https://nexmodeling.github.io/dddSnow/)
- [dddSoilMoisture](https://nexmodeling.github.io/dddSoilMoisture/)
- [dddSoilWater](https://nexmodeling.github.io/dddSoilWater/)
- [dddSoilDischarge](https://nexmodeling.github.io/dddSoilDischarge/)
- [dddUH](https://nexmodeling.github.io/dddUH/)
- [dddGroundwater](https://nexmodeling.github.io/dddGroundwater/)
