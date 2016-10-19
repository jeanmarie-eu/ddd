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
Sofar, only a demo (/demo/test.R) might be taken as a tutorial.
More how-to's will be release soon on the [ddd webpage](https://nexmodeling.github.io/ddd/)

## Dependencies

The ddd R-package works as a holding package that drives a set of packages:

- [dddModel](https://nexmodeling.github.io/dddModel/)
- [dddCelerity](https://nexmodeling.github.io/dddCelerity/)
- [dddEvapotranspiration](https://nexmodeling.github.io/dddEvapotranspiration/)
- [dddPrecipLZ](https://nexmodeling.github.io/dddPrecipLZ/)
- [dddTempLZ](https://nexmodeling.github.io/dddTempLZ/)
- [dddSnow](https://nexmodeling.github.io/dddSnow/)
- [dddSoilMoisture](https://nexmodeling.github.io/dddSoilMoisture/)
- [dddSoilWater](https://nexmodeling.github.io/dddSoilWater/)
- [dddSoilDischarge](https://nexmodeling.github.io/dddSoilDischarge/)
- [dddUH](https://nexmodeling.github.io/dddUH/)
- [dddGroundwater](https://nexmodeling.github.io/dddGroundwater/)
