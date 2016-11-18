# ddd 0.0.3

* use of onionR in order to create environment for each process
* model has now an onion structure where every process is embedded in the environment of its parent
* a ddd object enables the processing in between every process without any problem of interaction within variables
* this way of structuring the model simplify the writing of the package: time, obs, param, model, init, simulation
* new [so far very simple] obs-modules are incorporated (precip, temperature, runoff, snow coverage). This is a start for an easy connection of ddd with the different observations

# ddd 0.0.2

* add a do.simTSv2 function in order NVE to test ddd with its precipitation and temperature fields
* homogeneization of the name of the functions
* update the code according to the homogeneized function names of the related packages
* MAD is now taken as a parameter

# ddd 0.0.1

* Added a `NEWS.md` file to track changes to the package.
