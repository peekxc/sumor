# SUMOR 
__sumor__ is a very early-stage package for the [R Project for Stastical Computing](https://www.r-project.org/about.html) developed to enable access to a small subset of the [Simulation of Urban Mobility](http://sumo.dlr.de/wiki/Simulation_of_Urban_MObility_-_Wiki) (SUMO) API.

The focus of the package is to simplify the usage, configuration, and execution of the variety of moving parts necessary to build a simulation with SUMO.

## Installation
1. Make sure [sumo is installed](http://sumo.dlr.de/wiki/Installing) and you're aware of [the location of the home path](http://sumo.dlr.de/wiki/FAQ#Basic_Usage), i.e. `sumo` with nothing else should return something similar to:
> SUMO Version 0.25.0 <br />
> Copyright (C) 2001-2015 DLR and contributors; http://sumo.dlr.de <br />
> License GPLv3+: GNU GPL Version 3 or later <http://gnu.org/licenses/gpl.html> <br />
> Use --help to get the list of options.

2. Install the release version of `devtools` from CRAN with `install.packages("devtools")`
3. Install `sumor` with: 
```R
devtools::install_github("https://github.com/wsu-wacs/sumor")
```

4. Configure the variables needed from Step (1):

```R
## Load library, set SUMO home path 
suppressMessages({ library("sumor", quietly = T) })
Sys.setenv(SUMO_HOME="/opt/local/share/sumo")
  
## Create temporary directory to store all the intermediate data files (optional)
if (!dir.exists("./tmp")){ dir.create("./tmp") }
Sys.setenv(SUMO_TMP=paste0(getwd(),"/tmp/"))
```
## Usage 
SUMO is a microscopic, space-continuous road traffic simulation. SUMO simulations require-as-input and produce-as-output [numerous varieties](http://sumo.dlr.de/wiki/Other/File_Extensions) of XML or similar-format files on disk. The information within these files fundamentally control how SUMO simulations are configured. The __sumor__ package contains several easy to use functions that allow configuring, running, monitoring, and fine-tuning SUMO simulations based on these files. 

### Getting started: Simulating traffic with OpenStreetMap
For simplicity, the package is entirely designed around the [Reference Class](https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html) design in R. To access the SUMO API, simply create a new reference object of type 'sumo':

```R
  sumo_net <- sumo$new()
```
All of the functionality related to the given SUMO object is accessed through the '$' operator. 

The next step is to specify a source SUMO network for simulation. Although its possible to [build a network from scratch with SUMO](http://sumo.dlr.de/wiki/Networks/Building_Networks_from_own_XML-descriptions), for larger projects working with complex geographical areas, importing map data from an external source is much easier. You can download [OpenStreetMap](https://www.openstreetmap.org/about) (OSM) data for use with SUMO using the `getOSM` method by passing an [sp](https://cran.r-project.org/web/packages/sp/index.html) bounding box. For example, suppose you want to make a new SUMO traffic simulation around Columbus, OH. A bounding box can be selected easily with [klokantech.com](http://boundingbox.klokantech.com) and used with the __sumor__ package like so: 

```R     
## (Selected from http://boundingbox.klokantech.com)
columbus_bbox <- sp::bbox(matrix(c(-83.024733,39.995205,-83.006923,40.005429), ncol=2, byrow = T)) 
sumo_net$getOSM(columbus_bbox, file = "columbus.osm")
```
This will download the OSM data into the directory pre-defined by _SUMO\_TMP_, or a temporary directory if the path variable isn't defined. The next step is to convert the OSM data into a usable SUMO road network suitable for simulation using [NETCONVERT](http://sumo.dlr.de/wiki/NETCONVERT)

```R  
sumo_net$netconvert(urban = T, pedestrian = T, polygons = T)
```
The parameters to `netconvert` control the use of the default [conversion typemap](http://sumo.dlr.de/wiki/Networks/Import/OpenStreetMap#Recommended_Typemaps). 

There are hundreds of parameters that to tweak that change how traffic is generated. The following generates an hour of random traffic, where at most _n_ = 1 car arrives with probability _p_ = 0.5 every second. 

```R
## Generate trips and routes (s seconds * m minutes * h hours)
sumo_net$randomTrips(start = 0, end = 60 * 60 * 1, n = 1, p = 2)
```
To view the simulation itself through the [SUMO-GUI](http://sumo.dlr.de/wiki/SUMO-GUI), simply call the `viewSimulation` method:  

```R
sumo_net$viewSimulation() 
```
You can also extract information from the simulation between any given set of time points. For example, retrieving the vehicle spatial coordinates for the first half and hour of the simulation can be done throught the 'getVehiclePositions' member function: 

```R
vehicle_wgs84 <- sumo_net$getVehiclePositions(start = 0, end = lubridate::dhours(0.5))
```

## Further information
The entire __sumor__ package in encapsulated in reference class methods implemented as part of the sumo class definition, [sumo.R](src/sumo.R). To get an index of all of the methods implemented by the package, use: 

```R
package?sumor
```

## Prequisite knowledge 
Knowledge of how to manually create a simulation with SUMO is useful, but not required. 

__sumor__ imports the de-facto standard package for handling spatial data, [sp](https://cran.r-project.org/web/packages/sp/index.html), and has many often returns objects within the __sp__ family of classes. 

## Contributing 

Pull requests or functionality requests are more than welcome. Adding functionality is as simple as adding a new method to the reference class, along with some simple documentation, e.g. 

```R
#' @title My method 
#' @name my_method
#' @description Does something...
sumo$methods(my_method= function(...)
	...
})
```

