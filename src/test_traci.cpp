#include <Rcpp.h>
#include <iostream>
#include "TraCIAPI.h"

using namespace Rcpp;

class Client : public TraCIAPI {
public:
  Client() {};
  ~Client() {};
};

// [[Rcpp::export()]]
List getCO2(const int start, const int duration){
  Client client = Client();
  client.connect("localhost", 1337);
  client.simulationStep(start * 1000); // Get the simulation to the current start point
  std::vector<NumericVector> all_co2 = std::vector<NumericVector>();
  std::vector< std::vector<std::string> > all_ids = std::vector< std::vector<std::string> >();
  for (int sec = 0; sec < duration; ++sec){
    if (sec % 100 == 0) Rcpp::checkUserInterrupt();
    std::vector<std::string> vehicles = client.vehicle.getIDList();
    NumericVector veh_co2 = NumericVector(vehicles.size());
    int i = 0;
    for (std::vector<std::string>::iterator it = vehicles.begin(); it != vehicles.end(); ++it){
      veh_co2(i++) = client.vehicle.getCO2Emission(*it); //client.route.getEdges(*it);
    }
    all_co2.push_back(veh_co2);
    all_ids.push_back(vehicles);
    client.simulationStep(sec * 1000); // Step the simulation forward 1 second
  }
  client.close();
  List res = List::create(_["co2"] = wrap(all_co2), _["id"] = wrap(all_ids));
  return(res);
  // t.gui.screenshot("View #0",    "images/"+str(i)+".png"))
}


// [[Rcpp::export()]]
List getJunctions_int(){
  Client client = Client();
  client.connect("localhost", 1337);
  std::vector<std::string> junc_ids =client.junction.getIDList();
  NumericMatrix junc_pos = NumericMatrix(junc_ids.size(), 2);
  int i = 0;
  for (std::vector<std::string>::iterator it = junc_ids.begin(); it != junc_ids.end(); ++it){
    TraCIPosition xyz = client.junction.getPosition(*it); // client.vehicle.getPosition(*it);
    junc_pos(i++, _) = NumericVector::create(xyz.x, xyz.y); //client.route.getEdges(*it);
  }
  client.close();
  List res = List::create(_["junction_xy"] = junc_pos, _["junction_id"] = wrap(junc_ids));
  return(res);
}


// Given a start time and a duration, get the pedestrian positions
// [[Rcpp::export()]]
List getPedestrianPositions(const int start, const int duration){
  Client client = Client();
  client.connect("localhost", 1337);
  client.simulationStep(start * 1000); // Get the simulation to the current start point
  std::vector<NumericMatrix> all_pos = std::vector<NumericMatrix>();
  std::vector< std::vector<std::string> > all_ids = std::vector< std::vector<std::string> >();
  for (int sec = 0; sec < duration; ++sec){
    if (sec % 100 == 0) Rcpp::checkUserInterrupt();
    std::vector<std::string> pedestrian = client.person.getIDList();
    NumericMatrix ped_pos = NumericMatrix(pedestrian.size(), 2);
    int i = 0;
    for (std::vector<std::string>::iterator it = pedestrian.begin(); it != pedestrian.end(); ++it){
      TraCIPosition xyz = client.person.getPosition(*it);
      ped_pos(i++, _) = NumericVector::create(xyz.x, xyz.y); //client.route.getEdges(*it);
    }
    all_pos.push_back(ped_pos);
    all_ids.push_back(pedestrian);
    client.simulationStep(sec * 1000); // Step the simulation forward 1 second
  }
  client.close();
  List res = List::create(_["pos"] = wrap(all_pos), _["id"] = wrap(all_ids));
  return(res);

  // t.gui.screenshot("View #0",    "images/"+str(i)+".png"))
}

// Given a start time and a duration, get the vehicle positions
// [[Rcpp::export()]]
List getVehiclePositions(const int start, const int duration){
  Client client = Client();
  client.connect("localhost", 1337);
  client.simulationStep(start * 1000); // Get the simulation to the current start point
  std::vector<NumericMatrix> all_pos = std::vector<NumericMatrix>();
  std::vector< std::vector<std::string> > all_ids = std::vector< std::vector<std::string> >();
  for (int sec = 0; sec < duration; ++sec){
    if (sec % 100 == 0) Rcpp::checkUserInterrupt();
    std::vector<std::string> vehicles = client.vehicle.getIDList();
    NumericMatrix veh_pos = NumericMatrix(vehicles.size(), 2);
    int i = 0;
    for (std::vector<std::string>::iterator it = vehicles.begin(); it != vehicles.end(); ++it){
      TraCIPosition xyz = client.vehicle.getPosition(*it);
      veh_pos(i++, _) = NumericVector::create(xyz.x, xyz.y); //client.route.getEdges(*it);
    }
    all_pos.push_back(veh_pos);
    all_ids.push_back(vehicles);
    client.simulationStep(sec * 1000); // Step the simulation forward 1 second
  }
  client.close();
  List res = List::create(_["pos"] = wrap(all_pos), _["id"] = wrap(all_ids));
  return(res);
  // t.gui.screenshot("View #0",    "images/"+str(i)+".png"))
}

// [[Rcpp::export()]]
List getVehicleFuelConsumption(const int start, const int duration){
  Client client = Client();
  client.connect("localhost", 1337);
  client.simulationStep(start * 1000); // Get the simulation to the current start point
  std::vector<NumericVector> all_fc = std::vector<NumericVector>();
  std::vector< std::vector<std::string> > all_ids = std::vector< std::vector<std::string> >();
  for (int sec = 0; sec < duration; ++sec){
    if (sec % 100 == 0) Rcpp::checkUserInterrupt();
    std::vector<std::string> vehicles = client.vehicle.getIDList();
    NumericVector veh_fc = NumericVector(vehicles.size());
    int i = 0;
    for (std::vector<std::string>::iterator it = vehicles.begin(); it != vehicles.end(); ++it){
      veh_fc(i++) = client.vehicle.getFuelConsumption(*it); //client.route.getEdges(*it);
    }
    all_fc.push_back(veh_fc);
    all_ids.push_back(vehicles);
    client.simulationStep(sec * 1000); // Step the simulation forward 1 second
  }
  client.close();
  List res = List::create(_["fc"] = wrap(all_fc), _["id"] = wrap(all_ids));
  return(res);
  // t.gui.screenshot("View #0",    "images/"+str(i)+".png"))
}



// void testConnect(){
//   std::stringstream buffer;
//   std::streambuf * old = std::cerr.rdbuf(buffer.rdbuf());
//   Client client = Client();
//   client.connect("localhost", 1337);
//
//   std::string text = buffer.str(); // text will now contain "Bla\n"
// }

/*** R
#   library("sumor")
#
#   ## First, make sure SUMO is installed; then set environment variable "SUMO_HOME" to its installation directory
#   Sys.setenv(SUMO_HOME="/usr/local/Cellar/sumo/0.30.0")
#   Sys.setenv(SUMO_TMP="~/WaCS/sumor/osu_tmp")
#
#   ## Create new sumor object
#   sumo_net <- if ("sumo_net" %in% ls()) sumor::sumo$new()$import(sumo_net) else sumor::sumo$new()
#   osu_bbox <- sp::bbox(matrix(c(-83.035419,40.01156,-83.017309,40.021748), nrow=2, byrow = T))
#   sumo_net$quickSim(bbox = osu_bbox, sim_len = lubridate::dminutes(2))
#
#   juncs <- sumo_net$getJunctions()
#   ## Get OSM Map using bounding box (Selected from http://boundingbox.klokantech.com )
#   #
#   #sumo_net$quickSim(bbox = osu_bbox, sim_len = lubridate::dminutes(5))
#
#   #load("/Users/mpiekenbrock/WaCS/spatnet/data/CLIF.rda")
#   #clif_bbox <- sp::bbox(as.matrix(CLIF[, .(long, lat)]))
#   # sumo_net$getOSM(clif_bbox, overwrite = T)
#   #sumo_net$quickSim(bbox = clif_bbox, sim_len = lubridate::ddays(30), p = 10, n = 1) ## generate at maximum 1 car every 5 seconds
#
#   # View the simulation
#   # sumo_net$viewSimulation()
#
#   # Adapted from http://www.igorexchange.com/node/927
#   getUTMZone <- function(Long, Lat){
#     LongTemp <- (Long+180)-floor((Long+180)/360)*360-180
#     ZoneNumber <- floor((LongTemp + 180)/6) + 1
#     if (Lat >= 56.0 && Lat < 64.0 && LongTemp >= 3.0 && LongTemp < 12.0 )
#       ZoneNumber <- 32
#     if( Lat >= 72.0 && Lat < 84.0 ){
#       if  ( LongTemp >= 0.0  && LongTemp <  9.0 )
#         ZoneNumber = 31
#       else if( LongTemp >= 9.0  && LongTemp < 21.0 )
#         ZoneNumber = 33
#       else if(LongTemp >= 21.0 && LongTemp < 33.0 )
#         ZoneNumber = 35
#       else if(LongTemp >= 33.0 && LongTemp < 42.0 )
#         ZoneNumber = 37
#     }
#     return(as.integer(ZoneNumber))
#   }
#
#
#
#
#   ## Expects a matrix-coercible object of the form < lat, long >
#   ## Zone specifies the UTM zone. Defaults to eastern zone 17.
#   wgs84_to_meters <- function(lat, long){
#     zone <- getUTMZone(Long = mean(long), Lat = mean(lat))
#     x <- cbind(long, lat)
#     geo_pts <- sp::SpatialPoints(as.matrix(x), proj4string = CRS("+proj=longlat +ellps=WGS84"))
#     metric_pts <- sp::spTransform(geo_pts, CRS(paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
#     coords <- metric_pts@coords
#     new_coords <- cbind(coords[, 1] - min(coords[, 1]), coords[, 2] - min(coords[, 2]))
#     offsets <- c(min(coords[, 1]), min(coords[, 2]))
#     return(list(coords = new_coords, offet = offsets))
#   }
#
#   ## convert metric back to lat/long w/ the appropriate offset
#   meters_to_wgs84 <- function(x, y, zone = 17, offsets = c(0, 0)){
#     tmp_pts <- as.matrix(cbind(x, y))
#     tmp_pts[, 1] <- tmp_pts[, 1] + offsets[1]
#     tmp_pts[, 2] <- tmp_pts[, 2] + offsets[2]
#     metric_pts <- sp::SpatialPoints(tmp_pts, proj4string = CRS(paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
#     geo_pts <- sp::spTransform(metric_pts, CRS("+proj=longlat +ellps=WGS84"))
#     return(geo_pts@coords)
#   }
#
#   s_n <- sample(1:nrow(CLIF), size = 150L)
#   x <- CLIF[s_n, .(long, lat)]
#   x_sp <- sp::SpatialPoints(x, CRS("+proj=longlat +ellps=WGS84"))
#   x_m <-  sp::spTransform(x_sp, CRS(paste0("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
#   offsets <- c(min(x_m@coords[, 1]), min(x_m@coords[, 2]))
#   x_m2 <- cbind(x_m@coords[, 1] - offsets[1], x_m@coords[, 2] - offsets[2])
#   x_sp2 <- sp::spTransform(x_m, CRS("+proj=longlat +ellps=WGS84"))
#
#   # meters_to_wgs84 <- function(x, zone = 17){
#   #   metric_pts <- sp::spTransform(as.matrix(x), CRS(paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
#   #   geo_pts <- sp::SpatialPoints(as.matrix(x), proj4string = )
#   #   sp::spTransform(what, CRS("+proj=longlat +ellps=WGS84"))
#   #   coords <- metric_pts@coords
#   #   return(cbind(coords[, 1] - min(coords[, 1]), coords[, 2] - min(coords[, 2])))
#   # }
#
#   library("sp")
#   coords <- wgs84_to_meters(CLIF[, .(long, lat)])
#
#   #cmd <- "sumo -c /Users/mpiekenbrock/WaCS/sumor/osu_tmp/tmp.sumocfg --remote-port 1337 &"
#   #what <- system2("/usr/local/Cellar/sumo/0.30.0/bin/sumo", args = list("-c /Users/mpiekenbrock/WaCS/sumor/osu_tmp/tmp.sumocfg --remote-port 1337 &"))
#
#   ## extractSPs := extract Stay Points
#   ## Assumes
#   extractSPs <- function(x, d_thresh = 5L, t_thresh = 1L){
#     if (length(find("stay_pts")) == 0) { Rcpp::sourceCpp('~/WaCS/net-analysis/stay_pts.cpp', embeddedR = FALSE) }
#     if (ncol(x) != 4) stop("Expects a 4-column data.frame")
#     colnames(x) <- c("veh_id", "V1", "V2", "time")
#     pt_ids <- sort(unique(x$veh_id))
#     all_sps <- vector(mode = "list", length = length(pt_ids))
#     i <- 1
#     for (vid in pt_ids){
#       mat <- as.matrix(x[veh_id == vid, .(V1, V2, time)])
#       ## stay points are the medoid points within a contiguous sequence of trajectory points all of which
#       ## had subsequent distances less than d_thresh (meters) and collectively occurred over a period of
#       ## time longer than t_thresh
#       if (!is.null(dim(mat)) && nrow(mat) > 3L && ncol(mat) == 3){
#         res <- stay_pts(mat[, 1:2], mat[, 3], d_thresh, t_thresh)
#         all_sps[[i]] <- data.frame(res)
#       }
#       i <- i + 1
#     }
#     all_sps <- data.table::data.table(do.call(rbind, all_sps))
#     return(all_sps)
#   }
#
#   ## Start sumo server
#   one_day <- as.integer(lubridate::ddays(1))
#   all_sps <- vector("list", length = 2)
#   all_traj <- vector("list", length = 2)
#   i <- 1
#
#   duration <- as.integer(lubridate::dhours(24*3))
#   is_working <- system("lsof -i tcp:1337", ignore.stdout = F, intern = T)
#   veh_pos <- sumor:::getVehiclePositions(0L, duration)
#   veh_xy <- data.table::rbindlist(lapply(veh_pos$pos, as.data.frame), idcol = "time")
#   veh_id <- as.integer(unlist(sapply(veh_pos$id, as.integer)))
#   veh_data <- cbind(veh_xy, veh_id)
#   all_traj <- veh_data
#   all_sps <- extractSPs(veh_data)
#
#
#   for (day_i in 1:2L){
#     start <- day_i * one_day
#
#     is_working <- system("lsof -i tcp:1337", ignore.stdout = F, intern = T)
#     if (length(is_working) <= 1) sumo_net$startSumoServer(port = 1337L)
#     is_working <- system("lsof -i tcp:1337", ignore.stdout = F, intern = T)
#     if (length(is_working) >= 2){
#       veh_pos <- sumor:::getVehiclePositions(start, duration)
#       veh_xy <- data.table::rbindlist(lapply(veh_pos$pos, as.data.frame), idcol = "time")
#       veh_id <- as.integer(unlist(sapply(veh_pos$id, as.integer)))
#       veh_data <- cbind(veh_xy, veh_id)
#       all_traj[[i]] <- veh_data
#       all_sps[[i]] <- extractSPs(veh_data)
#       # save(veh_pos, file = paste0("day_", day_i, "_", b, ".rdata"))
#       #sps <- extractSPs(veh_data)
#       #save(sps, file = paste0("day_", day_i, "_", b , "_sps.rdata"))
#     }
#   }
#
#
#   ## Parse through
#   all_sps <- vector("list", length = 12*4)
#   all_traj <- vector("list", length = 12*4)
#   i <- 1
#   for (day_i in 0:2){
#
#       veh_xy <- data.table::rbindlist(lapply(veh_pos$pos, as.data.frame), idcol = "time")
#       veh_id <- as.integer(unlist(sapply(veh_pos$id, as.integer)))
#       veh_data <- cbind(veh_xy, veh_id)
#       all_traj[[i]] <- veh_data
#       all_sps[[i]] <- extractSPs(veh_data)
#       i <- i + 1L
#       print(i)
#     }
#     # readline(prompt = "Oh god please dont crash")
#     gc()
#   }
#
#   # veh_xy <- data.table::rbindlist(lapply(veh_pos$pos, as.data.frame), idcol = "time")
#   # veh_id <- unlist(sapply(veh_pos$id, as.integer))
# # veh_data <- cbind(veh_xy, veh_id)
#   save(veh_pos, file = paste0("day_", day_i, "_", b, ".rdata"))
# #sps <- extractSPs(veh_data)
# #save(sps, file = paste0("day_", day_i, "_", b , "_sps.rdata"))
#
#
#
#   load("/Users/mpiekenbrock/mjp.algorithms/CIKM2017 - ClusterTree POIs/resources/geolife.rdata")
#   geolife[["163"]]$traj <- geolife[["163"]]$traj[-1:-3,] ## fix
#
#   lubridate:::ymd_hms(what$traj[1, .(frame)]$frame, tz = "GMT")
#
#   starts <- sapply(ls(geolife), function(key) min(geolife[[key]]$traj$frame), simplify = F)
#   earliest_start <- lubridate::ymd_hms(starts[[names(which.min(starts))]], tz = "GMT")
#   time_steps <- lapply(ls(geolife), function(key) as.integer(earliest_start %--% geolife[[key]]$traj$frame))
#   keys <- ls(geolife)
#   for (i in 1:length(keys)){
#     key <- keys[[i]]
#     geolife[[key]]$traj <- cbind(geolife[[key]]$traj, time_steps[[i]])
#   }
#
#
#   geolife2 <- vector("list", length = length(keys))
#   for (i in seq(length(keys))){
#     key <- keys[[i]]
#     track <- geolife[[key]]$traj
#     metric_coords <- tryCatch(wgs84_to_meters(lat = track$lat, long = track$lng), error = function(e){}, finally = function() NULL)
#     if (!is.null(metric_coords)){
#       geolife2[[i]] <- data.table::data.table(V1 = metric_coords$coords[, 1], V2 = metric_coords$coords[, 2], time = track$V2)
#     }
#   }
#
#   geolife_parsed <- data.table::rbindlist(geolife2, idcol = "veh_id")
#   save(geolife_parsed, file = "geolife_parsed.rdata")
#
#   ### To replicate Geolife:
#   ### T_thresh == 30 minutes
#   ### D_thresh == 200 meters
#   geolife_sps <- extractSPs(geolife_parsed, d_thresh = 200, t_thresh = as.integer(lubridate::dminutes(30)))
#
#   geolife_net <- createGeoNet(geolife_parsed, POIs)
#
#   goelife[["164"]]
#
#   load("hour1.rdata")
#
#
#   veh_xy <- data.table::rbindlist(lapply(what$pos, as.data.frame), idcol = "time")
#   veh_id <- unlist(sapply(what$id, as.integer))
#   veh_data <- cbind(veh_xy, veh_id)
#   save(veh_data, file = "veh_data.rdata")
*/




