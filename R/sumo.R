#' @title SUMO Reference Class
#' @name sumo
#' @description Reference class implementation that enables easy API-access to SUMO functionality suitable for simulation. A few
#' methods have been added which partially incorporate SUMO's Traffic Control Interface (TraCI) for fast retrieval of certain
#' types of simulation queries. Additional commands have been added which parse through SUMO's various input files (such as the
#' SUMO road network itself).
#' @details Before creating a SUMO object, there are two environmental variables to be aware of. The SUMO_HOME variable must be
#' set in the environmental prior to creating an object, otherwise it will return an error. Once set, SUMO requires several files
#' to be saved to the disk prior to creating a simulation. Namely, the SUMO road network must be set up and configured with either
#' a preconfigured road network, or via some external source (i.e. OSM) with \code{netconvert}. By default, these files are saved
#' in the 'default' directory and can be accessed via \code{system.file(..., package = "sumor")}. Alternatively, if the SUMO_TMP
#' variable is defined in the environment, all files will be saved relative to there.
#' @usage
#' ## Make sure SUMO_HOME is set to SUMO's locations,
#' ## i.e. Sys.setenv(SUMO_HOME=...)
#' sumo_net <- sumo$new()
#' sumo_net$<method name> ...
#' @return An reference class object of class "sumo". Methods are accessed with the standard '$'.
#' @seealso \code{\link[methods]{ReferenceClasses}}
#' @examples
#' library("sumor"); library("sp"); library("lubridate")
#'
#' ## Select a bounding box; I use http://boundingbox.klokantech.com/
#' osu_bbox <- bbox(matrix(c(-83.021676,39.9933,-83.005166,40.004366), nrow = 2, byrow = TRUE))
#'
#' ## Create the sumo object
#' osu_net <- sumo$new()
#'
#' ## Run quick road simulation w/ default settings
#' osu_net$quickSim(bbox = osu_bbox, sim_len = lubridate::dhours(0.5), p = 4, n = 1) # on average generate 1 vehicle every 4 seconds
#'
#' ## View the simulation.
#' osu_net$viewSimulation()
#' @author Matt Piekenbrock
#' @section Methods:
#' \subsection{Network and Configuration}{
#'   \itemize{
#'     \item{\strong{\code{\link{netconvert}}:}}{ Covert a given source to a SUMO road network.}
#'     \item{\strong{\code{\link{getOSM}}:}}{ Download an OpenStreetMap tile source.}
#'     \item{\strong{\code{\link{getNetwork}}:}}{ Retrieve the SUMO road network.}
#'     \item{\strong{\code{\link{getJunctions}}:}}{ Retrieve the junctions in the SUMO road network.}
#'     \item{\strong{\code{\link{getPolygons}}:}}{ Retrieve the polygons in the SUMO road network.}
#'     \item{\strong{\code{\link{addPolys}}:}}{ Add polygon information to the SUMO road network.}
#'     \item{\strong{\code{\link{mergeTrips}}:}}{ Merge two or more trips files into one.}
#'     \item{\strong{\code{\link{getEdges}}:}}{ Get the edges of the SUMO road network.}
#'     \item{\strong{\code{\link{getPedestrianEdges}}:}}{ Retrieve pedestrian-only edges from the SUMO road network. }
#'     \item{\strong{\code{\link{getTargetNodes}}:}}{ Retrieve the junctions in the SUMO road network, by edge id.}
#'   }
#' }
#' \subsection{Simulation}{
#'   \itemize{
#'     \item{\strong{\code{\link{simulate}}:}}{ Run the configured SUMO simulation.}
#'     \item{\strong{\code{\link{quickSim}}:}}{ Creates a quick SUMO simulation with suitable defaults.}
#'     \item{\strong{\code{\link{viewSimulation}}:}}{ View the simulation with the SUMO GUI.}
#'     \item{\strong{\code{\link{randomTrips}}:}}{ Creates random trips suitable for simulation.}
#'     \item{\strong{\code{\link{getRoutes}}:}}{ Retrieve the routes in the current simulation.}
#'     \item{\strong{\code{\link{getVehiclePositions}}:}}{ Retrieve the vehicle positions in the current simulation.}
#'     \item{\strong{\code{\link{genConfig}}:}}{ Generate configuration suitable for simulation.}
#'     \item{\strong{\code{\link{vehicleProbe}}:}}{ Probe a vehicle for various information.}
#'     \item{\strong{\code{\link{vehicleAcc}}:}}{ Retrieve vehicle acceleration.}
#'     \item{\strong{\code{\link{getFCD}}:}}{ Retrieve vehicle 'Floating Car Data.'}
#'   }
#' }
#' \subsection{Utility}{
#'   \itemize{
#'     \item{\strong{\code{\link{command}}:}}{ Runs a SUMO command.}
#'     \item{\strong{\code{\link{projectCRS}}:}}{ Projects SUMO coordinates to arbitrary CRS.}
#'     \item{\strong{\code{\link{toMeters}}:}}{ Convert WGS84 coordinates to Cartesian coordinates.}
#'     \item{\strong{\code{\link{plotOSM}}:}}{ View the region to use for the simulation with leaflet.}
#'  }
#' }
#' @import methods sp xml2 data.table
#' @importFrom lubridate dminutes
#' @export sumo
#' @exportClass sumo
sumo <- setRefClass("sumo", fields = c("source", "trips", "network", "config", "configuration"))

literal <- function(str){ paste0("'", str, "'") }
sumo$methods(initialize = function(..., verbose = FALSE){
  callSuper(...)
  if (nchar(Sys.getenv("SUMO_HOME")) <= 1) stop("sumo expects the environmental variable 'SUMO_HOME' to be set")
  sumo_path <- paste0(normalizePath(Sys.getenv("SUMO_HOME")), .Platform$file.sep)

  ## File and directory checks
  if (!file.exists(paste0(sumo_path, "/bin/sumo"))) stop("Sumo executable not found in 'bin' folder of SUMO_HOME.")
  if (!dir.exists(paste0(sumo_path, "/tools"))) stop("SUMO tools not found in 'tools' folder of SUMO_HOME.")
  if (!dir.exists(paste0(sumo_path, "/data"))) stop("SUMO data not found in 'data' folder of SUMO_HOME.")
  if (nchar(Sys.getenv("SUMO_TMP")) <= 1){
    sumo_tmp <- file.path(paste0(system.file("default", package = "sumor"), .Platform$file.sep))
  } else { sumo_tmp <- paste0(normalizePath(Sys.getenv("SUMO_TMP")), .Platform$file.sep) }
  config <<- list(SUMO_HOME = Sys.getenv("SUMO_HOME"), SUMO_TMP = sumo_tmp, verbose = verbose)
})

#' @name Command
#' @title Command
#' @description Runs a SUMO command equivalent to running "< SUMO_HOME >/bin/< command >"
sumo$methods(command = function(cmd){
  bin_path <- normalizePath(paste0(config$SUMO_HOME, "/bin/"))
  final_cmd <- paste0(bin_path, .Platform$file.sep, cmd)
  if (config$verbose) cat("Running command:", paste(final_cmd, "\n"))
  status <- invisible(suppressMessages(system(final_cmd, ignore.stdout = config$verbose)))
  return(status)
})

#' @name startSumoServer
#' @title Start SUMO Simulation
#' @description Starts a SUMO simulation on the given port number.
sumo$methods(startSumoServer = function(port=1337){
  if(class(configuration) == "uninitializedField") stop("SUMO configuration must be generated with genConfig.")
  start_sumo <- paste0("sumo -c ", configuration, " --remote-port ", port, " &")
  status <- command(start_sumo)
  Sys.sleep(1) ## Wait for the process to start up
  is_working <- system(paste0("lsof -i tcp:", port), ignore.stdout = F, intern = T)
  return(length(is_working) >= 2)
})

# sumo$methods(setSource=function(source, type="osm", ...){
#   if (class(source) != "character") stop("sumo expects the source object to be a filepath")
#
# })

#' @title quickSim
#' @name quickSim
#' @description Starts a quick generic simulation. Used for testing.
sumo$methods(quickSim = function(bbox, sim_len = dminutes(15), p = 1, n = 1, ...){
  getOSM(bbox, file = "tmp.osm") # get osm map, set source
  if (!file.exists(paste0(config$SUMO_TMP, "tmp.net.xml"))){
    netconvert(urban = T, pedestrian = T, polygons = T, flags = "--sidewalks.guess --crossings.guess TRUE")
  } else { warning("SUMO Road network detected. Settings configuration to use existing file.") }

  ## Generate routing information
  randomTrips(start=0, end=as.integer(sim_len), p = p, n = n, ...)

  ## Generate configuration for the entire simulation
  genConfig(0, as.integer(sim_len), file="tmp.sumocfg")

  ## Process the simulation in batch mode
  # sim_len <- as.integer(sim_len)
  # batch_len <- as.integer(dminutes(5))
  # batch_len <- ifelse(sim_len < batch_len, sim_len, batch_len)
  # batches <- levels(cut(c(0, sim_len), as.integer(sim_len/batch_len), ordered_result = T))
  # sim_intervals <- data.table(start = as.numeric(sub("\\((.+),.*", "\\1", batches)),
  #                             end = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", batches)))
  # sim_intervals[1]$start <- 0
  # sim_intervals[nrow(sim_intervals)]$end <- sim_len

  ## Method to extract exemplars from trajectory source
  # res = vector(mode = "list", length = nrow(sim_intervals))
  # for (i in 1:nrow(sim_intervals)) {
  #   ## Running the simulation generated with randomTrips (0.01 == 10ms intervals)
  #   genConfig(begin = sim_intervals[i]$start, end = sim_intervals[i]$end)
  #   sim_flags <- paste("--step-length 0.5", "--fcd-output", paste0(config$SUMO_TMP, "tmp_fcd.xml"))
  #   simulate(flags = sim_flags)
  #
  #   ## get 'floating car data' (records pedestrian movements as well)
  #   res[[i]] <- getFCD(paste0(config$SUMO_TMP, "tmp_fcd.xml"))
  # }
  # res
})

#' @title toMeters
#' @name toMeters
#' @description Converts lat/long to meters.
sumo$methods(toMeters=function(lat1, lon1, lat2, lon2){
  R = 6378.137 ## Radius of earth in KM
  dLat = lat2 * pi / 180 - lat1 * pi / 180
  dLon = lon2 * pi / 180 - lon1 * pi / 180
  a = sin(dLat/2) * sin(dLat/2) +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) *
    sin(dLon/2) * sin(dLon/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  return(d * 1000) #  meters
})

#' @title netconvert
#' @name netconvert
#' @description Converts an OSM map to a SUMO road network suitable for simulation.
sumo$methods(netconvert = function(src_net = NA, net_file = "tmp.net.xml", net_type = "osmNetconvert.typ.xml", urban = T, pedestrian=F, polygons=F,
                                   defaults = "--geometry.remove --roundabouts.guess --ramps.guess --junctions.join --tls.guess-signals --tls.discard-simple --tls.join",
                                   flags="") {
  if (missing(src_net) && is.null(source)) { stop("source has not been populated") }
  if (!missing(src_net)){ source <<- src_net }
  network <<- net_file

  ## Use OSM and built-in OSM-to-SUMO defaults to build the SUMO network
  cat("Converting to SUMO network...\n")
  base <- "netconvert"
  base_type <- paste("--type-files", paste0(config$SUMO_HOME, "/data/typemap/", net_type))
  type_file <- base_type
  if (urban) { type_file <- paste(type_file, paste0(config$SUMO_HOME, "/data/typemap/osmNetconvertUrbanDe.typ.xml"), sep = ",") }
  if (pedestrian) { type_file <- paste(type_file, paste0(config$SUMO_HOME, "/data/typemap/osmNetconvertPedestrians.typ.xml"), sep = ",") }

  ## Cleaning process, replace file
  osm_file <- paste("--osm-files", literal(paste0(config$SUMO_TMP, source)))
  out_file <- paste("--output-file", literal(paste0(config$SUMO_TMP, network)))
  flags <- paste(defaults, flags)

  ## Piece command together and initiate it
  netconvert_cmd <- paste(base, type_file, osm_file, out_file, flags) #  "--proj.plain-geo TRUE"
  command(netconvert_cmd)

  ## If polygons provided, import them
  if (polygons) {
    base_poly <- "polyconvert"
    net_file <- paste("--net-file", literal(paste0(config$SUMO_TMP, network)))
    type_file <- paste("--type-file", literal(paste0(config$SUMO_HOME, "/data/typemap/osmPolyconvert.typ.xml")))
    out_file <- paste("-o", literal(paste0(config$SUMO_TMP, "tmp.poly.xml")))
    poly_command <- paste(base_poly, net_file, osm_file, type_file, out_file)
    command(poly_command)
    config$polygon <<- TRUE
  }

  sumo_net <- read_xml(x = paste0(config$SUMO_TMP, network))
  config$location <<- attributes(xml2::as_list(xml_find_first(sumo_net, "//location")))
  config$CRS <<- CRS(config$location[["projParameter"]])
  config$bbox <<- sp::bbox(matrix(as.numeric(unlist(strsplit(config$location$origBoundary, ","))), nrow=2, byrow = T))
})

#' @title addStops
#' @name addStops
#' @description For every pedestrian route walking along a road, if he/she travels to a junction connected to
#' a building, then this function will augment the route he/she is on to travel inside the building, waiting for
#' a specific duration (designated by the building type), and then walks out and conintues on the original route
sumo$methods(addStops = function(route_file = "tmp.rou.xml", new_route_file = route_file, alpha = 0.15){
  xml_net <- xml2::read_xml(paste0(config$SUMO_TMP, network))
  routes <- xml2::read_xml(paste0(config$SUMO_TMP, route_file))
  routes_list <- xml2::as_list(routes)

  ## Get all edges that allow pedestrians
  all_edges <- getEdges()
  all_edge_ids <- xml2::xml_attr(all_edges, "id")
  ped_edges <- getPedestrianEdges()
  edge_info <- data.table::rbindlist(lapply(xml2::xml_attrs(ped_edges), as.list), fill = T)

  ## Get all the junctions ids
  junction_ids <- xml2::xml_attr(xml2::xml_find_all(xml_net, xpath = "//junction"), "id")

  ## Fidn the junctions corresponding to buildings
  bids <- grep(x = junction_ids, pattern = "^b[[:digit:]]+", value = T)

  ## Get the edges that lead to buildings (added before)
  bedge_info <- edge_info[to %in% bids]

  target_nodes <- getTargetNodes()
  #cand_edge_map <- edge_info[| (from %in% bedge_info$from), .(id, from, to)]
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

  routes <- xml2::read_xml(paste0(config$SUMO_TMP, route_file)) #osu_net$

  ## Helper functions
  makeObject <- function(object_name, attrs){
    object <- xml2::read_xml(paste0("<", object_name, "/>"))
    xml2::xml_attrs(object) <- attrs
    object
  }

  ## Make function to append edges to a walk
  append_edges <- function(walk, new_edges){
    cedges <- ifelse(is.na(xml2::xml_attr(walk, "edges")), "", xml2::xml_attr(walk, "edges"))
    xml2::xml_attrs(walk) <- list(edges = trimws(paste0(c(cedges, new_edges), collapse = " ")))
    walk
  }

  ## Check connection of an edge to make sure there's an existing connection
  # check_connection <- function(eid){
  #   # <connection from=":1747241430_w0" to="e63" fromLane="0" toLane="0" dir="s" state="M"/>
  #   conns <- xml2::xml_find_all(xml_net, xpath = "//connection")
  # }

  pb <- txtProgressBar(min = 0, max = xml2::xml_length(routes), style = 3)
  for (i in 1:(xml2::xml_length(routes))){
    # print(i)
    ## Retrieve the persons current route
    person_route <- xml2::xml_find_first(routes, xpath=paste0("//person[", i, "]"))
    route <- xml2::as_list(person_route)

    ## Extract the sequential edges to be traveled along
    edges <- unlist(strsplit(attr(route$walk, "edges"), split = " "))

    ## Key Idea: split the segments at edges that lead to junctions that lead to buildings, preparing to
    ## add stops to trips using said segments. Double check the last edge of the segment does actually
    ## lead to a junction connected to a building
    person_target_nodes <- target_nodes[[i]]
    walk_segments <- splitAt(edges, which(person_target_nodes %in% bedge_info$from)+1L)

    ## Critical nodes are nodes connected to buildings along the walk segment splits
    critical_nodes <- person_target_nodes[which(person_target_nodes %in% bedge_info$from)]

    ## For each walk segment, augment the current route, creating a stop inside the building
    ## connected to the tail edges of each segment (except the last)
    person <- xml2::read_xml("<person></person>")
    walk <- xml2::read_xml("<walk/>")
    xml2::xml_attrs(person) <- attributes(route)[!names(attributes(route)) %in% c("names")]
    prev_stop <- ""
    ctarget_node <- 1L
    for (segment in walk_segments) {

      ## Choose a random building-connected edge also connected to the target node
      if (ctarget_node <= length(critical_nodes)){
        new_edge <- bedge_info[from == critical_nodes[[ctarget_node]]]$id
      } else { new_edge <- character(0L) }

      ## Always only choose one edge, random if there are more than one
      new_edge <- ifelse(length(new_edge) > 0, sample(new_edge, size = 1), new_edge)

      ## Add the stop along the route
      if (runif(1) < alpha && length(new_edge) > 0 && !is.na(new_edge) && new_edge != prev_stop) {
        ## Append previous and current edges to current walk, but add the stop
        wedges <- trimws(paste0(c(prev_stop, segment, new_edge), collapse = " "))
        walk <- append_edges(walk, wedges)

        ## Adding a stop; insert current walk + stop, mark the previous stop as the new edge, and create a new walk
        xml2::xml_add_child(person, walk, .copy = TRUE)
        xml2::xml_add_child(person, xml2::read_xml(paste0(c("<stop lane=\"", paste0(new_edge, "_0"), "\" duration=\"1\"/>"), collapse = "")))
        prev_stop <- new_edge

        ## Add random internal walking/stopping independent of the current visit
        walk_subsegment <- xml2::read_xml("<walk/>")
        edge_subtrips <- grep(x = all_edge_ids, pattern = paste0(new_edge, "_[[:digit:]]+"), value = T)
        nsubtrips <- sample(1:length(edge_subtrips), size = 1)
        for (ns in 1:nsubtrips){
          trip <- sample(edge_subtrips, size = 1)
          xml2::xml_add_child(person, makeObject("walk", list(edges = paste0(c(new_edge, trip), collapse = " "))))
          xml2::xml_add_child(person, makeObject("stop", list(lane = paste0(trip, "_0"), duration="1")))
          xml2::xml_add_child(person, makeObject("walk", list(edges = paste0(c(trip, new_edge), collapse = " "))))
        }
        #xml2::xml_add_child(person, xml2::read_xml(paste0(c("<stop lane=\"", paste0(new_edge, "_0"), "\" duration=\"20\"/>"), collapse = "")))

        ## Reset walk
        walk <- xml2::read_xml("<walk/>")
      } else {
        ## Append previous and current edges to current walk, assign previous stop as nothing
        wedges <- trimws(paste0(c(prev_stop, segment), collapse = " "))
        walk <- append_edges(walk, wedges)
        prev_stop <- ""
      }
      ctarget_node <- ctarget_node + 1L

    }
    ## Add last segment
    # wedges <- trimws(paste0(c(prev_stop, walk_segments[[length(walk_segments)]]), collapse = " "))
    if (!is.na(xml2::xml_attr(walk, "edges"))){
      xml2::xml_add_child(person, walk)
    }
    xml2::xml_replace(person_route, person)
    setTxtProgressBar(pb, value = i)
  }
  close(pb)
  xml2::write_xml(routes, file = paste0(config$SUMO_TMP, new_route_file))
})

#' @title mergeTrips
#' @name mergeTrips
#' @description Merges two or more trips files into one.
sumo$methods(mergeTrips = function(input, final = "tmp.trips.xml"){
  { vehicles <- NULL; persons <- NULL ; routes <- NULL}
  base <- "<?xml version=\"1.0\"?> <routes xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://sumo.dlr.de/xsd/routes_file.xsd\"> </routes>"
  base_doc <- XML::xmlParse(base, asText = T)
  for (trip_file in input){
    if (!file.exists(trip_file)) stop(paste("Could not find file:", trip_file))

    ## Get nodeset; set initial routing
    route_nodeset <- xml2::read_xml(trip_file)
    if (is.null(route_nodeset)) routes <- route_nodeset

    tmp_vehicles <- xml2::xml_find_all(route_nodeset, xpath = "//vehicle")
    tmp_persons <- xml2::xml_find_all(route_nodeset, xpath = "//person")

    ## Add or append vehicles and persons
    ## Note: xml2 library doesn't support adding as a whole nodesets
    vehicles <- append(vehicles, lapply(lapply(tmp_vehicles, as.character), XML::xmlParseString))
    persons <- append(persons, lapply(lapply(tmp_persons, as.character), XML::xmlParseString))
  }
  XML::addChildren(XML::getNodeSet(base_doc, "//routes")[[1]], persons)
  XML::addChildren(XML::getNodeSet(base_doc, "//routes")[[1]], vehicles)
  XML::saveXML(base_doc, file = paste0(config$SUMO_TMP, final))
})

#' @title addPolys
#' @name addPolys
#' @description Converts existing polygons into junctions suitable for simulation.
sumo$methods(addPolys = function(poly_file = "tmp.poly.xml", buildings_only=F, ignore_edges = c(),
                                 poi_res = 1){

  ## Initial variables
  SUMO_TMP <- config$SUMO_TMP

  ## Get junctions and xml network
  xml_net <- read_xml(paste0(SUMO_TMP, network))
  junctions <- getJunctions(simplify = TRUE)

  ## Get the nodes that allow pedestrians
  p_edges <- getPedestrianEdges()
  from_nodes <- xml2::xml_attr(p_edges, "from")
  to_nodes <- xml2::xml_attr(p_edges, "to")
  pedestrian_reachable_nodes <- na.omit(unique(c(from_nodes, to_nodes)))
  pedestrian_nodes <- Filter(x = junctions, f = function(junc) junc$id %in% pedestrian_reachable_nodes)

  ## Get the (x, y) positions of said junctions
  pn_xy <- lapply(pedestrian_nodes, function(pn) list(id = pn$id, x = as.numeric(pn$x), y = as.numeric(pn$y)))
  pn_xy <- data.table::rbindlist(pn_xy)

  ## Get the building/polygon data from OSM
  xpath <- ifelse(buildings_only, "//poly[@type='building']", "//poly")
  buildings <- as_list(xml_find_all(xml2::read_xml(paste0(SUMO_TMP, poly_file)), xpath))
  buildings <- lapply(buildings, function(poly) attributes(poly))
  building_meta <- data.table::rbindlist(lapply(buildings, function(b) b[c("id", "type", "color", "layer")]))
  building_shapes <- lapply(buildings, function(b) b$shape)

  ## Associate
  nodes <- xml2::read_xml("<nodes/>")
  edges <- xml2::read_xml("<edges/>")
  connections <- xml2::read_xml("<connections/>")
  edge_counter <- 1L
  buildings_xy <- lapply(building_shapes, function(bs) matrix(as.numeric(unlist(strsplit(unlist(strsplit(bs, " ")), ","))), ncol=2, byrow=T))
  pb <- txtProgressBar(min = 0, max = length(building_shapes), style = 3)

  ## Helper functions
  makeObject <- function(object_name, attrs){
    object <- xml2::read_xml(paste0("<", object_name, "/>"))
    xml2::xml_attrs(object) <- attrs
    object
  }

  ## Generate random point(s) inside [possibly non-convex] polygon
  rpip <- function(n = 1, polygon){
    bbox <- sp::bbox(polygon)
    samples <- list()
    while(length(samples) < n){
      x <- runif(1, min = bbox[1, "min"], max = bbox[1, "max"])
      y <- runif(1, min = bbox[2, "min"], max = bbox[2, "max"])
      if (sp::point.in.polygon(x, y, polygon[, 1], polygon[, 2])){
        samples <- append(samples, list(c(x, y)))
      }
    }
    do.call(rbind, samples)
  }

  for (i in 1:length(building_shapes)){

    ## Create a new junction XML node for each building, use polygon center of gravity as the primary point
    poly_xy <- buildings_xy[[i]]
    poly_center <- sp::Polygons(list(sp::Polygon(poly_xy)), ID = 1)@labpt
    poly_attr <- append(list(id = paste0("b", building_meta[i]$id), type="unregulated",
                             x = poly_center[1], y = poly_center[2]),
                        as.list(building_meta[i][, .(type, color, layer)]))
    poly_node <- makeObject("node", poly_attr)

    ## Create internal polygon nodes
    int_node_xy <- rpip(nrow(poly_xy)*poi_res, poly_xy)
    int_nodes <- lapply(1:nrow(poly_xy), function(j) list(id = paste0(":b", building_meta[i]$id, "_", j),
                                              type="internal", x = int_node_xy[j,1], y = int_node_xy[j,2]))
    int_nodes <- lapply(int_nodes, function(inode) { makeObject("node", inode) })

    ## Find the existing to connect to the new polygon node
    closest_node <- as.integer(class::knn1(as.matrix(pn_xy[, .(x, y)]), poly_center, cl = 1:nrow(pn_xy)))
    from_node <- as.character(pn_xy[closest_node]$id)
    to_node <- paste0("b", building_meta[i]$id)

    ## Logic Checks
    if (from_node == to_node) next;
    if (all(poly_center == as.numeric(pn_xy[closest_node, .(x, y)]))) next;
    if (edge_counter %in% ignore_edges){
      edge_counter <- edge_counter + 1L
      next;
    }

    ## Make edge between building and pedestrian junction
    building_edge <- makeObject("edge", list(id = paste0("e", edge_counter), from = from_node, to = to_node,
                                             type="highway.footway", spreadType = "center"))

    ## Make internal edges
    int_edges <- lapply(1:nrow(poly_xy), function(j) {
      makeObject("edge", list(
        id = paste0("e", edge_counter, "_", j),
        from = to_node,
        to = paste0(":b", building_meta[i]$id, "_", j),
        type="highway.footway", spreadType = "center"
      ))
    })

    ## Make the edges lane
    edge_lane <- makeObject("lane", list(index = "0", allow="pedestrian", speed = "2.78", width = "2",
                                         length = dist(rbind(as.numeric(pn_xy[closest_node, .(x, y)]), poly_center)),
                                         shape = paste(paste(poly_center, collapse = ","), paste(as.numeric(pn_xy[closest_node, .(x, y)]), collapse = ","))))
    int_lanes <- lapply(1:nrow(poly_xy), function(j){
      makeObject("lane", list(index = "0", allow="pedestrian", speed = "2.78", width = "2",
                              length = dist(rbind(poly_center, as.numeric(int_node_xy[j,]))),
                              shape = paste(paste(poly_center, collapse = ","), paste(as.numeric(int_node_xy[j,]), collapse = ","))))
    })

    ## Merge lanes into edge
    xml2::xml_add_child(building_edge, edge_lane)
    for (c_i in 1:length(int_edges)) { xml2::xml_add_child(int_edges[[c_i]], int_lanes[[c_i]]) }

    ## Increment edge counter
    edge_counter <- edge_counter + 1L

    ## Add the new edges and nodes the current nodeset
    xml2::xml_add_child(edges, building_edge)
    for (cedge in int_edges){ xml2::xml_add_child(edges, cedge) }

    ## Add new nodes
    xml2::xml_add_child(nodes, poly_node)
    for (cnode in int_nodes){ xml2::xml_add_child(nodes, cnode) }

    ## Update progress
    setTxtProgressBar(pb, value = i)
  }
  close(pb)

  ## Patch nodes
  patch_nodes <- xml2::xml_new_document()
  xml2::xml_add_child(patch_nodes, nodes)

  ## Patch edges
  patch_edges <- xml2::xml_new_document()
  xml2::xml_add_child(patch_edges, edges)

  ## Writing to files
  xml2::write_xml(x = patch_nodes, paste0(config$SUMO_TMP, "patch_node.", network), options="as_xml")
  xml2::write_xml(x = patch_edges, paste0(config$SUMO_TMP, "patch_edge.", network), options="as_xml")

  ## Get ordinary types
  base_types <- paste("--type-files", paste0(config$SUMO_HOME, "/data/typemap/osmNetconvert.typ.xml"))
  base_types <- paste(base_types, paste0(config$SUMO_HOME, "/data/typemap/osmNetconvertUrbanDe.typ.xml"), sep = ",")
  base_types <- paste(base_types, paste0(config$SUMO_HOME, "/data/typemap/osmNetconvertPedestrians.typ.xml"), sep = ",")

  command <- paste("netconvert",
                   base_types,
                   "--sumo-net-file", paste0(config$SUMO_TMP, network),
                   "--node-files", paste0(config$SUMO_TMP, "patch_node.", network),
                   "--edge-files", paste0(config$SUMO_TMP, "patch_edge.", network),
                   "-o", paste0(config$SUMO_TMP, network),
                   "--plain.extend-edge-shape --geometry.remove --roundabouts.guess --ramps.guess --junctions.join --xml-validation.net always")
  system(command)

  ## SUMO doesn't allow conversion of the 'function' attribute for edges, so manually change those
  # sumo_net <- getNetwork()
  # edges <- xml2::xml_find_all(sumo_net, "//edge")
  # edge_ids <- xml2::xml_attr(edges, "id")
  #
  # ## Make any new edge that leads into a building invisible in the sumo GUI
  # sub_edges <- grep(x = edge_ids, pattern = paste0("e[[:digit:]]+_?[[:digit:]]*"))
  # xml2::xml_attr(edges[sub_edges], "function") <- "connector"
  # xml2::write_xml(sumo_net, paste0(config$SUMO_TMP, network))
})


#' @title plot
#' @name plot
#' @description Converts an OSM map to a SUMO road network suitable for simulation.
sumo$methods(plotOSM = function(x = NULL, add = F, type = "leaflet", src = c("arcgis", "leaflet", "osm")) {
  if (missing(src) || src == "arcgis")
    template <- "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
  map <- leaflet() %>%
      setView(lng = apply(config$bbox, 1, mean)[[1]], lat = apply(config$bbox, 1, mean)[[2]], zoom = 16)
})

#' @title randomTrips2
#' @name randomTrips2
#' @description Uses a SUMO road network to generate road traffics
sumo$methods(randomTrips2 = function(start=0, end=3600, lambda = 1, unit = "second", person = F){

  ## SUMO-RandomTrips generates routes and trips
  cat("Running Road Traffic Simulation...\n")
  SUMO_TMP <- config$SUMO_TMP
  xml_net <- xml2::read_xml(paste0(SUMO_TMP, network))

  ## Generate trips
  trip_file <- xml2::xml_new_document()
  xml2::xml_add_child(xml2::read_xml("<routes xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://sumo.dlr.de/xsd/routes_file.xsd\">"))

  ## Get junction data
  junctions <- xml2::xml_find_all(xml_net, xpath = "//junction")
  int_junctions <- Filter(junctions, f = function(junc) junc$type == "internal")

  ## Generate number of arrivals at each time unit ahead of time
  n_arrivals <- rpois(end - start, lambda = lambda)
  ##

  ## Calculate "fringe-factor"
  geo_center <- apply(osu_net$config$bbox, 1, mean)
  n_constant <- dist(rbind(geo_center, osu_net$config$bbox[, 1]))

  ## Get junction (x, y) coordinates + distance to center
  junc_xy <- lapply(xml2::xml_attrs(junctions), function(j) as.list(j[c("x", "y")]))
  junc_xy <- data.table::rbindlist(junc_xy)[, .(x=as.numeric(x), y=as.numeric(y))]
  junc_dist <- sapply(1:nrow(junc_xy), function(i) dist(rbind(as.numeric(junc_xy[i,]), geo_center)))
  junc_dist/sum(n_constant)

  ## Get the distribution of how often each junction should be picked (uniformly prefer junctions on the fringe)
  junc_prob <- junc_dist/sum(junc_dist)

  ## Generate random origin and destinations
  origins <- sapply(n_arrivals, function(n_arrivals) sample(x = 1:nrow(junc_xy), size = n_arrivals, prob = junc_prob, replace = T))
  destinations <- sapply(n_arrivals, function(n_arrivals) sample(x = 1:nrow(junc_xy), size = n_arrivals, prob = junc_prob, replace = T))

  route_file <- xml2::xml_new_document()
  base <- "sumo-randomTrips"
  net_inp <- paste0("-n ", SUMO_TMP, network)
  route_inp <- paste0("-r ", SUMO_TMP, "tmp.rou.xml")
  trip_inp <- paste0("-o ", SUMO_TMP, "tmp.trips.xml")
  time_inp <- paste0("-s ", start, " -e ", end)
  type_inp <- paste0("-p ", p, " -l --binomial=", n)

  ## Assemble command and simulate
  command <- paste(base, net_inp, route_inp, trip_inp, time_inp, type_inp)
  if (person) command <- paste(command, "--pedestrians")
  cat("Running command: ")
  cat(command)
  # config$sim_status <- system(command)
  if (config$sim_status == 0){
    config$trips <<- paste0(SUMO_TMP, "tmp.trips.xml")
    config$routes <<- paste0(SUMO_TMP, "tmp.rou.xml")
  }
})

#' @title simulate
#' @name simulate
#' @description Uses a SUMO road network to generate road traffics
sumo$methods(randomTrips = function(start=0, end=3600, p = 1, n=1, person = F, output = "tmp.trip.xml", flags=""){

  ## SUMO-RandomTrips generates routes and trips
  cat("Running Road Traffic Simulation...\n")
  SUMO_TMP <- config$SUMO_TMP
  base <- paste0(config$SUMO_HOME, "/tools/randomTrips.py")
  net_inp <- paste0("-n ", SUMO_TMP, network)
  route_inp <- paste0("-r ", SUMO_TMP, "tmp.rou.xml")
  trip_inp <- paste0("-o ", SUMO_TMP, output)
  time_inp <- paste0("-s ", start, " -e ", end)
  type_inp <- paste0(" -l --binomial=", n, " --period=", p)

  ## Assemble command and simulate
  command <- paste(base, net_inp, route_inp, trip_inp, time_inp, type_inp)
  if (person) command <- paste(command, "--pedestrians")
  command <- paste(command, flags)
  cat("Running command: ")
  cat(command)
  config$sim_status <<- system(command)
  if (config$sim_status == 0){
    config$trips <<- paste0(SUMO_TMP, output)
    config$routes <<- paste0(SUMO_TMP, "tmp.rou.xml")
  } else {
    stop(paste0("Failed to save trips and routes file: status code ", config$sim_status))
  }
})


#' @title Open SUMO GUI and run simulation
#' @name sumo_viewSimulation
#' @description Retrieves a map from OpenStreetMap to use for SUMO simulation
sumo$methods(viewSimulation = function(net_file = "tmp.net.xml", route_file = "tmp.rou.xml", flags=""){
  ## Open GUI for viewing simulation
  sumo_gui_cmd <- paste0("sumo-gui -n ", config$SUMO_TMP, net_file, " -r ", config$SUMO_TMP, route_file, " ", flags)
  if (config$polygon){ sumo_gui_cmd <- paste0(sumo_gui_cmd, " -a ", config$SUMO_TMP, "tmp.poly.xml") }
  command(sumo_gui_cmd)
})

#' @title Get OpenStreetMap source
#' @name getOSM
#' @description Retrieves a map from OpenStreetMap to use for SUMO simulation
sumo$methods(getOSM = function(bbox, file="tmp.osm", overwrite=FALSE) {
  if(!"matrix" %in% class(bbox)) { stop("Need 2x2 bounding box matrix to extract from OSM") }
  coords <- paste0(bbox, collapse = ",")
  url <- paste0("http://api.openstreetmap.org/api/0.6/map?bbox=", coords)
  if (file.exists(paste0(config$SUMO_TMP, file)) && overwrite == FALSE){
    warning("Detected existing OSM file with that name. Setting source to that file.")
    source <<- file
  } else {
    cat("Downloading OSM from: ", url, "\n")
    curl::curl_download(url = url, destfile = paste0(config$SUMO_TMP, file))
    source <<- file
  }
})

#' @title getEdges
#' @name getEdges
#' @description Converts an OSM map to a SUMO road network suitable for simulation.
sumo$methods(getEdges = function(internal=F, nested =F) {
  tmp_net_file <- paste0(config$SUMO_TMP, network)
  xpath <- ifelse(internal, "edge", "edge[@from]")
  edges <- xml2::xml_find_all(xml2::read_xml(tmp_net_file), xpath)
  if (nested){
    ## todo
    res <- xml2::as_list(xml2::xml_find_all(xml2::read_xml(tmp_net_file), xpath))
  } else {
    edge_xml <- xml2::xml_attrs(xml2::xml_find_all(xml2::read_xml(tmp_net_file), xpath))
    return(data.table::rbindlist(lapply(edge_xml, as.list), fill = T))
  }
})


#' @title Get SUMO Network
#' @name getNetwork
#' @description Retrieves the whole SUMO road network
sumo$methods(getNetwork = function(simplify = FALSE){
  sumo_net <- read_xml(paste0(config$SUMO_TMP, network))
  if (simplify){
    res <- as_list(xml_find_all(sumo_net, "//junction"))
    res <- lapply(res, function(junc) append(attributes(junc), list(requests=unlist(junc))))
    return(res)
  } else {
    return(sumo_net)
  }
})

#' @title Get SUMO Junctions
#' @name getJunctions
#' @description Retrieves junctions in SUMO network
sumo$methods(getJunctions = function(){
  # current_config <- xml2::read_xml(configuration)
  # if (missing(interval) || interval == "all"){
  #   start <- as.integer(xml2::xml_text(xml2::xml_find_all(current_config, "time/begin/@value")))
  # } else { start <- interval$start }
  # if (missing(interval) || interval == "all") {
  #   end <- as.integer(xml2::xml_text(xml2::xml_find_all(current_config, "time/begin/@value")))
  # } else { end <- interval$end }

  status <- startSumoServer()
  if (status){
    junctions <- getJunctions_int()
    res <- data.frame(x = junctions$junction_xy[, 1], y = junctions$junction_xy[, 2], id=junctions$junction_id)
    return(res)
  } else { return(NULL) }

  # sumo_net <- getNetwork()
  # if (simplify){
  #   res <- as_list(xml_find_all(sumo_net, "//junction"))
  #   res <- lapply(res, function(junc) append(attributes(junc), list(requests=unlist(junc))))
  #   return(res)
  # }
  # xml_find_all(sumo_net, "//junction")
})

#' #' @title Get SUMO Edges
#' #' @name getEdges
#' #' @description Retrieves edges in SUMO road network
#' sumo$methods(getEdges = function(simplify = FALSE){
#'   sumo_net <- getNetwork()
#'   if (simplify){
#'     res <- as_list(xml_find_all(sumo_net, "//edge"))
#'     res <- lapply(res, function(edge) append(attributes(edge), list(lanes=attributes(edge$lane))))
#'     return(res)
#'   } else {
#'     return(xml2::xml_find_all(sumo_net, xpath = "//edge"))
#'   }
#' })

#' @title Get SUMO Polygons
#' @name getPolygons
#' @description Retrieves Polygons in SUMO network
sumo$methods(getPolygons = function(poly_file =  "tmp.poly.xml"){
  sumo_net <- read_xml(paste0(config$SUMO_TMP, "tmp.poly.xml"))
  res <- as_list(xml_find_all(sumo_net, "//poly"))
  res <- lapply(res, function(poly) attributes(poly))
  res
})


#' @title Get SUMO Routes
#' @name getRoutes
#' @description Retrieves Routes in SUMO network
sumo$methods(getRoutes = function(route_file = "tmp.rou.xml", simplify=FALSE){
  sumo_net <- read_xml(paste0(config$SUMO_TMP, route_file))
  if (simplify){
    persons <- as_list(xml_find_all(sumo_net, "//person"))
    res <- lapply(persons, function(person) attributes(unname(person)))
    res2 <- lapply(persons, function(person) sapply(names(person), function(mode) attributes(person[[mode]])))
    return(Map(c, res, res2))
  } else {
    return(sumo_net)
  }
})

#' @name getVehiclePositions
#' @title getVehiclePositions
#' @description  Uses TRACI API to retrieve the vehicle coordinates from < start, end >
sumo$methods(getVehiclePositions = function(start, end){
  status <- clif_net$startSumoServer()
  if (status){
   return(sumor:::getVehiclePositions(start, end))
  } else { stop("Error: There was a problem starting the SUMO simulation.") }
})
# @inheritSection methods Retrieves vehicle coordinates in a SUMO simulation


#' @title Project to Coordinate Reference System
#' @name projectCRS
#' @description Project coordinates defined in the SUMO network to a target Coordinate Reference System. Defaults to WGS84.
sumo$methods(projectCRS= function(x, crs=CRS("+proj=longlat +ellps=WGS84")){
  if (!is.matrix(x)) stop("projectCRS expects an nx2 matrix of numeric points.")
  offset <- as.numeric(unlist(strsplit(config$location$netOffset, ",")))
  ref <- cbind(x[, 1] - offset[[1]], x[, 2] - offset[[2]])
  spoints <- sp::SpatialPoints(ref, proj4string = config$CRS)
  sp::spTransform(spoints, crs)
})

#' @title Generate SUMO Configuration
#' @name genConfig
#' @description Generates a sumo configuration file suitable for simulation
sumo$methods(genConfig = function(begin, end, file="tmp.sumocfg"){
  if (is(network, "uninitializedField")) stop("network file is uninitialized")
  if (is(config$routes, "uninitializedField")) stop("routes file is uninitialized")

  config_doc <- XML::newXMLDoc()
  config_tag <- XML::newXMLNode("configuration", doc = config_doc)

  input <- XML::newXMLNode("input", parent = config_tag)
  XML::newXMLNode("net-file", attrs = c(value=network), parent = input)
  XML::newXMLNode("route-files", attrs = c(value=config$routes), parent = input)

  ## simulation time
  time <- XML::newXMLNode("time", parent = config_tag)
  XML::newXMLNode("begin", attrs = c(value=begin), parent = time)
  XML::newXMLNode("end", attrs = c(value=end), parent = time)
  # XML::newXMLNode("step-length", attrs = c(value=0.01), parent = time)

  ## disable the automatic removal of vehicles which wait too long in front of an intersection.
  XML::newXMLNode("time-to-teleport", attrs = c(value="-1"), parent = config_tag)

  # Configuration file
  XML::saveXML(config_doc, paste0(config$SUMO_TMP, file))
  configuration <<- paste0(config$SUMO_TMP, file)
})

#' @title simulate
#' @name simulate
#' @description Uses a SUMO road network to generate road traffic
sumo$methods(simulate = function(flags=""){
  if (is(configuration, "uninitializedField")){
    stop("SUMO configuration file not found. Has genConfig been called?")
  } else {
    cat(paste0("sumo -c ", configuration, " ", flags))
    command(paste0("sumo -c ", configuration, " ", flags))
  }
})


sumo$methods(vehicleProbe = function(probe = list(id="probe1", type="DEFAULT_VEHTYPE", freq="1"), flags=""){
  config_doc <- XML::newXMLDoc()
  additional <- XML::newXMLNode("additional", doc = config_doc)
  probe$file <- paste0(config$SUMO_TMP, "tmp_vehprobe.xml")
  vTypeProbe <- XML::newXMLNode("vTypeProbe", parent = additional, attrs = probe)
  settings_files <- paste0(config$SUMO_TMP, "tmp_vehprobe_settings.xml")
  XML::saveXML(config_doc, file = settings_files)
  probe_cmd <- paste0("sumo -c ", configuration, " ", flags, " --additional-files ", settings_files)
  cat(probe_cmd)
  status <- command(probe_cmd)

  if (status == 0){
    timesteps <- xml2::as_list(xml2::read_xml(probe$file))
    return(data.table::rbindlist(unname(lapply(timesteps, function(veh_list) {
      data.table::rbindlist(lapply(veh_list, function(vehicle) attributes(vehicle)))
    })), idcol = "timestep"))
  } else {
    warning(paste0("SUMO returned non-0 status code: ", status))
    return(NULL)
  }
})

sumo$methods(vehicleAcc = function(traj_file, flags=""){
  if (file.exists(traj_file)){
    timesteps <- xml2::read_xml(traj_file)
    motion_states <- xml2::xml_attrs(xml2::xml_find_all(timesteps, "//motionState"))
    return(data.table::rbindlist(lapply(motion_states, as.list)))
  } else {
    warning(paste0("File doesn't exist: ", traj_file))
    return(NULL)
  }
})

sumo$methods(getFCD = function(fcd_file, flags=""){
  if (file.exists(fcd_file)){
    fcd <- xml2::read_xml(fcd_file)
    timesteps <- xml2::xml_find_all(fcd, "//timestep")
    steps <- xml2::xml_attr(timesteps, "time")

    ## Extract vehicle info per timestep
    res <- new.env(parent=emptyenv())
    mapply(function(key, container) invisible(res[[key]] <- xml2::xml_attrs(xml2::xml_children(container))), steps, timesteps)
    res <- as.list(res)

    ## Cast if columns match schema and return
    result1 <- lapply(res, function(states) data.table::rbindlist(lapply(states, as.list)))
    result2 <- list()

    i <- 1
    while(i < 25){
      result2 <- try(data.table::rbindlist(result1, idcol="timestep"), silent = T)
      if ("try-error" %in% class(result2)){ i <- i + 1 }
      else break;
    }

    if (i == 25){
      result2 <- do.call(rbind, result1)
      result2$timestep <- length(unlist(unname(sapply(names(result1), function(timestep) rep(as.numeric(timestep), nrow(result1[[timestep]]))))))
    }
    result <- as.data.table(result2)

    ## Convert types
    base_schema <- c("timestep", "id", "x", "y", "angle", "speed", "pos", "slope")
    if (all(base_schema %in% colnames(result))){
      col_types <- c(as.numeric, as.integer, as.numeric, as.numeric, as.numeric, as.factor, as.numeric, as.numeric,
                     as.character, as.numeric, as.integer)
      result[, (base_schema) := .(as.numeric(timestep),
                             as.integer(id),
                             as.numeric(x),
                             as.numeric(y),
                             as.numeric(angle),
                             as.numeric(speed),
                             as.numeric(pos),
                             as.numeric(slope))]
      result <- result[order(timestep)]
    }
    return(result)
  } else {
    stop(paste0("File doesn't exist: ", fcd_file))
  }
})

#' @title Get pedestrian edges
#' @name getPedestrianEdges
#' @description Returns the edges that have any lane allowing pedestrian traffic
sumo$methods(getPedestrianEdges = function(){
  ## Get all edges that allow pedestrians
  lanes <- xml2::xml_find_all(getNetwork(), xpath = "//lane")
  lane_indices <- sapply(xml2::as_list(lanes), function(lane) {
    if ("allow" %in% names(attributes(lane))){
      allowances <- attr(lane, "allow")
      return("pedestrian" %in% unlist(strsplit(allowances, " ")))
    } else FALSE;
  })
  edges <- xml2::xml_parent(lanes[lane_indices])
  return(edges)
})

#' @title Get target nodes
#' @name getTargetNodes
#' @description Returns the target nodes (junctions) of each edges in a given route
sumo$methods(getTargetNodes = function(){
  routes <- getRoutes()
  pids <- xml2::xml_attr(xml2::xml_find_all(routes, "//person"), "id")
  edge_routes <- xml2::xml_attr(xml2::xml_find_all(routes, "//person/walk"), "edges")

  ## Get all edges that allow pedestrians
  edges <- getPedestrianEdges()
  edge_info <- data.table::rbindlist(lapply(xml2::xml_attrs(edges), as.list), fill = T)

  ## Extract the sequential edges to be traveled along
  edge_routes <- lapply(edge_routes, function(er) unlist(strsplit(er, split = " ")))
  data.table::setkey(edge_info, "id")

  ## Extract nodes traversed by the edge routes, inclusive of the start, exclusive of the end
  ## (since cannot be determined)
  target_nodes <- mapply(function(ers, i){
    if (length(ers) == 1) return(NULL)
    # print(i)
    mapply(function(s, e) {
      # cat(c(s, e))
      pair <- edge_info[ers[s:e], .(id, from, to)]
      res <- names(which(table(c(pair$from, pair$to)) == 2))
      ifelse(length(res) > 1, "UNKNOWN", res) # || is.null(res) || is.na(res)
    }, 1:(length(ers) - 1L), 2:length(ers))
  }, edge_routes, 1:length(edge_routes))
  return(target_nodes)
})


sumo$methods(testNetwork = function(output_file = "hello.net.xml"){
  node_doc <- xml2::xml_new_document()
  nodes <- xml2::read_xml("<nodes/>")
  node_xy <- matrix(c(0, 0, 10, 0, 10, 10), byrow = T, ncol=2)
  for (i in 1:3){
    node <- xml2::read_xml("<node/>")
    xml2::xml_attrs(node) <- list(id = i, x = node_xy[i, 1], y = node_xy[i, 2])
    xml2::xml_add_child(nodes, node)
  }
  xml2::xml_add_child(node_doc, nodes)
  xml2::write_xml(node_doc, file = paste0(config$SUMO_TMP, "hello.nod.xml"))

  edge_doc <- xml2::xml_new_document()
  edges <- xml2::read_xml("<edges/>")
  for (i in 1:2){
    edge <- xml2::read_xml("<edge/>")
    xml2::xml_attrs(edge) <- list(id = paste0("e", i), from = i, to = i + 1)
    xml2::xml_add_child(edges, edge)
    lane <- xml2::read_xml("<lane/>")
    xml2::xml_attrs(lane) <- list(allow="pedestrian", id = paste0("e", i, "_0"),
                                  index = "0",  allow="pedestrian", speed = "2.78",
                                  length = "1", width = "2")
    xml2::xml_add_child(edge, lane)
  }
  # edge <- xml2::read_xml("<edge/>")
  # xml2::xml_attrs(edge) <- list(id = paste0("e", 3), from = 3, to = 1)
  # xml2::xml_add_child(edges, edge)
  xml2::xml_add_child(edge_doc, edges)
  xml2::write_xml(edge_doc, file = paste0(config$SUMO_TMP, "hello.edge.xml"))

  test_cmd <- paste0("netconvert",
                   " --node-files=", paste0(config$SUMO_TMP, "hello.nod.xml"),
                   " --edge-files=", paste0(config$SUMO_TMP, "hello.edge.xml"),
                   " --output-file=", paste0(config$SUMO_TMP, output_file))
  command(test_cmd)
})




