# _targets.R


# Targets setup --------------------
setwd(this.path::this.dir())

#load libraries for targets script
#install.packages("geotargets", repos = c("https://njtierney.r-universe.dev", "https://cran.r-project.org"))
library(targets)
library(tarchetypes)
library(geotargets)

# This hardcodes the absolute path in _targets.yaml, so to make this more
# portable, we rewrite it every time this pipeline is run (and we don't track
# _targets.yaml with git)
tar_config_set(
  store = file.path(this.path::this.dir(),("_targets")),
  script = file.path(this.path::this.dir(),("_targets.R"))
)

#Source functions in pipeline
lapply(list.files(file.path("Scripts"), full.names = TRUE, recursive = TRUE), source)

#set options
#this option has been deprecrated, need to update to new version...
#options(clustermq.scheduler="multicore")

#Load packages
tar_option_set(packages = c("tidyr",
                            "purrr",
                            "stringr",
                            "dplyr",
                            "sf",
                            "amt",
                            "raster",
                            "terra",
                            "lubridate",
                            "ggplot2",
                            "mapview",
                            "RcppParallel",
                            "fitdistrplus",
                            "crawl",
                            "pander",
                            "furrr",
                            "forcats",
                            "RcppHungarian",
                            "data.table"))

# Pipeline ---------------------------------------------------------

list(
  
  ## Input data paths -----  
  
  ### Input paths to raw data -----------
  tar_target(Input_folder,
             file.path("Input"),
             format="file"),
  
  ## Read input data ----- 
  
  #*Note, the following objects were output from Explore_Caribou_Data_Pipeline
  #*caribou_geolocations.rds is just consolidated/tidied df with all geolocations
  #*roads.rds is an sf file with roads
  #*road_summary_list.rds describes all roads that intersect with caribou
  #*nsdat is the north slope shapefile data, source linked below
  ### Read caribou geolocations rds -------------
  tar_target(geo0,ReadRDS(Input_folder,"caribou_geolocations.rds")),
  
  ### Read Roads rds -------------
  tar_target(roads0,ReadRDS(Input_folder,"roads.rds")),
  
  ### Read North slope data -------------
  #from https://catalog.northslopescience.org/dataset/2663
  tar_target(nsdat,ReadFormatNS(Input_folder)),
  
  ## Format input data -------------
  
  ### Merge and format road shapefiles ---------
    #add unique ID for each object and combine
    #standardize column names
    #from northslope data, removing first couple layers that are small pads/runway features
    #Inputs: AK roads and North Slope shapefiles
    #Outputs: roads1, sf data frame with all roads
  tar_target(roads1,MergeShapefiles(roads0,nsdat)),
  
  ### Remove caribou with < 16 weeks of data ---------
    #Inputs: 
      #geo0, data frame of geolocation data
      #datecolname, string indicating column name with datetimes in geo0
      #mindur, units object indicating the minimum time duration needed for each individual in geolocation data
    #Notes: should probably add user-input way to specify animalID ("uniqueid"), or have function up top that formats data to correct colnames
  tar_target(geo1,TrimDuration(geo0,"datetime_UTC",lubridate::duration(16,units="weeks"))),
  
  ### Create object with road buffer polygons -------
    #Inputs:
      #roads1, sf data.frame with road multilinestrings
      #dbuff, numeric, distance in meters to set buffers
    #Outputs:
      #roadbuffs, sf object with road buffer polygons
  tar_target(roadbuffs,Road_Buffer(roads1,20000)),
  
  ### Get summary of caribou intersections with buffers -------
  #This takes a while to run...
    #Inputs:
      #roads1, sf data.frame with road multilinestrings
      #roadbuffs, sf object with road buffer polygons
      #geo1, data.frame with geolocation data, trimmed with minimum duration
      #test, boolean, if TRUE, selects three individuals at random and creates short rb_summary_list. For troubleshooting function.
    #Output: rb_summary_list, nested_track, nested by uniqueid (caribou)
        #*data- amt formatted track data 
        #*lines- track data formatted to sf lines
        #*ints- roads that intersect with each caribou track
        #*n_ints- number of roads in ints intersecting with each caribou
        #*data_uid- amt formatted track data with ID for each geolocation in track
        #*data_sf- sf points formatted track data
        #*ints_buffs- sf points, geolocations that intersect with road buffers
  tar_target(rb_summary_list,SummarizeRoadIntersections(roads1,roadbuffs,geo1,test=FALSE)),
 
  ### Get geolocation set with clusterIDs (unique interactions with road) -------
  #Assigns integers (1,-1) that identify whether a contiguous segment is an intersection with a road (1) or not (-1)
  #Assigns unique identification numbers for each contiguous segment with/without a road intersection
    #Input: rb_summary_list (see SummarizeRoadIntersections notes for more detail)
    #Output: geo_c, geolocation data frame with uniqueIDs for contiguous road segments (clusterIDs) and intIDs (int)
  tar_target(geo_c,SetRoadIntID(rb_summary_list)),
  
  ### Describe road intersections -----
  #Get durations during, before, after each unique intersection with road buffers
  #This will be used for any filtering or matching steps later on
    #Input: geo_c, geolocation data frame with cluster/int uniqueIDs
    #Output: ints, grouped data frame for each contiguous trajectory
      #*mint- start datetime of intersection with road buffer
      #*maxt- end datetime of interesection with road buffer
      #*difft- total duration of intersection with road buffer in days
      #*nlocs- number of geolocations in intersection with road buffer
      #*dur_before- number of days in continuous trajectory before intersection with the road buffer
      #*dur_after- number of days in continuous trajectory before intersection with the road buffer
      #*month- 
  tar_target(ints,DescribeRoadInts(geo_c)),
  
  ### Filter intersections by minimum duration set for before/after ------
  #removes continuous trajectories that are shorter than these criteria
    #Input: 
      #*ints, grouped df describing characteristics of each traj
      #*filter_before: integer, number of days minimum for before intersection with road
      #filter_after: integer, number of days minimum for after intersection with road
    #Output:
      #same as ints data frame, but removed intersections that do not have before/after periods meeting the criteria
  tar_target(ints_its,FilterInts(ints,filter_before=28,filter_after=28)),

  ### Find control groups according to filter (min day span) ------
    #Input: 
      #*geo_c, geolocation data frame with traj IDs
      #*filter, integer, minimum number of days for control traj segment
    #Output: grouped data frame with uniqueid/clustIDs with duration greater than filter
  tar_target(ctrls,FindCtrlGroups(geo_c,84)),
  
  ### Make season matrix ------
  #Makes matrix to assign seasons
    #Input: matrix with season names, start dates, and end dates of each season, all dates are strings in MM/DD format
    #Output: same as input
  tar_target(seasons,MakeSeasons(seasons=c("calving","insect","latesummer","fallmigr","winter","springmigr"),
                                 strt.dts=c("05/28","06/15","07/15","09/01","12/01","04/01"),
                                 end.dts=c("06/14","07/14","08/31","11/30","03/31","05/27")
                                 )),
  
  ### Add seasons to geolocations (for now just wah) ------
    #Input: 
      #*geo_c, geolocation data frame with traj IDs
      #*t_, column with datetimes
      #*seasons, character matrix with season names, start and end dates 
      #*herd, string indicating herd, matches herd column in geo_c
  tar_target(geo_cs,assign_season(geo_c,t_,seasons,herd="wah")),

  ### Output intersections with period (before, after etc.) for each set ------
  #This function uses the uniqueid/clusterID to pull selected intersections and associated before/after trajs, and labels trajs by period (before, during, after)
    #Input:
      #*geo_cs
      #*ints_its
    #Output: its_seq
  tar_target(its_seq,FormatClusterSets(geo_cs,ints_its)),
  
  ### Output control data geolocations only
  #This function is analogous to FormatClusterSets, but pulls control data
    #Input:
    #Output: geo_ctrls
  tar_target(geo_ctrls,FormatCtrls(geo_cs,ctrls)),
  
  ### Assign season to each unique track segment using set of criteria ------
  #Sometimes trajectories overlap seasons. This function creates a new unique segment ID that assigns an ID to each seasonal portion of the segment for iterating
    #Input:
    #Output:
  tar_target(its_seq02,AssignClusterSeasons(its_seq)),
  tar_target(geo_ctrls02,AssignClusterSeasons(geo_ctrls)),
  
  ### Get start/end date for each traj for counterfac matching
  tar_target(its_seq2,GetTrajDates(its_seq02)),
  tar_target(geo_ctrls2,GetTrajDates(geo_ctrls02)),
  
  ### Get movement model parameters -------
  #This runs movement models for each unique segment
    #Input
    #Output
  tar_target(ctrl_locs_fit,RunMovementModels(geo_ctrls2,"wah")),
  tar_target(tbl_locs_fit,RunMovementModels(its_seq2,"wah")),

  ### Calculate mean dist and time for each segment -------
    #Input
    #Output
  tar_target(tbl_locs_fit2,CalculateMeanVelocity(tbl_locs_fit)),
  tar_target(ctrl_locs_fit2,CalculateMeanVelocity(ctrl_locs_fit)),

  ### Combine model params and mean velocity into one data frame, unnest ------
    #Input
    #Output
  tar_target(tbl_locs_fit3,CombineModelParams(tbl_locs_fit2)),
  tar_target(ctrl_locs_fit3,CombineModelParams(ctrl_locs_fit2)),
    
  ### Match control and treatment trajectories
    #for now this is just with the date spans
    #may want to add other criteria later
  tar_target(matches,Match_Ctrl_Trt(tbl_locs_fit3,ctrl_locs_fit3)),
  
  ### Remove duplicated pairs
    #uses Hungarian matching to maximize overlap between dates
    #if/when implement additional covariates, want to use propensity score matching probably
    #other trimming (ie correlated movement paths) would be done prior to this step
  tar_target(pairs,Hungarian_matching(matches)),
  
  ### Filter out unpaired and trim durations of pairs accordingly
  
  
  ## Visualization --------
  
  ### Make plots, for each herd, showing spread by season -------
  #only showing intersections that have cutoff of 28 days before and after intersection event
  tar_target(ints_cah,VizDescribeInts(ints,
                                      seasons=data.frame(
                                        season = c("Winter","Spring","Summer","Fall"),
                                        start  = c(1,30,90,275),
                                        end    = c(30,90,275,365),
                                        label=c("Feb-1","March-1","June-1","October-1")),
                                      alpha=0.5,
                                      colors= c("grey85", "grey95"),
                                      label_vjust=0.1,
                                      label_size=2,
                                      "cah",
                                      filter=28)),
  
  tar_target(ints_wah,VizDescribeInts(ints,
                                      seasons=data.frame(
                                        season = c("Winter","Spring","Summer","Fall"),
                                        start  = c(1,30,90,275),
                                        end    = c(30,90,275,365),
                                        label=c("Feb-1","March-1","June-1","October-1")),
                                      alpha=0.5,
                                      colors= c("grey85", "grey95"),
                                      label_vjust=-0.2,
                                      label_size=2,
                                      "wah",
                                      filter=28)),
  
  tar_target(ints_nelchina,VizDescribeInts(ints,
                                           seasons=data.frame(
                                             season = c("Winter","Spring","Summer","Fall"),
                                             start  = c(1,30,90,275),
                                             end    = c(30,90,275,365),
                                             label=c("Feb-1","March-1","June-1","October-1")),
                                           alpha=0.5,
                                           colors= c("grey85", "grey95"),
                                           label_vjust=0.1,
                                           label_size=2,
                                           "nelchina",
                                           filter=28)),
  
  tar_target(ints_denali,VizDescribeInts(ints,
                                         seasons=data.frame(
                                           season = c("Winter","Spring","Summer","Fall"),
                                           start  = c(1,30,90,275),
                                           end    = c(30,90,275,365),
                                           label=c("Feb-1","March-1","June-1","October-1")),
                                         alpha=0.3,
                                         colors= c("grey85", "grey95"),
                                         label_vjust=0.1,
                                         label_size=2,
                                         "denali",
                                         filter=28)),
  
  

  #plot x coordinate velocity diffs
  tar_target(plot_vx_diffs,Plot_Diffs_by_Season(tbl_locs_fit3,velocity=TRUE,v="vx")),
  #plot y coordinate velocity diffs
  tar_target(plot_vy_diffs,Plot_Diffs_by_Season(tbl_locs_fit3,velocity=TRUE,v="vy")),
  #plot sigma velocity diffs
  tar_target(plot_sigma_diffs,Plot_Diffs_by_Season(tbl_locs_fit3,response="ln sigma (Intercept)")),
  #plot tau velocity diffs
  tar_target(plot_tau_diffs,Plot_Diffs_by_Season(tbl_locs_fit3,response="ln beta (Intercept)")),
  
  #Plot dates between control/treatment trajectories before matching
  tar_target(datespan_ctrl_trt,PlotTrajDates(tbl_locs_fit3,ctrl_locs_fit3))
  
  )
