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
  
  ## 1. Read input data ----- 
  
  #*Note, the following objects were output from Explore_Caribou_Data_Pipeline
  #*caribou_geolocations.rds is just consolidated/tidied df with all geolocations
  #*roads.rds is an sf file with roads
  #*nsdat is the north slope shapefile data, source linked below
  ### Read caribou geolocations rds -------------
  tar_target(geo0,ReadRDS(Input_folder,"caribou_geolocations.rds")),
  
  ### Read seasons rds -------------
  tar_target(seasons,ReadRDS(Input_folder,"seasons.rds")),
  
  ### Read Roads rds -------------
  tar_target(roads0,ReadRDS(Input_folder,"roads.rds")),
  
  ### Read North slope data -------------
  #from https://catalog.northslopescience.org/dataset/2663
  tar_target(nsdat,ReadFormatNS(Input_folder)),
  
  ## 2. Format input data -------------
  
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

  ## 3. Identify treatment and control trajectories -------------
  
  ### Make season matrix ------
  #Makes matrix to assign seasons
  #Input: matrix with season names, start dates, and end dates of each season, all dates are strings in MM/DD format
  #Output: same as input
  #tar_target(seasons,MakeSeasons(seasons=c("calving","insect","latesummer","fallmigr","winter","springmigr"),
  #                               strt.dts=c("05/28","06/15","07/15","09/01","12/01","04/01"),
  #                               end.dts=c("06/14","07/14","08/31","11/30","03/31","05/27")
  #)),
  
  #Find road interactions (treatment traj)
  #Summarise durations before, during and after road interactions
  #Filter treatment trajectories by before/after durations
  #Pull road interactions plus before/after durations into nested data frame
  #Find control trajectories (no road interactions)
  #Filter control trajectories by minimum duration
  #Output list with ctrl and treatment trajectory df
  tar_target(trt_ctrl,ProcessTrtCtrl(rb_summary_list,
                                  filter_before=28,
                                  filter_after=28,
                                  ctrl_filter=84,
                                  herd="wah",
                                  seasons,
                                  t_=t_)),
  
  ## 4. Match control and treatment trajectories ------
  ### Determine overlap times between each treatment and control trajectory 
  ### Use Hungarian matching to reach global maximum overlap 
  ### Filter trailing end of each pair, and unpaired traj
  tar_target(pgeo,Match_Ctrl_Trt(trt_ctrl$trt,trt_ctrl$ctrl)),
  
  ### Check for correlated movement paths
  
  ## 5. Fit movement models ----
  ### Runs movement models, calculates mean velocity, returns tidy output
  tar_target(movepairs,GetMovementParameters(pgeo,"wah")),

  ### Evaluate movement model fits
  
  ### Check BACI assumptions
  
  ### Run linear models
  
  ## 6. Visualization --------
  
  ### Forest plots of movement parameters and high/low conf intervals
  tar_target(sigma_for,BACI_intxn_plot(movepairs,"sigma")),
  tar_target(beta_for,BACI_intxn_plot(movepairs,"beta")),
  
  ### Baci interaction plots
  tar_target(baci_sigma,BACI_intxn_plot(movepairs,"`estimate_ln sigma (Intercept)`")),
  tar_target(baci_beta,BACI_intxn_plot(movepairs,"`estimate_ln beta (Intercept)`")),
  tar_target(baci_vx,BACI_intxn_plot(movepairs,"vx")),
  tar_target(baci_vy,BACI_intxn_plot(movepairs,"vy")),
  
  ### Data summaries
  tar_target(any_dupl_trt,Check_Pseudo_Trt(movepairs,pgeo))
  
  ### Summarize road interactions in data
  #which roads, how many times represented, etc.
  
  )
