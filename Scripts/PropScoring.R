#trt_ctrl<-tar_read(trt_ctrl)
#dat=DoPropScoring(trt_ctrl)
#grid.list=BuildSpatialGrid(dat,100000)
#graph=BuildNeighborhood(grid.list)
#model_df=Assemble_Model_DF(dat,roads1)

#install.packages('INLA',
#                 repos = c(INLA = 'https://inla.r-inla-download.org/R/stable'),
#                 dep   = TRUE)
#there is no package called ‘fmesher’
#install.packages("fmesher")
#library(fmesher)
#library(INLA)
#mod.list=DoPropScoring(trt_ctrl,roads1,res)
### Wrapper for whole thing, goes in targets pipeline
DoPropScoring<-function(trt_ctrl,grid_resolution){
  dat=GetTimeSeq(trt_ctrl)
  print(2)
  grid.list=BuildSpatialGrid(dat,grid_resolution)
  print(3)
  graph=BuildNeighborhood(grid.list)
  print(4)
  model_df=Assemble_Model_DF(grid.list$dat)
  print(5)
  mod.list=Run_PropScore_Model(model_df,graph)
  return(mod.list)
}

### -Discrete time period, e.g. year/season and trims out trajIDs where uniqueid is repeated in same season
GetTimeSeq<-function(trt_ctrl){
  
  #separate trt/ctrl
  its_seq=trt_ctrl$trt
  geo_ctrls=trt_ctrl$ctrl
  
  #label with type, format
  its_seq$type="trt"
  geo_ctrls$type="ctrl"
  geo_ctrls$period="before"
  its_seq$trajID=as.character(its_seq$trajID)
  
  #bind back together
  dat=dplyr::bind_rows(geo_ctrls,its_seq)
  
  #get min and max datetimes for whole dataset (will set start of year/season integer)
  ymin=min(min(unique(lubridate::year(dat$t_))))
  ymax=max(max(unique(lubridate::year(dat$t_))))
  
  #make expanded season-year dataframe
  seasons=c("springmigr","calving","insect","latesummer","fallmigr","winter")
  ys=expand.grid(year=ymin:ymax,
                 season=seasons) %>%
    arrange(year,season)
  ys$ys=paste(ys$year,ys$season,sep="_")
  ys$ysID=1:nrow(ys)
  ys=ys[,c(3,4)]
  
  #get key for matching with year/season integer
  dat$ys=paste(lubridate::year(dat$t_),dat$season,sep="_")
  
  #after attributing, check for geolocations set as winter with months between 1-3. 
  #These should be assigned to winter of previous year.
  dat$ys[lubridate::month(dat$t_)<4]=paste((lubridate::year(dat$t_[lubridate::month(dat$t_)<4])-1),dat$season[lubridate::month(dat$t_)<4],sep="_")
  #NA seasons should be NA in joinkey
  dat$ys[is.na(dat$season)]=NA

  dat=dplyr::left_join(dat,ys,by="ys")

  #filter dups-- same season/uniqueid, different assignment
  #means long before period in treatment
  #remove the control one
  dups=dat %>% dplyr::summarise(n=n_distinct(type),
                                .by = c(uniqueid, ysID)) %>% filter(n>1)
  
  dups$dupkey=paste(dups$uniqueid,dups$ysID,sep="_")
  dups=dups[,c(4,3)]
  dat$dupkey=paste(dat$uniqueid,dat$ysID,sep="_")
  
  dat=left_join(dat,dups,by="dupkey")  
  
  #for every trajID that was matched, filter out control
  dat2=dat %>% filter(!(!is.na(n)&type=="ctrl"))

  return(dat2)
}

### -Assign each time segment to a spatial grid and build a neighborhood graph
#### - build grid for all points in herd
#### - for each season segment, get median spatial coordinates
#### - overlay medians to grid, get cell for each season segment
BuildSpatialGrid<-function(dat,grid_resolution){
  require(sf)
  require(spdep)
  require(dplyr)
  
  #Create uniqueid to get medians, trajID*ysID
  dat$propID=paste(dat$trajID,dat$ysID,sep="_")
  
  #Get median locations per propID
  dat.m=dat %>% dplyr::group_by(propID) %>%
    dplyr::summarise(Xm=median(x_),Ym=median(y_))
  
  # Convert GPS to sf object — set your CRS
  gps_sf <- sf::st_as_sf(dat.m,
                     coords = c('Xm', 'Ym'),
                     crs = st_crs(6393))
  
  #Set grid resolution in map units (usually meters)
  #grid_resolution <- grid_resolution  # meters
  
  # Create grid over study area
  study_bbox <- st_bbox(gps_sf)
  grid <- sf::st_make_grid(sf::st_as_sfc(study_bbox),
                       cellsize = grid_resolution,
                       square   = TRUE) %>%
    sf::st_sf() %>%
    dplyr::mutate(grid_cell = row_number())
  
  # Assign each GPS fix to a grid cell
  
      grid_cells=
        sf::st_join(gps_sf, grid) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(grid_cell)
      
      dat.m=dplyr::bind_cols(dat.m,grid_cells)
      
      dat_out=dplyr::left_join(dat,dat.m,by="propID")
      
      return(list("dat"=dat_out,"grid"=grid))
}

#### - build the neighborhood graph
BuildNeighborhood<-function(grid.list){
  require(spdep)
  require(INLA)
  
  dat=grid.list$dat
  grid=grid.list$grid
  # Build queen contiguity neighborhood (shares edge or corner)
  nb <- poly2nb(grid, queen = TRUE)
  
  # Convert to INLA graph format
  nb2INLA('grid_neighborhood.graph', nb)
  inla_graph <- inla.read.graph('grid_neighborhood.graph')

  # Plot neighborhood structure
  #plot(st_geometry(grid), border = 'grey')
  #plot(nb, st_centroid(st_geometry(grid)), add = TRUE, col = 'red')
  return(inla_graph)
  
}

### -Assemble model dataframe
    #Extract spatial covariates that would predict a road interaction 
      #e.g. distance to nearest road
    #one row per animal per trajectory segment
    #in current structure with season, don't need previous time period-- considering them independent
    #this may be needed if using daily, weekly, monthly, etc.
Assemble_Model_DF<-function(dat){
  dat2=dat %>% 
    dplyr::group_by(herd,uniqueid,type,trajID,propID,period,season,ysID) %>% 
    dplyr::select(Xm,Ym,grid_cell) %>%
    unique()
  
  #calc dist to each road
  #dat2sf<-st_as_sf(dat2,coords=c("Xm","Ym"),crs=st_crs(6393))
  
  #get mindist for each point
  #dists=st_distance(dat2sf,roads1)
  #dat2$road_mindist=apply(dists,1,min)
  
  # Encode factor variables as needed
  model_df <- dat2 %>%
    dplyr::mutate(season = as.factor(season),
           type = as.factor(type),
           uniqueid=as.factor(uniqueid))
  
  model_df$type=as.character(model_df$type)
  model_df$type_idx=model_df$type
  model_df$type_idx[model_df$type=="trt"]<-1
  model_df$type_idx[model_df$type=="ctrl"]<-0
  model_df$type_idx=as.numeric(model_df$type_idx)
  model_df=model_df[complete.cases(model_df),]
  
  return(model_df)
  
}

### -Run propensity score model
Run_PropScore_Model<-function(model_df,graph){
  formula_ps <- type_idx ~
    
    #fixed effect of season
    season+
    
    #random effect of individual
    f(uniqueid,model="iid")+

    # ── Spatial random effect (CAR / Besag model) ──────────────────
    # Accounts for residual spatial clustering in treatment probability
    f(grid_cell,
      model  = 'besag',
      graph  = graph,
      scale.model = TRUE,
      hyper  = list(prec = list(prior = 'loggamma', param = c(1, 0.001))))+
    
    #Temporal random effect (random walk)
    f(ysID,
      model  = 'rw1',
      hyper  = list(prec = list(prior = 'loggamma', param = c(1, 5e-5))))
  
  #4.4 Fit the Model
  model_ps <- inla(
    formula  = formula_ps,
    family   = 'binomial', 
    data     = model_df,
    
    # Compute fitted values (the propensity scores)
    control.predictor = list(
      compute = TRUE,
      link    = 1 #response scale
    ),
    
    # Compute model fit criteria
    control.compute = list(
      dic  = TRUE,
      waic = TRUE,
      cpo  = TRUE
    ),
    
    # Weakly informative priors on fixed effects
    control.fixed = list(
      mean        = 0,
      prec        = 0.001,
      mean.intercept = 0,
      prec.intercept = 0.001
    )
  )
  
  # Quick model summary
  mod=summary(model_ps)
  #model_df=model_df[,c(1:11)]
  #4.5 Extract Propensity Scores
  # Fitted values from INLA are on the probability scale
  # (because we set link = 1 above)
  model_df=model_df %>%
    cbind(
      propensity_score = model_ps$summary.fitted.values[, 'mean'],
      ps_lower         = model_ps$summary.fitted.values[, '0.025quant'],
      ps_upper         = model_ps$summary.fitted.values[, '0.975quant']
    )
  
  # Inspect the distribution of propensity scores
  # Treatment and control should overlap substantially (positivity assumption)
  p1=ggplot(model_df, aes(x = propensity_score, fill = factor(type))) +
    geom_density(alpha = 0.5) +
    labs(title = 'Propensity Score Distribution',
         x     = 'Propensity Score',
         fill  = 'type') +
    facet_wrap(~season)+
    theme_minimal()
  
  #table(model_df$poor_overlap, model_df$type)
  return(list("model_df"=model_df,"plot"=p1,"fit"=mod))
}





