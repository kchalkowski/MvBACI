
## 1. Read input data ----- 
### Read RDS objects------
ReadRDS<-function(path,filename){
  readRDS(file.path(path,filename))
}

Filter_Roads<-function(roads1,filter){
  roads1 %>% st_cast("LINESTRING")
  
  rd1=roads1 %>% filter(dataset=="roads")
  rd2=roads1 %>% filter(dataset=="north_slope")
  rd3=roads1 %>% filter(dataset=="taps")
  
  rd1l=rd1 %>% st_cast("LINESTRING")
  rd1l$lengths=as.numeric(st_length(rd1l))
  rd1f=rd1l[rd1l$lengths>=filter,]
  
  rd2$lengths=as.numeric(st_length(rd2))
  rd2f=rd2[rd2$lengths>=filter,]
  roads_out=rbind(rd1f,rd2f)
  return(roads_out)
}


### Read and format north slope spatial data ------
ReadFormatNS<-function(Input_folder){
  
  path=file.path("Input","NSSI_data")
  taps_path=file.path(path,"312-taps-pipeline")
  
  #pull taps
  taps=sf::st_read(taps_path)
  
  #clean colname to match other shapefiles
  colnames(taps)<-c("name","geometry")
  
  #pull multiple ns shapefiles
  ns_path=file.path(path,"nsinfra_v13_shapefile","NSInfra_V13_shapefile")
  ns_files=list.files(ns_path,full.names=TRUE)
  ns_files=ns_files[grep(".shp$",list.files(ns_path))]
  northsl=vector(mode="list",length=length(ns_files))
  for(i in 1:length(northsl)){
    northsl[[i]]=sf::st_read(ns_files[[i]])
  }

  #trim columns
  northsl[[1]]=northsl[[1]][,c(2,3,5,10)]
  northsl[[2]]=northsl[[2]][,c(1,4)]
  northsl[[3]]$UnitName[is.na(northsl[[3]]$UnitName)]<-""
  northsl[[3]]$Route_Name[is.na(northsl[[3]]$Route_Name)]<-""
  northsl[[3]]$UnitName=paste(northsl[[3]]$UnitName,northsl[[3]]$Route_Name)
  northsl[[3]]$UnitName=stringr::str_trim(northsl[[3]]$UnitName)
  northsl[[3]]=northsl[[3]][,c(1,3,6)]
  
  #clean up names
  colnames(northsl[[1]])<-c("type","name","unitname","geometry")
  colnames(northsl[[2]])<-c("unitname","geometry")
  colnames(northsl[[3]])<-c("name","unitname","geometry")
  
  #remove z coordinates  
  northsl[[1]]=sf::st_zm(northsl[[1]],drop=TRUE)
  northsl[[2]]=sf::st_zm(northsl[[2]],drop=TRUE)
  northsl[[3]]=sf::st_zm(northsl[[3]],drop=TRUE)
  
  #transform to same projection as geo_sf
  northsl[[1]]=sf::st_transform(northsl[[1]],crs=sf::st_crs(taps))
  northsl[[2]]=sf::st_transform(northsl[[2]],crs=sf::st_crs(taps))
  northsl[[3]]=sf::st_transform(northsl[[3]],crs=sf::st_crs(taps))
  
  #output list
  return(list("nsl1"=northsl[[1]],"nsl2"=northsl[[2]],"nsl3"=northsl[[3]],"taps"=taps))
  
}

## 2. Format input data -------------

### Merge and format road shapefiles ---------
MergeShapefiles<-function(roads0,nsdat){
  
  roads0=sf::st_transform(roads0,sf::st_crs(6393))
  nsdat[[3]]=sf::st_transform(nsdat[[3]],sf::st_crs(6393))
  nsdat[[4]]=sf::st_transform(nsdat[[4]],sf::st_crs(6393))
  
  
  roads0=sf::st_zm(roads0,drop=TRUE)
  
  
  #trim road cols
  roads0=roads0[,c(8,9,10,13,17,19)]
  
  #Need to create unique ids for each row of road
  roads0=roads0 %>% group_by(Route_ID) %>% dplyr::mutate(countID=1:n()) %>% ungroup()
  roads0$uniqueid=paste(roads0$Route_ID,roads0$countID,sep="_")
  
  #decision: merge taps and nsl3 to roads to get road buffers
  #nsl1 and nsl2 are small features like pads and runways... does include what are technically roads but are not cross-cutting roads, more like developed landscape
  roads0=sf::st_transform(roads0,sf::st_crs(6393))
  nsdat[[3]]=sf::st_transform(nsdat[[3]],sf::st_crs(6393))
  nsdat[[4]]=sf::st_transform(nsdat[[4]],sf::st_crs(6393))
  
  #standardize colnames
  colnames(roads0)[5]<-"name"
  colnames(nsdat[[4]])[1]<-"name"
  
  #create unique id for northslope
  nsdat[[3]]=nsdat[[3]] %>% group_by(name) %>% mutate(countID=1:n()) %>% ungroup()
  nsdat[[3]]$uniqueid=paste(nsdat[[3]]$name,nsdat[[3]]$countID,sep="_")
  
  #create uniqueid for taps
  nsdat[[4]]$uniqueid=paste("taps",nsdat[[4]]$name,sep="_")
  
  #remove countID from both
  nsdat[[3]]=nsdat[[3]][,-which(colnames(nsdat[[3]])=="countID")]
  roads0=roads0[,-which(colnames(roads0)=="countID")]
  
  #add column to indicate dataset from
  nsdat[[3]]$dataset="north_slope"
  nsdat[[4]]$dataset="taps"
  roads0$dataset="roads"
  
  #add column to roads for type
  roads0$type="Road"
  
  #bind together
  roads_out=dplyr::bind_rows(roads0,nsdat[[3]])
  roads_out=dplyr::bind_rows(roads_out,nsdat[[4]])
  
  return(roads_out)
}

### Run initial filter on geolocation data by duration ----------
#*want to remove any caribou with <4 months of data
#*geo is geolocation df
#*datecolname is string indicating name of datetime column
#*mindur is a units object indicating the minimum time needed per individual
TrimDuration<-function(geo,datecolname,mindur){
  #geo[,which(colnames(geo)==datecolname)]
  #group by individual
  gdats=geo %>% group_by(uniqueid) %>% 
    dplyr::summarise(mindat=min(eval(parse(text=datecolname))),maxdat=max(eval(parse(text=datecolname)))) %>%
    as.data.frame()
  gdats$dur=difftime(gdats$maxdat,gdats$mindat,units="weeks")
  
  keep.ids=gdats[gdats$dur>=mindur,]$uniqueid
  geo=geo[geo$uniqueid%in%keep.ids,]
  return(geo)
  }

### Create object with road buffer polygons -------
Road_Buffer<-function(roads1,dbuff){
  
  #select types to buffer...
  roadbuffs=st_buffer(roads1,dist=dbuff)
  return(roadbuffs)
}

### Get summary of caribou intersections with buffers -------
SummarizeRoadIntersections<-function(roads,roadbuffs,geo2,test){
  
  if(test==TRUE){
    #pull IDs
    IDs=unique(geo2$uniqueid)
    #pull a few unique caribou out for testing functionality
    IDs.test=sample(IDs,3)
    geo2=geo2[geo2$uniqueid%in%IDs.test,]
    
  }
  
  #Set up road summary list
  #get nested tracks
  road_summary_list=
    amt::make_track(geo2,X,Y,.t=datetime_UTC,id=uniqueid,all_cols=TRUE,crs=sf::st_crs(6393)) |>
    tidyr::nest(data=-"uniqueid")
  
  #get column in nested df of line format of tracks
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(lines = purrr::map(data, function(x) 
      x |> amt::as_sf_lines()))
  
  #get another column with number of intersections with roads
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(ints = purrr::map(lines, function(x) 
      x |> sf::st_intersection(roads,x)))
  
  #get number of intersections
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(n_ints = purrr::map(ints, function(x) 
      x |> nrow()))
  
  #get union'd object for road buffers
  rb=sf::st_union(roadbuffs)
  
  #add column to each dataframe with id number for geolocation ID
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(data_uid = purrr::map(data,function(x)
      x |> douniqueID()
    ))
  
  #actually need overlap with points.. not lines
  #lines is quicker but need points associated with time
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(data_sf = purrr::map(data_uid,function(x)
      x |> sf::st_as_sf(coords=c("x_","y_"),crs=sf::st_crs(6393))
    ))
  
  #get another column with intersections with road buffers
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(ints_buffs = purrr::map(data_sf, function(x) 
      sf::st_intersection(x,rb)))
  
  #get number of intersections
  road_summary_list <- road_summary_list |> 
    dplyr::mutate(n_ints_buffs = purrr::map(ints_buffs, function(x) 
      x |> nrow()))
  
  #Return whole nested df with all buffer intersections, nested by caribou ID
  return(road_summary_list)

}
 
#Helper function for SummarizeRoadIntersections
douniqueID<-function(dat){
  #remove burst from uniqueid_bID
  dat$uniqueid=paste(dat$owner,dat$herd,dat$Animal_ID,sep="_")
  dat$geoloc_index=1:nrow(dat)
  dat$uniqueid_geo=paste(dat$uniqueid,dat$geoloc_index,sep="_")
  
  #remove uniqeuid
  dat$uniqueid<-NULL
  return(dat)
}

## 3. Identify treatment and control trajectories -------------

ProcessTrtCtrl<-function(rb_summary_list,
                         filter_before=28,
                         filter_after=28,
                         ctrl_filter=84,
                         herd="wah",
                         seasons,
                         t_=t_){
  
  #Get unique IDs for each road interaction
  geo_c=SetRoadIntID(rb_summary_list)
  
  #Filter out by herd
  geo_c=geo_c[geo_c$herd==herd,]
  
  #Get durations before and after each road interaction
  ints=DescribeRoadInts(geo_c)
  
  #Filter road interactions based on duration filters set
  ints_its=FilterInts(ints,filter_before,filter_after)
  
  #Find control groups using control filter
  ctrls=FindCtrlGroups(geo_c,ctrl_filter)
  
  #Assign seasons using seasons df input
  geo_cs=assign_season(geo_c,"t_",seasons,herd)
  
  #
  its_seq=FormatClusterSets(geo_cs,ints_its)
  
  #
  geo_ctrls=FormatCtrls(geo_cs,ctrls) 
  
  return(list("trt"=its_seq,
              "ctrl"=geo_ctrls))
}

### Find treatment trajectories that overlap with roads -----
SetRoadIntID<-function(rb_summary_list){

  #add column to int, int=1-- this will be used to indicate whether a point intersects with a road
  rb_summary_list <- rb_summary_list |> 
    dplyr::mutate(ints_buffs = purrr::map(ints_buffs,function(x)
     dointID(x,1)
    ))
  
  #unnest trajectory data with unique geolocation ids
  traj_all=rb_summary_list |> 
    dplyr::select(data_uid) |> 
    tidyr::unnest_legacy(cols=c(data_uid))
  
  #format ints buffs into df, without geometry
  rb_summary_list <- rb_summary_list |> 
    dplyr::mutate(ints_buffs = purrr::map(ints_buffs,function(x)
      sf::st_drop_geometry(x)
    ))
  
  #unnest ints_buffs, which contains only geolocation points that intersect with a road buffer
  traj_int=rb_summary_list |> 
    dplyr::select(ints_buffs) |> 
    tidyr::unnest_legacy(cols=c(ints_buffs))

  #select only uniqueid_geo and int status for traj_int
  traj_int=traj_int[,c("uniqueid_geo","int")]

  #join intersected geolocations with full geolocation data
  traj_out=dplyr::left_join(traj_all,traj_int)
  
  #if intersection is NA (meaning the point wasn't in the ints_buffs data), it was not an intersection, assign 0
  traj_out$int[is.na(traj_out$int)]<-0

  #Get unique ids for each group of points overlapping a road
  traj_out$lagint=lag(traj_out$int) #Get lag intersection column
  traj_out$diffint=traj_out$int-traj_out$lagint #if diffint is 0, no change
  traj_out$uniqueid=gsub("_[0-9]*$","",traj_out$uniqueid_geo) #remove geolocation id from uniqueid-- this leaves caribou indiv id

  print("setting clusterIDs")
  IDs=unique(traj_out$uniqueid) #loop through individuals
  for(k in 1:length(IDs)){
    #for each individual
    dat=traj_out[traj_out$uniqueid==IDs[k],]
    #set clusterID-- this is a uniqueID for each contiguous segment where animals are either overlapping or not overlapping a road
    dat=setclusterID(dat)
    if(k==1){
      dat_out=dat
    } else{
      dat_out=rbind(dat_out,dat)
    }
  }
  
  #clean up dat_out, keep only needed columns for downstream analysis
  dat_out=
    dat_out[,c("uniqueid",
             "uniqueid_geo",
             "clustID",
             "int",
             "x_",
             "y_",
             "t_",
             "owner",
             "herd_str",
             "herd",
             "Animal_ID",
             "Sex",
             "epsg")]
  
  return(dat_out)

}

#Helper function for SetRoadIntID
dointID<-function(dat,intval){
  dat$int=intval
  return(dat)
}

#Helper function for SetRoadIntID to get clusterID for individual
setclusterID<-function(dat){
  dat$clustID=0
  j=0
  k=-1
  for(i in 1:nrow(dat)){
    #if diffint!=0 (start of new seq) and is intersection
    #or, start of dataset and is intersection
    if((dat$diffint[i]!=0&dat$int[i]==1)|(i==1&dat$int[i]==1)){
      dat$clustID[i]=j+dat$int[i]
      j=j+1
    }
    #if diffint!=0 (start of new seq) and is not interesection
    #or, start of dataset and is not intersection
    if(dat$diffint[i]!=0&dat$int[i]==0|(i==1&dat$int[i]==0)){
      dat$clustID[i]=k
      k=k-1
    }
    if(dat$diffint[i]==0&i!=1){
      dat$clustID[i]=dat$clustID[i-1]
    }
  }   
  return(dat)
}

### Summarize road interaction durations -----
#created nested dataframe that summarizes each road interaction 
#Get summary of characteristics for each road intersection cluster
#time duration before
#time duration after
#N geolocations before
#N geolocations after
#month
#year
#herd
DescribeRoadInts<-function(geo_c){
    clusters=
      geo_c %>% 
    dplyr::summarise(mint=min(t_),
                     maxt=max(t_),
                     difft=difftime(max(t_),min(t_),units="hours"),
                     nlocs=n(),
                     .by = c(herd, uniqueid, int, clustID)) %>%
    dplyr::arrange(herd,uniqueid,mint)
  
    #for each intersection, get time durations before and after next intersection
    caribou=unique(clusters$uniqueid)
    for(c in 1:length(caribou)){
      cdf=clusters[clusters$uniqueid==caribou[c],]
      #loop through intersection clusters (positive clustIDs)
      ints=unique(cdf$clustID)[unique(cdf$clustID)>0]
      if(length(ints)!=0){ #if any intersections
      for(i in 1:length(ints)){
        cdurs_i=data.frame("dur_before"=NA,"dur_after"=NA)
        #order of assignment to before/after gets swithced if pos/neg
        if(first(cdf$clustID)<0){
        cdf_i=cdf[cdf$clustID==ints[i],]
        cdf_before=cdf[cdf$clustID==-(ints[i]),]
        cdf_after=cdf[cdf$clustID==-(ints[i]+1),]
        } 
        
        #order of assignment to before/after gets swithced
        if(first(cdf$clustID)>0){
          cdf_i=cdf[cdf$clustID==ints[i],]
          cdf_before=cdf[cdf$clustID==-(ints[i]-1),]
          cdf_after=cdf[cdf$clustID==-(ints[i]),]
        }
        
        #add to dataframe
        if(nrow(cdf_before)>0){ #allows for situation where intersection is at beginning or end of track
          cdurs_i$dur_before=as.numeric(cdf_before$difft)
        } else{
          cdurs_i$dur_before=0
        }
        if(nrow(cdf_after)>0){
          cdurs_i$dur_after=as.numeric(cdf_after$difft)
        } else{
          cdurs_i$dur_after=0
        }

        if(!exists("cdurs")){
          cdurs=cdurs_i
        } else{
          cdurs=rbind(cdurs,cdurs_i)
        }
        
      } #for i in intersections
        
        
        } #if length ints!=0
      
      } #for c in caribou

    #assign before/after durations to dataset with just cluster info
    ints=
      clusters %>% 
      dplyr::group_by(herd,uniqueid,int,clustID) %>%
      dplyr::filter(clustID>0)
    
    ints$dur_before=cdurs$dur_before
    ints$dur_after=cdurs$dur_after
    
    #pull some other descriptive info to make viz easier
    ints$month=lubridate::month(ints$mint)
    
    return(ints)    
    
} #function closing bracket

### Filter treatment trajectories with duration minimums -------
FilterInts<-function(ints,filter_before,filter_after){
  sel_ints=ints[ints$dur_before>=filter_before*24&ints$dur_after>=filter_after*24,]
  return(sel_ints)
}

### Find control trajectories of input minimum duration -------
FindCtrlGroups<-function(geo_c,ctrl_filter){
  
  #trim road interaction clusters out
  geonr=geo_c[geo_c$clustID<0,]
  
  clusters=
    geonr %>% 
    dplyr::summarise(mint=min(t_),
                     maxt=max(t_),
                     difft=difftime(max(t_),min(t_),units="hours"),
                     nlocs=n(),
                     .by = c(herd, uniqueid, clustID)) %>%
    dplyr::arrange(herd,uniqueid,mint)
  
  #make days column
  clusters$days=round(as.numeric(clusters$difft/24))
  
  #caribou clusters with at least 84 days (12 weeks) of no road interaction
  ctrls=clusters[clusters$days>=ctrl_filter,]
  return(ctrls)
}

### Make seasons df -------
MakeSeasons<-function(seasons,strt.dts,end.dts){
  seasons=data.frame("season"=seasons,"strt"=strt.dts,"end"=end.dts)
return(seasons)
}

### Assign season to dataframe by herd -----
assign_season <- function(df, datetime_col, seasons,
                          herd = "wah",
                          tz = "UTC",
                          year_ref = 2000,
                          season_col = "season") {
  
  # df: data.frame
  # datetime_col: unquoted column name containing POSIXct
  # seasons: data.frame or matrix with columns: season, start, end
  #         where start/end are "MM-DD" strings (e.g., "03-20")
  # tz: timezone used for extracting month/day from POSIXct
  # year_ref: fixed leap year to support Feb 29 (2000 is leap)
  # keep_bounds: optionally attach computed start/end doy columns

  # Extract day-of-year from POSIXct using same reference year
  # (we only need month/day; year_ref is just a vehicle)
  df=as.data.frame(df)
  dt <- df[,which(colnames(df)==datetime_col)]

  # Convert to month-day in tz, then to doy in year_ref
  doy <- lubridate::yday(dt)

  season_out <- rep(NA, length(doy))
  
  for (i in seq_len(nrow(seasons))) {
    st <- seasons$start[i]
    en <- seasons$end[i]
    hit <- doy >= st & doy <= en
    season_out[is.na(season_out) & hit] <- seasons$season[i]
  }
  
  df[[season_col]] <- season_out
  
  return(df)
}


### Output intersections with period (before, after etc.) for each set ------
FormatClusterSets<-function(geo_cs,ints_its){
  
  #for each intersection, pull the before span and after span
  ints_its$uniqueid_c=paste(ints_its$uniqueid,ints_its$clustID,sep="_")
  cIDs=unique(ints_its$uniqueid_c)
  
  for(c in 1:length(cIDs)){
    print(c)
    ID=ints_its[ints_its$uniqueid_c==cIDs[c],]$uniqueid
    cnum=ints_its[ints_its$uniqueid_c==cIDs[c],]$clustID
    cdf=geo_cs[geo_cs$uniqueid==ID,]
    
    #order of assignment to before/after gets switched if pos/neg
    if(first(cdf$clustID)<0){
      cdf_i=cdf[cdf$clustID==cnum,]
      cdf_before=cdf[cdf$clustID==-(cnum),]
      cdf_after=cdf[cdf$clustID==-(cnum+1),]
    } 
    
    #order of assignment to before/after gets switched
    if(first(cdf$clustID)>0){
      cdf_i=cdf[cdf$clustID==cnum,]
      cdf_before=cdf[cdf$clustID==-(cnum-1),]
      cdf_after=cdf[cdf$clustID==-(cnum),]
    }
    
    cdf_before$period="before"
    cdf_i$period="during"
    cdf_after$period="after"
    
    #add id for whole trajectory, incl each period
    cdf_before$trajID=c
    cdf_i$trajID=c
    cdf_after$trajID=c
    
    its_seq=rbind(cdf_before,cdf_i)
    its_seq=rbind(its_seq,cdf_after)
    
    if(c==1){
      out=its_seq
    } else{
      out=rbind(out,its_seq)
    }
    
  } #for c in cIDs
  
  return(out)    
  
} #function closing bracket

### Output control data geolocations ------
#This function is analogous to FormatClusterSets, but pulls control data
#Input:
#Output: geo_ctrls
FormatCtrls<-function(geo_cs,ctrls,herd="wah"){
  
  geo_cs=geo_cs[geo_cs$herd=="wah",]
  ctrls=ctrls[ctrls$herd=="wah",]
  ctrls$uniqueid_c=paste(ctrls$uniqueid,ctrls$clustID,sep="_")
  geo_cs$uniqueid_c=paste(geo_cs$uniqueid,geo_cs$clustID,sep="_")
  cIDs=unique(ctrls$uniqueid_c)
  
  
  for(c in 1:length(cIDs)){
    print(c)
    ID=ctrls[ctrls$uniqueid_c==cIDs[c],]$uniqueid
    cnum=ctrls[ctrls$uniqueid_c==cIDs[c],]$clustID
    cdf=geo_cs[geo_cs$uniqueid_c==cIDs[c],]
    
    cdf$period="before"
    
    #add id for whole trajectory
    cdf$trajID=paste0("ctrl",c)
    
    if(c==1){
      out=cdf
    } else{
      out=rbind(out,cdf)
    }
    
  } #for c in cIDs
  
  return(out)    
  
} #function closing bracket


## 5. Fit movement models ----
#Runs movement models, calculates mean velocity, returns tidy output
GetMovementParameters<-function(pgeo,herd="wah",minrow=10){
  geo=pgeo[pgeo$herd==herd,]
  
  check=geo %>% 
    ungroup() %>%
    dplyr::summarise(n_distinct(type),
                     .by=c(pairID,segID))
  
  sf_locs=sf::st_as_sf(geo,coords=c("x_","y_"),crs=sf::st_crs(6393))
  
  ngeo=sf_locs %>% dplyr::group_by(uniqueid,
                                   pairID,
                                   type,
                                   trajID,
                                   segID,
                                   period,
                                   season,
                                   group_start,
                                   group_end) |> tidyr::nest()
  
  ngeo=ngeo %>% mutate(nrows=map(data,nrow))
  
  ngeo <- ngeo %>% 
    dplyr::mutate(
      fixpar=list(c(NA,NA))
    )
  
  ngeo <- ngeo %>% 
    dplyr::mutate(
      theta=list(c(2,0))
    )
  
  ngeo=ngeo[ngeo$nrows>minrow,]
  
  tbl_locs_fit2=RunMovementModels(ngeo)
  movepairs2=CalculateMeanVelocity(tbl_locs_fit2)
  movepairs3=CombineModelParams_Pairs(movepairs2)
  movepairs4=TidyMovePairs(movepairs3)
  
  return(movepairs4)
}

#Helper function for GetMovementParameters
RunMovementModels<-function(ngeo){
  
  tbl_locs_fit <- ngeo %>% 
    dplyr::mutate(fit = furrr::future_pmap(list(d = data,f=fixpar,t=theta),
                                           fit_crawl,.options=furrr::furrr_options(seed=TRUE)))
  tbl_locs_fit2 <- 
    tbl_locs_fit %>% 
    dplyr::mutate(
      params=
        map(fit,dofit)
    )
  
  return(tbl_locs_fit2)
  
}

#Helper function for GetMovementParameters
fit_crawl <- function(d,f,t) {
  
  ## if relying on a prior for location quality
  ## replace this with the function described previously
  prior <- function(p) {
    dnorm(p[2], -4, 2, log = TRUE)
  } 
  
  fit <- crawl::crwMLE(
    mov.model =  ~ 1,
    if (any(colnames(d) == "activity")) {
      activity <- ~ I(activity)
    } else {activity <- NULL},
    fixPar = f,
    data = d,
    theta=t,
    method = "Nelder-Mead",
    Time.name = "t_",
    prior = prior,
    attempts = 8,
    control = list(
      trace = 0
    ),
    initialSANN = list(
      maxit = 1500,
      trace = 0
    )
  )
  fit
}

#Helper function for GetMovementParameters
init_params <- function(d) {
  if (any(colnames(d) == "x") && any(colnames(d) == "y")) {
    ret <- list(a = c(d$x_[1], 0,
                      d$y_[1], 0),
                P = diag(c(10 ^ 2, 10 ^ 2,
                           10 ^ 2, 10 ^ 2)))
  } else if (inherits(d,"sf")) {
    ret <- list(a = c(sf::st_coordinates(d)[[1,1]], 0,
                      sf::st_coordinates(d)[[1,2]], 0))
  }
  ret
} 

#Helper function for GetMovementParameters
dofit<-function(fit){
  if(all(class(fit)=="crwFit")){
    crawl::tidy_crwFit(fit)
  }
}

#Helper function for GetMovementParameters
CalculateMeanVelocity<-function(tbl_locs_fit){
  tbl_locs_fit %>%
    mutate(
      dist_x = furrr::future_pmap(list(data=data),getdist_x),
      dist_y = furrr::future_pmap(list(data=data),getdist_y),
      difft = furrr::future_pmap(list(data=data),getdifft)
    ) #m/hr
}

#Helper function for GetMovementParameters
getdist_x<-function(data){
  mean(as.numeric(st_coordinates(data)[,1]-lag(st_coordinates(data)[,1])),na.rm=T)
}

#Helper function for GetMovementParameters
getdist_y<-function(data){
  mean(as.numeric(st_coordinates(data)[,2]-lag(st_coordinates(data)[,2])),na.rm=T)
}

#Helper function for GetMovementParameters
getdifft<-function(data){
  mean(as.numeric(difftime(data$t_,lag(data$t_),units="hours")),na.rm=T)
}

#Helper function for GetMovementParameters
CombineModelParams_Pairs<-function(tbl_locs_fit2){
  tbl_locs_fit3=tbl_locs_fit2 |> tidyr::unnest(cols=c(uniqueid,season,period,trajID,segID,type,pairID,params,dist_x,dist_y,difft))
  tbl_locs_fit3$vx=tbl_locs_fit3$dist_x/tbl_locs_fit3$difft
  tbl_locs_fit3$vy=tbl_locs_fit3$dist_y/tbl_locs_fit3$difft
  tbl_locs_fit3=
    tbl_locs_fit3[,c("uniqueid",
                     "season",
                     "period",
                     "pairID",
                     "trajID",
                     "type",
                     "segID",
                     "group_start",
                     "group_end",
                     "term",
                     "estimate",
                     "std.error",
                     "conf.low",
                     "conf.high",
                     "vx",
                     "vy"
    )]
  
  tbl_locs_fit3=tbl_locs_fit3[(tbl_locs_fit3$term!="AIC"),]
  tbl_locs_fit3=tbl_locs_fit3[(tbl_locs_fit3$term!="logLik"),]
  
  return(tbl_locs_fit3)
  
}

TidyMovePairs<-function(movepairs){
  movepairs=movepairs[complete.cases(movepairs),]
  
  movepairs_w=movepairs %>% 
    tidyr::pivot_wider(
      names_from=term,
      names_sep="_",
      values_from=c(estimate,std.error,conf.low,conf.high),
      id_cols=c(pairID,uniqueid,season,period,type,trajID,segID,vx,vy))
  
  #reorder cols, position term variables in adjacent cols
  movepairs_w=movepairs_w[,c(1:10,12,14,16,11,13,15,17)]
  
  return(movepairs_w)
}

## 6. Make plots ---------
BACI_intxn_plot<-function(movepairs,response="`estimate_ln beta (Intercept)`"){
  movepairs$season<-as.factor(movepairs$season)
  movepairs$season<-forcats::fct_relevel(movepairs$season,
                                         c("springmigr",
                                           "calving",
                                           "insect",
                                           "latesummer",
                                           "fallmigr",
                                           "winter"))
  
  movepairs$period<-as.factor(movepairs$period)
  movepairs$period<-forcats::fct_relevel(movepairs$period,
                                         c("before",
                                           "during",
                                           "after"))
  
  mp=movepairs %>% 
    dplyr::group_by(type,period,season) %>%
    dplyr::summarise(mean_est=mean(eval(parse(text=response))))
  
  p1=mp %>%
    ggplot() +
    geom_line(mapping=aes(x=period,y=mean_est,group=type))+
    geom_point(mapping=aes(x=period,y=mean_est,color=type))+
    facet_wrap(~season)+
    theme_minimal()+
    ylab(response)
  
  return(p1)
  
}

ForestPlot_MovePars<-function(movepairs,response="sigma"){
  
  if(response=="sigma"){
    p1=movepairs %>%
      ggplot() +
      geom_segment(mapping=
                  aes(x=`conf.low_ln sigma (Intercept)`,
                      xend=`conf.high_ln sigma (Intercept)`,
                      y=segID,
                      yend=segID))+
      geom_point(mapping=
                     aes(x=`estimate_ln sigma (Intercept)`,
                         y=segID),size=0.5)+
      geom_vline(xintercept=0,linetype="dashed",color="red")+
      theme_minimal()+
      xlab(response)+
      theme(axis.text.y=element_blank())
  }
  
  if(response=="beta"){
    p1=movepairs %>%
      ggplot() +
      geom_segment(mapping=
                     aes(x=`conf.low_ln beta (Intercept)`,
                         xend=`conf.high_ln beta (Intercept)`,
                         y=segID,
                         yend=segID))+
      geom_point(mapping=
                   aes(x=`estimate_ln beta (Intercept)`,
                       y=segID),size=0.5)+
      geom_vline(xintercept=0,linetype="dashed",color="red")+
      theme_minimal()+
      xlab(response)+
      theme(axis.text.y=element_blank())
    
    
  }

  return(p1)
}
  
Check_Dupl_Trt<-function(movepairs,pgeo){
  #subset to treatments
  trt=movepairs[movepairs$type=="trt",]
  
  #get year from pgeo, join to movepairs
  pgeo$year=lubridate::year(pgeo$group_start)
  pg=unique(pgeo[,c(which(colnames(pgeo)=="segID"),
                    which(colnames(pgeo)=="year"))])
  trt1=left_join(trt,pg,by="segID")
  
  #Do any caribou interact with a road within the same season
  #resulting in two treatments from the same animal, year, and season
  #if so, there would be n>1 for the below result
  summ=trt1 %>% filter(period=="during") %>%
    dplyr::group_by(uniqueid,year,season) %>% 
    dplyr::summarise(n=n())
  
  return(any(summ$n>1))
  
}

GetPairSummaries<-function(pgeo1){
  
  ####Period alone
  #get min dists for each pairID
  pairs=unique(pgeo1$pairID)
  
  #during=pgeo1[pgeo1$period=="during",]
  pgeo1=pgeo1 %>% st_as_sf(coords=c("x_","y_"),crs=st_crs(6393))
  for(p in 1:length(pairs)){
    pgeo1_p=pgeo1[pgeo1$pairID==pairs[p],]
    trt=pgeo1_p[pgeo1_p$type=="trt",]
    ctrl=pgeo1_p[pgeo1_p$type=="ctrl",]
    
    summary_p=
      pgeo1_p %>% dplyr::group_by(period) %>%
      dplyr::summarise(mint=min(t_),
                       maxt=max(t_)) %>%
      st_drop_geometry()
    
    summary_p$mindist=NA
    summary_p[summary_p$period=="during",]$mindist=
      min(st_distance(trt[trt$period=="during",],ctrl[ctrl$period=="during",]))
    summary_p[summary_p$period=="after",]$mindist=
      min(st_distance(trt[trt$period=="after",],ctrl[ctrl$period=="after",]))
    summary_p[summary_p$period=="before",]$mindist=
      min(st_distance(trt[trt$period=="before",],ctrl[ctrl$period=="before",]))
    
    summary_p$pairID=pairs[p]
    
    if(p==1){
      summary=summary_p
    } else{
      summary=rbind(summary,summary_p)
    }
  }
    summary_period=summary
    
    ####Period and season
    #get min dists for each pairID
    pairs=unique(pgeo1$pairID)
    
    #during=pgeo1[pgeo1$period=="during",]
    pgeo1=pgeo1 %>% st_as_sf(coords=c("x_","y_"),crs=st_crs(6393))
    for(p in 1:length(pairs)){
      pgeo1_p=pgeo1[pgeo1$pairID==pairs[p],]
      trt=pgeo1_p[pgeo1_p$type=="trt",]
      ctrl=pgeo1_p[pgeo1_p$type=="ctrl",]
      
      summary_p=
        pgeo1_p %>%
        ungroup() %>%
        dplyr::summarise(mint=min(t_),
                         maxt=max(t_),
                         .by=c(period,season),
                         across(geometry, st_union)) %>%
        st_drop_geometry()
      
      summary_p$mindist=NA
      summary_p[summary_p$period=="during",]$mindist=
        min(st_distance(trt[trt$period=="during",],ctrl[ctrl$period=="during",]))
      summary_p[summary_p$period=="after",]$mindist=
        min(st_distance(trt[trt$period=="after",],ctrl[ctrl$period=="after",]))
      summary_p[summary_p$period=="before",]$mindist=
        min(st_distance(trt[trt$period=="before",],ctrl[ctrl$period=="before",]))
      
      summary_p$pairID=pairs[p]
      
      if(p==1){
        summary=summary_p
      } else{
        summary=rbind(summary,summary_p)
      }
    
  }
  #rearrange cols
  summary=summary[,c(6,1:4)]
  return(list("period"=summary_period,"period_season"=summary))
}

VizualizePairSummaries<-function(pair_summaries_list){
  pair_summaries=pair_summaries_list$period
  pair_seasons=pair_summaries_list$period_season
  
  pair_summaries$mindist<-as.numeric(pair_summaries$mindist)
 
  pair_summaries=pair_summaries %>% dplyr::group_by(pairID) %>%
    dplyr::mutate(np=n_distinct(period))
  
  p1=pair_summaries %>%
    filter(period=="during") %>%
    ggplot()+
    geom_histogram(mapping=aes(x=mindist))+
    theme(axis.text.y=element_blank())
  
  pair_summaries$duration=as.numeric(difftime(pair_summaries$maxt,pair_summaries$mint,units="days"))
  
  #breakdown of durations
  p2=pair_summaries %>%
    ggplot()+
    geom_point(mapping=aes(x=duration,y=pairID,color=period))+
    theme(axis.text.y=element_blank())
  
  #next: want to see how much data per season
    #-season x period per pair
    #heat map
  ps=pair_seasons %>% dplyr::group_by(period,season) %>%
    dplyr::summarise(num=n())
  p3=ps %>% ggplot()+geom_tile(mapping=aes(x=period,y=season,fill=num))
  
  return(list("mindists"=p1,"durations"=p2,"sample_sizes"=p3))
}

TrimPairDistances<-function(pgeo,pair_summaries,mindist){
  during=pair_summaries[pair_summaries$period=="during",]
  during$mindist=as.numeric(during$mindist)
  keep_pairs=during[during$mindist<=90000,]$pairID
  pgeo_f=pgeo[pgeo$pairID%in%keep_pairs,]
  return(pgeo_f)
}
