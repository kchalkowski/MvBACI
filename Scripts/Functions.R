
## 1. Read input data ----- 
### Read RDS objects------
ReadRDS<-function(path,filename){
  readRDS(file.path(path,filename))
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
  #tar_target(geo_c,SetRoadIntID(rb_summary_list))
  geo_c=SetRoadIntID(rb_summary_list)
  #tar_target(ints,DescribeRoadInts(geo_c)),
  ints=DescribeRoadInts(geo_c)
  #tar_target(ints_its,FilterInts(ints,filter_before=28,filter_after=28)),
  ints_its=FilterInts(ints,filter_before,filter_after)
  #tar_target(ctrls,FindCtrlGroups(geo_c,84)),
  ctrls=FindCtrlGroups(geo_c,ctrl_filter)
  #tar_target(geo_cs,assign_season(geo_c,t_,seasons,herd="wah")),
  geo_cs=assign_season(geo_c,"t_",seasons,herd) #character string is not in a standard unambiguous format
  #tar_target(its_seq,FormatClusterSets(geo_cs,ints_its)),
  its_seq=FormatClusterSets(geo_cs,ints_its)
  #tar_target(geo_ctrls,FormatCtrls(geo_cs,ctrls)),
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
    dplyr::group_by(herd,uniqueid,int,clustID) %>%
    dplyr::summarise(mint=min(t_),
                     maxt=max(t_),
                     difft=difftime(max(t_),min(t_),units="hours"),
                     nlocs=n()) %>%
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

        if(i==1&c==1){
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
    dplyr::group_by(herd,uniqueid,clustID) %>%
    dplyr::summarise(mint=min(t_),
                     maxt=max(t_),
                     difft=difftime(max(t_),min(t_),units="hours"),
                     nlocs=n()) %>%
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
    hit <- if (st <= en) {
      doy >= st & doy <= en
    } else {
      # wraps across Dec 31 -> Jan 1
      doy >= st | doy <= en
    }
    season_out[is.na(season_out) & hit] <- seasons$season[i]
  }
  
  df[[season_col]] <- season_out
  
  df[df$herd!=herd,]$season<-NA

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

## 4. Match control and treatment trajectories ------

### Determine overlap times between each treatment and control trajectory ----
Match_Ctrl_Trt<-function(its_seq,geo_ctrls){
  durations=FindOverlapDurations(its_seq,geo_ctrls)
  pairs=Hungarian_matching(durations)
  pgeo=Filter_Pairs(its_seq,geo_ctrls,pairs)
  
  return(pgeo)
}

#Helper function for Match_Ctrl_Trt
FindOverlapDurations<-function(its_seq,geo_ctrls){
  
  trt=its_seq %>% dplyr::group_by(trajID,period) %>% dplyr::mutate(min.seg.t=min(t_),max.seg.t=max(t_))
  trt=trt %>% dplyr::group_by(trajID) %>% dplyr::mutate(min.traj.t=min(t_),max.traj.t=max(t_))
  ctrl=geo_ctrls %>% dplyr::group_by(trajID) %>% dplyr::mutate(min.traj.t=min(t_),max.traj.t=max(t_))
  
  trt=unique(trt[,c(which(colnames(trt)=="trajID"),
                    which(colnames(trt)=="period"),
                    which(colnames(trt)=="min.seg.t"),
                    which(colnames(trt)=="max.seg.t"),
                    which(colnames(trt)=="min.traj.t"),
                    which(colnames(trt)=="max.traj.t")
  )]
  )
  
  ctrl=unique(ctrl[,c(which(colnames(ctrl)=="trajID"),
                      which(colnames(ctrl)=="period"),
                      which(colnames(ctrl)=="min.traj.t"),
                      which(colnames(ctrl)=="max.traj.t")
  )]
  )
  
  result <- ctrl %>%
    dplyr::rename(ctrl_id = trajID, ctrl_start = min.traj.t, ctrl_end = max.traj.t) %>%
    dplyr::cross_join(trt %>% rename(trt_id = trajID, trt_period = period, trt_start = min.seg.t, trt_end = max.seg.t)) %>%
    dplyr::mutate(overlap_days = overlap_duration(ctrl_start, ctrl_end, trt_start, trt_end)) %>%
    dplyr::select(ctrl_id, trt_id, trt_period, ctrl_start, ctrl_end, trt_start, trt_end, overlap_days)
  
  res.total=result %>% group_by(ctrl_id,trt_id) %>% dplyr::summarise(overlaps=sum(overlap_days),po=sum(overlap_days==0))
  res.filt=res.total[res.total$overlaps>0&res.total$po==0,]
  
}

#Helper function for Match_Ctrl_Trt
overlap_duration <- function(s1, e1, s2, e2) {
  overlap_start <- pmax(s1, s2)
  overlap_end   <- pmin(e1, e2)
  duration      <- pmax(0, difftime(overlap_end,overlap_start,units="days"))
  return(duration)
}

#Helper function for Match_Ctrl_Trt
Hungarian_matching<-function(matches){
  
  matches$ctrl_id<-as.character(matches$ctrl_id)
  matches$trt_id<-as.character(matches$trt_id)
  matches$overlaps<-as.numeric(matches$overlaps)
  
  #need remove row/colnames
  Wmat=data.table::dcast(as.data.table(matches,keep.rownames=FALSE),ctrl_id~trt_id,value.var="overlaps")
  Wmat=as.matrix(Wmat)
  rownames(Wmat)=Wmat[,1]
  Wmat=Wmat[,2:ncol(Wmat)]
  storage.mode(Wmat)="numeric"
  Wmat[is.na(Wmat)]<-0
  Wmat=Wmat*-1 #flip the sign, since algo below minimizes cost instead of maximizes weight
  
  #Wmat rows are ctrl
  #Wmat cols are treatment
  Wmat_optim=HungarianSolver(Wmat)
  
  #1st col is treatment
  #second col is contrl
  pairs=Wmat_optim$pairs
  matches<-as.data.frame(matches)
  #need convert pairs (which has row/col numbers) to IDs
  pairs=as.data.frame(pairs)
  colnames(pairs)=c("ctrl","trt")
  
  pairs$ctrl=rownames(Wmat)
  pairs=pairs[pairs$trt>0,]
  pairs$trt=colnames(Wmat)[pairs$trt]
  pairs$pairID=paste(pairs$ctrl,pairs$trt,sep="_")
  matches$pairID=paste(matches$ctrl,matches$trt,sep="_")
  pairs=left_join(pairs,matches,by="pairID")
  
  return(pairs)
}

#Helper function for Match_Ctrl_Trt
#combine
#and get new cutoff dates with just overlap
#filter geolocation data
#tar_target(tbl_locs,Filter_Pairs(its_seq2,geo_ctrls2,matches)),
Filter_Pairs<-function(its_seq,geo_ctrls,pairs){
  its_seq$type="trt"
  geo_ctrls$type="ctrl"
  geo_ctrls$period="before"
  its_seq$trajID=as.character(its_seq$trajID)
  
  dat=dplyr::bind_rows(geo_ctrls,its_seq)
  dat_summary=dat %>% dplyr::group_by(trajID) %>% dplyr::summarise(mint=min(t_),maxt=max(t_))
  
  pairs=pairs[complete.cases(pairs),]
  colnames(pairs)[2]<-"trajID.trt"
  colnames(dat_summary)[1]<-"trajID.trt"
  pairs2=left_join(pairs,dat_summary,by="trajID.trt")
  pairs2$trajID.trt=as.character(pairs2$trajID.trt)
  
  colnames(pairs2)[1]<-"trajID.ctrl"
  colnames(dat_summary)[1]<-"trajID.ctrl"
  pairs2$trajID.ctrl=as.character(pairs2$trajID.ctrl)
  pairs3=left_join(pairs2,dat_summary,by="trajID.ctrl",suffix=c(".trt",".ctrl"))
  
  pairs3$overlap.start=apply(pairs3[,c(which(colnames(pairs3)=="mint.trt"),
                                       which(colnames(pairs3)=="mint.ctrl"))],1,max)
  pairs3$overlap.end=apply(pairs3[,c(which(colnames(pairs3)=="maxt.trt"),
                                     which(colnames(pairs3)=="maxt.ctrl"))],1,min)
  
  pairs3$overlap.dur=difftime(pairs3$overlap.end,pairs3$overlap.start)
  
  joinID=unique(pairs3[,c(which(colnames(pairs3)=="trajID.trt"),
                          which(colnames(pairs3)=="pairID"),
                          which(colnames(pairs3)=="overlap.start"),
                          which(colnames(pairs3)=="overlap.end")
  )])
  colnames(joinID)[1]="trajID"
  dat2=left_join(its_seq,joinID,by="trajID")
  dat2=dat2[complete.cases(dat2),]
  
  joinID=unique(pairs3[,c(which(colnames(pairs3)=="trajID.ctrl"),
                          which(colnames(pairs3)=="pairID"),
                          which(colnames(pairs3)=="overlap.start"),
                          which(colnames(pairs3)=="overlap.end")
  )])
  colnames(joinID)[1]="trajID"
  dat2c=left_join(geo_ctrls,joinID,by="trajID")
  dat2c=dat2c[complete.cases(dat2c),]
  
  dat3=dplyr::bind_rows(dat2,dat2c)
  
  dat4=dat3[dat3$t_>=dat3$overlap.start&dat3$t_<=dat3$overlap.end,]
  
  summarize_np=dat4 %>% dplyr::group_by(pairID) %>% dplyr::summarise(n_distinct(period))
  summarize_np=summarize_np[summarize_np$`n_distinct(period)`==3,]
  
  dat5=dat4[dat4$pairID%in%summarize_np$pairID,]
  
  trt=dat5[dat5$type=="trt",]
  ctrl=dat5[dat5$type=="ctrl",]
  
  trt <- trt %>%
    group_by(pairID, period) %>%  # add whatever grouping vars you have
    mutate(
      group_start = min(t_),
      group_end   = max(t_)
    ) %>%
    ungroup() %>%
    # Assign a unique group ID per distinct group
    mutate(group_uid = consecutive_id(pairID, period))
  
  ctrl <- ctrl %>%
    left_join(
      trt %>%
        distinct(pairID, group_uid, group_start, group_end),
      by = "pairID",
      relationship = "many-to-many") %>%
    filter(t_ >= group_start & t_ <= group_end)
  
  dat6=dplyr::bind_rows(trt,ctrl)
  
  
  #grouping by group_uid and asking distinct n of type should get 2
  #ones that not represented in both involve small num pts that don't overlap well with control
  check_period_overlaps=dat6 %>% dplyr::group_by(pairID,group_uid) %>% dplyr::summarise(nt=n_distinct(type)) 
  check_period_overlaps=check_period_overlaps[check_period_overlaps$nt!=2,]
  drop_pairIDs=unique(check_period_overlaps$pairID)
  
  dat7=dat6[!(dat6$pairID%in%drop_pairIDs),]
  
  #each pairID has matching before, during, after by group_uid
  #now get the period to match as well
  
  dat7n=dat7 %>% dplyr::group_by(pairID,group_uid) %>% tidyr::nest()
  
  #assign same period to controls, matching treatment by group_uid
  dat7n2=dat7n %>% mutate(data=purrr::map(data,changeperiod)) %>% tidyr::unnest(cols=c(data))
  
  #Now need redo splits for all, but by pairID,period,season
  dat7n2$segID=paste(dat7n2$pairID,dat7n2$period,dat7n2$season,sep="_")
  
  #each segId should have two types (ctrl, treatment)
  check2=dat7n2 %>% dplyr::group_by(pairID,period,season,segID) %>% dplyr::summarize(nt=n_distinct(type))
  check2=unique(check2[,c(which(colnames(check2)=="pairID"),
                          which(colnames(check2)=="nt"))])
  check2=check2[check2$nt<2,]
  drop_pairIDs=check2$pairID
  
  #remove pairIDs with only control or treatment for respective season
  dat_filtered=dat7n2[!(dat7n2$pairID%in%drop_pairIDs),]
  #check=dat_filtered %>% dplyr::group_by(segID) %>% dplyr::summarise(nt=n_distinct(type))
  
  return(dat_filtered)
  
}

#Helper function for Filter_Pairs
changeperiod=function(df){
  df[df$type=="ctrl",which(colnames(df)=="period")]=
    df[df$type=="trt",which(colnames(df)=="period")][[1]][1]
  return(df)
}

## 5. Fit movement models ----
#Runs movement models, calculates mean velocity, returns tidy output
GetMovementParameters<-function(pgeo,herd="wah",minrow=10){
  geo=pgeo[pgeo$herd==herd,]
  
  check=geo %>% dplyr::group_by(pairID,segID) %>% dplyr::summarise(n_distinct(type))
  
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
  
