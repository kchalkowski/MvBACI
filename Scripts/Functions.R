

## Read data ----- 
ReadRDS<-function(path,filename){
  readRDS(file.path(path,filename))
}

#Read and format north slope spatial data
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




Plot_Diffs_by_Season<-function(response,velocity=FALSE,v=NULL){
  if(velocity){
    if(!missing(v)){
      if(v=="vx"){
        datwb=dat %>% dplyr::filter(term=="ln sigma (Intercept)") %>%
          tidyr::pivot_wider(
            names_from=period,
            values_from=vx,
            id_cols=c(uniqueid,season,segIDs))
        datwb$ba=datwb$after-datwb$before
      }
      if(v=="vy"){
        datwb=dat %>% dplyr::filter(term=="ln sigma (Intercept)") %>%
          tidyr::pivot_wider(
            names_from=period,
            values_from=vy,
            id_cols=c(uniqueid,season,segIDs))
        datwb$ba=datwb$after-datwb$before
      }
    }
  } else{
    datwb=dat %>% dplyr::filter(term==response) %>%
      tidyr::pivot_wider(
        names_from=period,
        values_from=estimate,
        id_cols=c(uniqueid,season,segIDs))
    datwb$ba=datwb$after-datwb$before
  }
  
  
  datwb %>% 
    ggplot() +
    geom_point(
      aes(x=ba, y=segIDs), 
      size = 4
    ) + 
    facet_wrap(~season) +
    geom_vline(xintercept=0)
  
}

## Check and Trim by duration -----
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

FormatGeolocs<-function(geo,type){
  if(type=="amt"){
    geo_out=amt::mk_track(geo,.x=X,.y=Y,.t=datetime_UTC,all_cols=TRUE,crs=sf::st_crs(6393))
  }
  if(type=="sf"){
    geo_out=sf::st_as_sf(geo,coords=c("X","Y"),crs=sf::st_crs(6393))
  }
  return(geo_out)
}

ResampleGeolocs<-function(geotk,hr_rate,hr_tol){
  tol=round(hr_tol*60) #needs to be whole number
  
  trk1 <- geotk |> nest(data = -"uniqueid")
  
  trk2 <- trk1 |> 
    mutate(steps = map(data, function(x) 
      x |> track_resample(rate = hours(8), tolerance = minutes(tol))
    ))
     
  trk3=trk2 |> dplyr::select(uniqueid, steps) |> unnest(cols = steps) 
  
  trk_out=as.data.frame(trk3)    
  
  return(trk_out)
  
}

Road_Buffer<-function(roads1,dbuff){
  
  #select types to buffer...
  roadbuffs=st_buffer(roads1,dist=dbuff)
  return(roadbuffs)
}

SummarizeRoadIntersections<-function(roads,roadbuffs,geo2,test){
  
  if(test==TRUE){
    #pull IDs
    IDs=unique(geo2$uniqueid)
    #pull a few unique caribou out for testing functionality
    IDs.test=sample(IDs,3)
    geo2=geo2[geo2$uniqueid%in%IDs.test,]
    
  }
  
  #Set up needed formats
  #pull amt format
  #geotk=amt::mk_track(geo,.x=X,.y=Y,.t=datetime_UTC,all_cols=TRUE,crs=sf::st_crs(6393))
  
  #pull sf format
  #geo_sf=sf::st_as_sf(geo2,coords=c("X","Y"),crs=sf::st_crs(6393))  
  
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


Caribou_MinCross<-function(rb_summary_list,geo_r,min_crossings){
  n_ints=rb_summary_list |> dplyr::select(uniqueid, n_ints_buffs)
  n_ints=unlist(n_ints$n_ints_buffs)
  int_summaries=data.frame("uniqueid"=rb_summary_list$uniqueid,"n_ints"=n_ints)
  keep.id=int_summaries$uniqueid[int_summaries$n_ints>=min_crossings]
  return(keep.id)
}

Road_MinCross<-function(rb_summary_list,roads1,keep_caribou_mincross,min_indiv){
  rb_summary_list2=rb_summary_list[rb_summary_list$uniqueid%in%keep_caribou_mincross,]
  rblist=rb_summary_list2 |> dplyr::select(uniqueid, ints_buffs) |> unnest(cols=ints_buffs)

  rb2=as.data.frame(rblist)

  rb3=rb2 %>% 
    group_by(Name) %>% 
    dplyr::summarise(num_uniq_caribou=n_distinct(uniqueid)) %>%
    as.data.frame()

  keep_roads=rb3$Route_Name[rb3$num_uniq_caribou>=min_indiv]
  
  roads_out=roads1[roads1$Route_Name%in%keep_roads,]

  
  return(roads_out)
}

#rt=dplyr::left_join(roads1,rb3,by="Name")
#rt=rt[!is.na(rt$num_uniq_caribou),]

#mapview(rt,zcol="num_uniq_caribou")
#mapview(rt,zcol="num_uniq_caribou")

Trim_Caribou_Geolocs_Mincross<-function(geo_r,keep_caribou_mincross){
  return(geo_r[geo_r$uniqueid%in%keep_caribou_mincross,])
}

Format_BABA<-function(obj,type){
  if(type=="geo"){
    out=sf::st_as_sf(obj,coords=c("x_","y_"),crs=sf::st_crs(6393))
    colnames(out)[which(colnames(out)=="uniqueid")]<-"Animal.ID"
    colnames(out)[which(colnames(out)=="t_")]<-"date"
  }
  
  if(type=="barrier"){
    #out=obj[,c(16)]
    #colnames(out)[1]="Name"
    
    #Need to create unique ids for each row of road
    obj=obj %>% group_by(Route_ID) %>% mutate(countID=1:n())
    obj$Name=paste(obj$Route_ID,obj$countID,sep="_")
    out=obj[,c(21,17,19)]
  }
  
  return(out)
}
  
#library(mapview)
#mapview(animal,zcol="Animal.ID")+
#  mapview(barrier)
#bar=barrier[barrier$Name=="Deering Road",]

Run_BABA<-function(geo,barrier){
  bar=barrier[barrier$Name=="Deering Road",]
  animal=geo[geo$Animal.ID=="NPS_wah_1514",]
  out.path=file.path("Output","BABA_Out")
  d=rep(20000,nrow(bar))
  
  animal$x=st_coordinates(animal)[,1]
  animal$y=st_coordinates(animal)[,2]
  
  baba1=
    BaBA_caribou2(animal = animal, barrier = bar,
               d = d,
               interval = 10, tolerance = 0, units = 'hours',
               round_fixes = TRUE, crs = 'EPSG:6393',
               export_images = TRUE,
               img_path = out.path,
               img_prefix = 'baba', img_suffix = Sys.Date(),
               img_background = NULL)
  

}
 
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

#Pull trajectories with user input cutoffs 
#caribou~road interaction is an instance where a caribou trajectory overlaps with a road buffer
#for each caribou~road interaction...
  #assign unique id to each interaction
  #determine characteristics of each interaction
    #time duration of caribou before interaction with another road
    #time duration of caribou after interaction with another road
    #import:
      #caribou unique id and all geolocation characteristics
      #road ID
#helper function, get unique id for each geolocation within nested data frame
#to use with mutate
douniqueID<-function(dat){
  #remove burst from uniqueid_bID
  dat$uniqueid=paste(dat$owner,dat$herd,dat$Animal_ID,sep="_")
  dat$geoloc_index=1:nrow(dat)
  dat$uniqueid_geo=paste(dat$uniqueid,dat$geoloc_index,sep="_")
  
  #remove uniqeuid
  dat$uniqueid<-NULL
  return(dat)
}

dointID<-function(dat,intval){
  dat$int=intval
  return(dat)
}

#Helper function to get clusterID for individual
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

#output geolocations that overlap with roads, and unique IDs for each interaction instance
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

#created nested dataframe that is organized by unique clusters as rows 
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

#helper function to split df by year
split_nondec_df <- function(df, cols){
  cols <- if(is.character(cols)) match(cols, names(df)) else cols
  do.call(rbind, lapply(seq_len(nrow(df)), function(i){
    x <- unlist(df[i, cols], use.names = FALSE)
    g <- cumsum(c(0, diff(x) < 0))
    do.call(rbind, lapply(split(seq_along(cols), g), function(k){
      out <- df[i, , drop = FALSE]
      out[cols] <- NA
      out[cols[k]] <- df[i, cols[k], drop = FALSE]
      out
    }))
  }))
}

#helper function to split rows when overlaps year
fill_boundary_na <- function(df, cols, left_value = 1, right_value = 365){
  cols <- if(is.character(cols)) match(cols, names(df)) else cols
  M <- as.matrix(df[, cols])
  
  for(i in seq_len(nrow(M))){
    na <- is.na(M[i, ])
    if(!any(na)) next
    
    num_idx <- which(!na)
    if(!length(num_idx)) next
    
    L <- min(num_idx); R <- max(num_idx)
    
    # Fill the NA adjacent to the numeric block (left side if it exists, else right side)
    if(L > 1 && is.na(M[i, L - 1])) {
      df[i, cols[L - 1]] <- left_value
    } else if(R < ncol(M) && is.na(M[i, R + 1])) {
      df[i, cols[R + 1]] <- right_value
    }
  }
  df
}

#helper function to add background shading for season to plot
add_season_shading <- function(
    p, seasons,
    colors = c("grey85", "grey95"),
    alpha = 0.2,
    label_y = Inf,
    label_vjust = 1.2,
    label_size = 3.5,
    show_legend = FALSE
){
  stopifnot(all(c("season","start","end") %in% names(seasons)))
  
  seasons <- seasons[order(seasons$start), , drop = FALSE]
  seasons$shade <- factor((seq_len(nrow(seasons)) - 1) %% length(colors) + 1)
  seasons$xmid  <- seasons$start + (seasons$end - seasons$start) / 2
  
  p +
    ggplot2::geom_rect(
      data = seasons,
      ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = shade),
      inherit.aes = FALSE,
      alpha = alpha
    ) +
    ggplot2::geom_text(
      data = seasons,
      ggplot2::aes(x = xmid, y = label_y, label = season),
      inherit.aes = FALSE,
      vjust = label_vjust,
      size = label_size
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(colors, levels(seasons$shade)),
      guide = if (show_legend) "legend" else "none"
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

#seasons data frame format input:
#seasons <- data.frame(
#  season = c("Winter","Spring","Summer","Fall"),
#  start  = c(1,30,90,275),
#  end    = c(30,90,275,365)
#)
#Visualize intersections
VizDescribeInts<-function(ints,
                          seasons,
                          alpha=0.3,
                          colors= c("grey85", "grey95"),
                          label_vjust=0.1,
                          label_size=2,
                          herd,
                          filter=28){

  #find clusters where duration before, during, after are all 30 days
  ints28=ints[ints$dur_before>=filter*24&ints$dur_after>=filter*24,]
  #sel_ints=ints[ints$dur_before>=filter*24&ints$dur_after>=filter*24,]
  
  ints28$difft=round(ints28$difft/24)
  ints28$dur_before=round(ints28$dur_before/24)
  ints28$dur_after=round(ints28$dur_after/24)
  
  ints28$year=lubridate::year(ints28$mint)
  ints28$caribou_year=paste(ints28$uniqueid,ints28$year,sep=" ")
  ints28$yday=lubridate::yday(ints28$mint)
  
  ints28$b1=ints28$mint-lubridate::days(ints28$dur_before)
  ints28$b2=ints28$mint
  ints28$d1=ints28$mint
  ints28$d2=ints28$maxt
  ints28$a1=ints28$maxt
  ints28$a2=ints28$maxt+lubridate::days(ints28$dur_after)
  
  plot_cols=apply(ints28[,c(15:20)],2,lubridate::yday)
  df=data.frame("caribou_year"=ints28$caribou_year,plot_cols,"herd"=ints28$herd)
  df=split_nondec_df(df,cols=2:7)    
  df=fill_boundary_na(df,2:7,1,365)
  df$caribou_row=paste(df$caribou_year,1:nrow(df),sep=" ")
  df$caribou_ID=gsub("\\s[0-9]*$","",df$caribou_year)
  #line segment plot
    #each row is caribou uniqueID * year
    #x-axis is yday 1-364
    #segment 1: mint-dur_before to mint
    #segment 2: mint to maxt
    #segment 3: maxt to maxt+dur_after
  
  #group plot by herd (sep plots)
  p1=
    df[df$herd==herd,] %>%
    ggplot() +
    geom_linerange(aes(xmin = b1, 
                     xmax = b2,
                     y=caribou_ID),
                 color="#fe4a49",
                 linewidth=1,
                 position=position_dodge2(1)) +
    geom_linerange(aes(xmin = d1, 
                     xmax = d2,
                     y=caribou_ID),
                 color="#2ab7ca",
                 linewidth=1,
                 position=position_dodge2(1))+
    geom_linerange(aes(xmin = a1, 
                     xmax = a2,
                     y=caribou_ID),
                 color="#e3b505",
                 linewidth=1,
                 position=position_dodge2(1))+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title=element_text(size=8),
          panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = unit(c(1,1,1,1), "cm"))+
    ylab("unique caribou-road interactions (20km buffer)")
    
  p_shaded = add_season_shading(p1, 
                                  seasons, 
                                  colors = colors, 
                                  alpha = alpha,
                                  label_vjust = label_vjust,
                                  label_size = label_size)
  
  #add date labels to seasons
  p_shaded2=p_shaded+geom_text(seasons[1:nrow(seasons)-1,], 
                     mapping=aes(x=end, 
                     label=label, y=-1),
                     hjust=1.2,
                     colour="black", 
                     angle=45,
                     size=2)+
    geom_vline(seasons[1:nrow(seasons)-1,],
               mapping=aes(
                 xintercept=end
               ),
               linetype="dotted")
  
  return(p_shaded2)
    
}

#filter=n days minimum for duration of ctrl trajectory with no roads
FindCtrlGroups<-function(geo_c,filter){
  
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
  ctrls=clusters[clusters$days>=filter,]
  return(ctrls)
}

FilterInts<-function(ints,filter_before,filter_after){
  sel_ints=ints[ints$dur_before>=filter_before*24&ints$dur_after>=filter_after*24,]
  return(sel_ints)
}

SetCtrlTrt<-function(geo_c,sel_ints,ctrls){
  geo_c$ct=""
  geo_c$jIDs=paste(geo_c$uniqueid,geo_c$clustID,sep="_")
  int_jIDs=paste(sel_ints$uniqueid,sel_ints$clustID,sep="_")
  ctrl_jIDs=paste(ctrls$uniqueid,ctrls$clustID,sep="_")
 
  #set treatments
  geo_c[geo_c$jIDs%in%int_jIDs,which(colnames(geo_c)=="ct")]<-"trt"
  
  #set ctrls
  geo_c[geo_c$jIDs%in%ctrl_jIDs,which(colnames(geo_c)=="ct")]<-"ctrl"
  
  #filter out caribou IDs not represented ever in trt/ctrl categories
  IDs=unique(geo_c[geo_c$ct=="trt"|geo_c$ct=="ctrl",which(colnames(geo_c)=="uniqueid")])
  IDs=data.frame(IDs)
  #filter out unassigned clusters
  #dat=geo_c[geo_c$ct!="",]
  dat=geo_c[geo_c$uniqueid%in%IDs$uniqueid,]
  
  return(dat)
  
}

#Visualize intersections
VizTrtCtrl<-function(dat,
                          ints_sel,
                          seasons,
                          alpha=0.3,
                          colors= c("grey85", "grey95"),
                          label_vjust=0.1,
                          label_size=2,
                          herd,
                          filter=28){
  
  clusters=
    dat %>% 
    dplyr::group_by(herd,uniqueid,int,clustID,ct) %>%
    dplyr::summarise(mint=min(t_),
                     maxt=max(t_),
                     difft=difftime(max(t_),min(t_),units="hours"),
                     nlocs=n()) %>%
    dplyr::arrange(herd,uniqueid,mint)

  clusters$jID=paste(clusters$uniqueid,clusters$clustID,sep="_")
  ints_sel$jID=paste(ints_sel$uniqueid,ints_sel$clustID,sep="_")
  colnames(ints_sel)
  jints=ints_sel[,c(which(colnames(ints_sel)=="jID"),
              which(colnames(ints_sel)=="dur_before"),
              which(colnames(ints_sel)=="dur_after"))]
  jints$dur_before=round(jints$dur_before/24)
  jints$dur_after=round(jints$dur_after/24)
  
  vdat=dplyr::left_join(clusters,jints,by="jID")
  
  vdat$difft=round(vdat$difft/24)

  vdat$year=lubridate::year(vdat$mint)
  vdat$caribou_year=paste(vdat$uniqueid,vdat$year,sep=" ")
  vdat$yday=lubridate::yday(vdat$mint)
  
  vdat$b1=vdat$mint-lubridate::days(vdat$dur_before)
  vdat$b2=vdat$mint
  vdat$d1=vdat$mint
  vdat$d2=vdat$maxt
  vdat$a1=vdat$maxt
  vdat$a2=vdat$maxt+lubridate::days(vdat$dur_after)
  
  vdat[is.na(vdat$dur_before),c(which(colnames(vdat)=="d1"),
                                which(colnames(vdat)=="d2"),
                                which(colnames(vdat)=="a1"),
                                which(colnames(vdat)=="a2")
                                )]<-NA
  
  vdat[is.na(vdat$dur_before),c(which(colnames(vdat)=="b1"),
                                which(colnames(vdat)=="b2"))]<-
    vdat[is.na(vdat$dur_before),c(which(colnames(vdat)=="mint"),
                                  which(colnames(vdat)=="maxt"))]
  
  plot_cols=apply(vdat[,c(16:21)],2,lubridate::yday)
  df=data.frame("caribou_year"=vdat$caribou_year,
                plot_cols,
                "herd"=vdat$herd,
                "trt_ctrl"=vdat$ct
                )
  
  df=split_nondec_df(df,cols=2:7)    
  df=fill_boundary_na(df,2:7,1,365)
  df$caribou_row=paste(df$caribou_year,1:nrow(df),sep=" ")
  df$caribou_ID=gsub("\\s[0-9]*$","",df$caribou_year)
  
  df[df$trt_ctrl=="ctrl",c(which(colnames(df)=="d1"),
                  which(colnames(df)=="d2"),
                  which(colnames(df)=="a1"),
                  which(colnames(df)=="a2"))]<-NA
  
  
  #line segment plot
  #each row is caribou uniqueID * year
  #x-axis is yday 1-364
  #segment 1: mint-dur_before to mint
  #segment 2: mint to maxt
  #segment 3: maxt to maxt+dur_after
  
  #group plot by herd (sep plots)
  p1=
    df[df$herd==herd,] %>%
    ggplot() +
    geom_linerange(aes(xmin = b1, 
                       xmax = b2,
                       y=caribou_ID),
                   color="#fe4a49",
                   linewidth=1,
                   position=position_dodge2(1)) +
    geom_linerange(aes(xmin = d1, 
                       xmax = d2,
                       y=caribou_ID),
                   color="#2ab7ca",
                   linewidth=1,
                   position=position_dodge2(1))+
    geom_linerange(aes(xmin = a1, 
                       xmax = a2,
                       y=caribou_ID),
                   color="#e3b505",
                   linewidth=1,
                   position=position_dodge2(1))+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title=element_text(size=8),
          panel.background = element_rect(fill = "white", colour = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = unit(c(1,1,1,1), "cm"))+
    ylab("filtered caribou trajectories")+
    facet_wrap(~trt_ctrl)
  
  p_shaded = add_season_shading(p1, 
                                seasons, 
                                colors = colors, 
                                alpha = alpha,
                                label_vjust = label_vjust,
                                label_size = label_size)
  
  #add date labels to seasons
  p_shaded2=p_shaded+geom_text(seasons[1:nrow(seasons)-1,], 
                               mapping=aes(x=end, 
                                           label=label, y=-1),
                               hjust=1.2,
                               colour="black", 
                               angle=45,
                               size=2)+
    geom_vline(seasons[1:nrow(seasons)-1,],
               mapping=aes(
                 xintercept=end
               ),
               linetype="dotted")
  
  return(p_shaded2)
  
}



dfGamma_shape<-function(dat){
  out=fitdist(dat,distr="gamma",
              lower=c(0,0),
              start=list(scale=1,shape=1))
  out$estimate[1]
}

dfGamma_rate<-function(dat){
  out=fitdist(dat,distr="gamma",
              lower=c(0,0),
              start=list(scale=1,shape=1))
  out$estimate[2]
}
#Get step lengths for treatment/ctrls
#need to get entire cluster of int + after together in one line for cf..
  #for now.. just compare cluster to no cluster for sl to test it out
SL_TC<-function(dat){
  
  #group by cluster ID, make track, get step lengths
  trk=dat %>% 
    amt::make_track(x_,y_,t_,all_cols=TRUE) |>
    amt::nest(data=-"jIDs")
  
  trk2 <- trk |> 
    amt::mutate(steps = map(data, function(x) 
      x |> 
        amt::track_resample(rate = hours(8), tolerance = hours(2)) |> 
        amt::steps_by_burst(keep_cols="start")))
  
  steps=trk2 |> 
    amt::select(jIDs,steps) |> 
    amt::unnest(cols=steps)
  
  steps$month=lubridate::month(steps$t1_)
  
  gsteps=
    steps %>% 
    amt::group_by(herd,jIDs) %>%
    amt::filter(dplyr::n() > 10) %>%
    dplyr::summarise(shape=dfGamma_shape(sl_+0.0001),
                     rate=dfGamma_rate(sl_+0.0001))
  
  #add trt/ctrl back
  gsteps$ct="trt"
  gsteps$ct[grep("-",gsteps$jIDs)]="ctrl"
  outsteps=dplyr::left_join(steps,gsteps[,c(2,3,4)],by="jIDs")
  
  return(outsteps)
  
}


MakeSeasons<-function(seasons,strt.dts,end.dts){
  seasons=data.frame("season"=seasons,"strt"=strt.dts,"end"=end.dts)
return(seasons)
}

# Parse "MM-DD" into day-of-year using a fixed reference year
md_to_doy <- function(md,year_ref) {
  # allow "MM-DD" or "MM/DD"
  md <- gsub("/", "-", md)
  d <- as.Date(paste0(year_ref, "-", md))
  as.integer(strftime(d, format = "%j"))
}

#df2=assign_season(df,t_,seasons)
assign_season <- function(df, datetime_col, seasons,
                          herd = "wah",
                          tz = "UTC",
                          year_ref = 2000,
                          season_col = "season",
                          keep_bounds = FALSE) {
  
  # df: data.frame
  # datetime_col: unquoted column name containing POSIXct
  # seasons: data.frame or matrix with columns: season, start, end
  #         where start/end are "MM-DD" strings (e.g., "03-20")
  # tz: timezone used for extracting month/day from POSIXct
  # year_ref: fixed leap year to support Feb 29 (2000 is leap)
  # keep_bounds: optionally attach computed start/end doy columns
  
  datetime_col <- rlang::ensym(datetime_col)
  
  s <- as.data.frame(seasons, stringsAsFactors = FALSE)
  if (ncol(s) < 3) stop("`seasons` must have 3 columns: season, start, end")
  names(s)[1:3] <- c("season", "start", "end")
  
  s$start_doy <- md_to_doy(s$start,year_ref)
  s$end_doy   <- md_to_doy(s$end,year_ref)
  
  # Extract day-of-year from POSIXct using same reference year
  # (we only need month/day; year_ref is just a vehicle)
  dt <- df[[rlang::as_string(datetime_col)]]
  if (!inherits(dt, "POSIXt")) stop("`datetime_col` must be POSIXct/POSIXlt.")
  
  # Convert to month-day in tz, then to doy in year_ref
  md <- format(dt, format = "%m-%d", tz = tz)
  doy <- md_to_doy(md,year_ref)
  
  # Build an assignment vector (first match wins)
  season_out <- rep(NA_character_, length(doy))
  
  for (i in seq_len(nrow(s))) {
    st <- s$start_doy[i]; en <- s$end_doy[i]
    hit <- if (st <= en) {
      doy >= st & doy <= en
    } else {
      # wraps across Dec 31 -> Jan 1
      doy >= st | doy <= en
    }
    season_out[is.na(season_out) & hit] <- s$season[i]
  }
  
  df[[season_col]] <- season_out
  
  df[df$herd!=herd,]$season<-NA
  
  if (keep_bounds) {
    attr(df, "season_bounds") <- s
  }
  df
}

#dat=tar_read(dat_its)
ModelCaribouMvmt<-function(dat){
  #filter non-wah for now
  datw=dat[dat$herd=="wah",]
  #get ID for uniqueid,season
  datw$sy_id=paste(datw$uniqueid,datw$season,lubridate::year(datw$t_),sep="_")
  dd=datw[datw$sy_id=="NPS_wah_904_fall_2010",]
  d1sf=st_as_sf(dd,coords=c("x_","y_"),crs=st_crs(6393))
  d1=datw[datw$sy_id=="NPS_wah_904_fall_2010",]
  d1=PreFormat_for_Telemetry(d1)
  te1=Convert.Telemetry(d1)
  #SVF <- variogram(te1,CI="Gauss")
  #plot(SVF)
  GUESS1 <- ctmm.guess(te1, CTMM=ctmm(error=FALSE,range = FALSE), interactive = FALSE)
  FIT1_pHREML <- ctmm.select(te1, GUESS1, method = 'pHREML',cores=40,verbose=TRUE)
  
  plot(SVF,CTMM=FIT1_pHREML[[1]],fraction=0.65,col.CTMM="red")
  summary(FIT1_pHREML)
  variogram<-variogram(te1)
  plot(variogram)
  
  out=simulate(FIT1_pHREML[[1]],nsim=10,t=te1$t)
  pt=list("te1"=te1,"sim"=out)
  plot(out,col=rainbow(length(out)))

  }

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

AssignClusterSeasons<-function(its_seq){
  
  its_seq2=its_seq %>% group_by(segID=consecutive_id(trajID,period,season))
  its_seq2=as.data.frame(its_seq2)
  return(its_seq2)
  
}

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
  
  dofit<-function(fit){
    if(all(class(fit)=="crwFit")){
      crawl::tidy_crwFit(fit)
    }
  }

RunMovementModels<-function(dat,
                      herd="wah"){
  #geo_ctrls2<-tar_read(geo_ctrls2)
  #dat=geo_ctrls2
  geo=dat[dat$herd==herd,]
  
  sf_locs=st_as_sf(geo,coords=c("x_","y_"),crs=st_crs(6393))
  
  ngeo=sf_locs %>% dplyr::group_by(uniqueid,
                                   period,
                                   season,
                                   trajID,
                                   traj.start,
                                   traj.end,
                                   traj.dur,
                                   segID) |> tidyr::nest()
  
  ngeo=ngeo %>% mutate(nrows=map(data,nrow))
  
  ngeo <- ngeo %>% 
    dplyr::mutate(
      fixpar=list(c(NA,NA))
      )
  
  ngeo <- ngeo %>% 
    dplyr::mutate(
      theta=list(c(2,0))
    )
  
  ngeo=ngeo[ngeo$nrows>10,]
  
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

getdist_x<-function(data){
  mean(as.numeric(st_coordinates(data)[,1]-lag(st_coordinates(data)[,1])),na.rm=T)
}

getdist_y<-function(data){
  mean(as.numeric(st_coordinates(data)[,2]-lag(st_coordinates(data)[,2])),na.rm=T)
}

getdifft<-function(data){
  mean(as.numeric(difftime(data$t_,lag(data$t_),units="hours")),na.rm=T)
}

#Format movement
CalculateMeanVelocity<-function(tbl_locs_fit){
  tbl_locs_fit %>%
    mutate(
      dist_x = furrr::future_pmap(list(data=data),getdist_x),
      dist_y = furrr::future_pmap(list(data=data),getdist_y),
      difft = furrr::future_pmap(list(data=data),getdifft)
    ) #m/hr
}

CombineModelParams<-function(tbl_locs_fit2){
  
  #unnest
  tbl_locs_fit3=tbl_locs_fit2 |> tidyr::unnest(cols=c(uniqueid,season,period,segID,params,dist_x,dist_y,difft))
  
  tbl_locs_fit3$vx=tbl_locs_fit3$dist_x/tbl_locs_fit3$difft
  tbl_locs_fit3$vy=tbl_locs_fit3$dist_y/tbl_locs_fit3$difft
  
  #isolate to model params
  tbl_locs_fit3=
    tbl_locs_fit3[,c("uniqueid",
                   "season",
                   "period",
                   "trajID",
                   "traj.start",
                   "traj.end",
                   "traj.dur",
                   "segID",
                   "term",
                   "estimate",
                   "std.error",
                   "conf.low",
                   "conf.high",
                   "vx",
                   "vy"
                   )]
  
  #remove AIC/logLik rows
  tbl_locs_fit3=tbl_locs_fit3[(tbl_locs_fit3$term!="AIC"),]
  tbl_locs_fit3=tbl_locs_fit3[(tbl_locs_fit3$term!="logLik"),]
  
  return(tbl_locs_fit3)
  
  }

#Next step: descriptive viz
Plot_Diffs_by_Season<-function(dat,response="ln sigma (Intercept)",velocity=FALSE,v=NULL){
  if(velocity){
    if(!missing(v)){
      if(v=="vx"){
        datwb=dat %>% dplyr::filter(term=="ln sigma (Intercept)") %>%
          tidyr::pivot_wider(
            names_from=period,
            values_from=vx,
            id_cols=c(uniqueid,season,segID))
        datwb$ba=datwb$after-datwb$before
        x_axis_title="x-coordinate velocity difference after road interaction (m)"
      }
      if(v=="vy"){
        datwb=dat %>% dplyr::filter(term=="ln sigma (Intercept)") %>%
          tidyr::pivot_wider(
            names_from=period,
            values_from=vy,
            id_cols=c(uniqueid,season,segID))
        datwb$ba=datwb$after-datwb$before
        x_axis_title="y-coordinate velocity difference after road interaction (m)"
      }
    }
  } else{
    datwb=dat %>% dplyr::filter(term==response) %>%
      tidyr::pivot_wider(
        names_from=period,
        values_from=estimate,
        id_cols=c(uniqueid,season,segID))
    datwb$ba=datwb$after-datwb$before
    x_axis_title=paste0(response," difference after road interaction")
    
  }
  
  
  datwb %>% 
    ggplot() +
    geom_point(
      aes(x=ba, y=segID), 
      size = 4
    ) + 
    facet_wrap(~season) +
    geom_vline(xintercept=0)+
    labs(x=x_axis_title)
  
}

GetTrajDates<-function(geo){
  geo_out=geo %>% 
    dplyr::group_by(trajID) %>% 
    dplyr::mutate(traj.start=min(t_),
                  traj.end=max(t_),
                  traj.dur=difftime(max(t_),
                                    min(t_),
                                    units="days")) %>% 
 return(geo_out)
  
}


