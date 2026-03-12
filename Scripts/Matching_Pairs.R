Match_Ctrl_Trt<-function(its_seq,geo_ctrls,model_df){
  print(1)
  matches=GetAllMatches(its_seq,geo_ctrls,model_df)
  print(2)
  pairs=Hungarian_matching(matches)
  print(3)
  pgeo=Filter_Pairs(its_seq,geo_ctrls,pairs)
  return(pgeo)
}

calc_difference=function(x,y){
  abs(x-y)
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
    dplyr::select(ctrl_id, trt_id, trt_period, ctrl_start, ctrl_end, trt_start, trt_end, overlap_days) %>%
    ungroup()
  
  res.total=result %>% dplyr::summarise(overlaps=sum(overlap_days),po=sum(overlap_days==0), .by = c(ctrl_id, trt_id))
  res.filt=res.total[res.total$overlaps>0&res.total$po==0,]
  
  #run getspatialdist
  #left join res.filt with output from getspatialdist
  #keep cols with d=NA (trim the matches)
  #res.dist=GetSpatialDist(its_seq,geo_ctrls, 150000)
  
  #Join to get dists
  #res.filt$ID=paste(res.filt$trt_id,res.filt$ctrl_id,sep="_")
  #res.dist$ID=paste(res.dist$trt_id,res.dist$ctrl_id,sep="_")
  #res.dist=res.dist[,c(4,3)]
  #res.filt2=left_join(res.filt,res.dist,by="ID")
  
  #keep those with NA (means they are < dist)
  #res.filt2=res.filt2[is.na(res.filt2$min_dist),]
  
  #removing col for joining
  #res.filt2=res.filt2[,c(1:4)]
  
  return(res.filt)
}

#Helper function for Match_Ctrl_Trt
overlap_duration <- function(s1, e1, s2, e2) {
  overlap_start <- pmax(s1, s2)
  overlap_end   <- pmin(e1, e2)
  duration      <- pmax(0, difftime(overlap_end,overlap_start,units="days"))
  return(duration)
}

#Helper function for Match_Ctrl_Trt
PropScoreDiffs<-function(model_df){
  
  trt=model_df[model_df$type=="trt",]
  ctrl=model_df[model_df$type=="ctrl",]
  
  #Matching rules
    #only matching within ysID, between trt/ctrl

  #keep only trt during
  trt=trt[trt$period=="during",]
  
  #remove ctrl ysIDs that is their last season (where >1 season)
  end_seasons=ctrl %>% 
    dplyr::group_by(trajID) %>% 
    dplyr::summarise(end_season=max(ysID),
                     ns=n_distinct(ysID))
  #filter out those with only one season
  end_seasons=end_seasons[end_seasons$ns>1,]
  end_seasons$key=paste(end_seasons$trajID,end_seasons$end_season,sep="_")
  end_seasons=end_seasons[,4]
  end_seasons$val=1
  ctrl$key=paste(ctrl$trajID,ctrl$ysID,sep="_")
  ctrl=left_join(ctrl,end_seasons,by="key")
  ctrl=ctrl[is.na(ctrl$val),]
  
  #get all matches, remove those with different ysIDs later
  result <- ctrl %>%
    dplyr::rename(ctrl_id = trajID, ctrl_ysID=ysID, ctrl_score = propensity_score) %>%
    dplyr::cross_join(trt %>% rename(trt_id = trajID, trt_ysID=ysID, trt_score = propensity_score)) %>%
    dplyr::mutate(ps_diff = calc_difference(ctrl_score, trt_score)) %>%
    dplyr::select(ctrl_id, trt_id, ctrl_ysID,trt_ysID,ps_diff)
  
  result=result[result$ctrl_ysID==result$trt_ysID,]
  
  return(result)
}

#Get all matches
GetAllMatches<-function(its_seq,geo_ctrls,model_df){
  matches1=PropScoreDiffs(model_df)
  matches2=FindOverlapDurations(its_seq,geo_ctrls)
  
  #pairs that are not in matches2 do not overlap in time, and do not overlap during all 3 periods for each treatment
  #make key and join together
  matches1$key=paste(matches1$ctrl_id,matches1$trt_id)
  matches2$key=paste(matches2$ctrl_id,matches2$trt_id)
  matches_j=left_join(matches1,matches2[,c(3:5)],by="key")
  
  matches_out=matches_j[complete.cases(matches_j),]
  matches_out=matches_out[,c(1:5,7)]
  return(matches_out)
}

#Helper function for Match_Ctrl_Trt
Hungarian_matching<-function(matches){
  
  matches$ctrl_id<-as.character(matches$ctrl_id)
  matches$trt_id<-as.character(matches$trt_id)
  matches$ps_diff<-as.numeric(matches$ps_diff)
  

  matches2=
    matches %>% 
    ungroup() %>% 
    dplyr::select(ctrl_id,trt_id,ps_diff) %>% 
    dplyr::summarise(psd_mean=mean(ps_diff),.by=c(ctrl_id,trt_id))
  
  #need remove row/colnames
  Wmat=data.table::dcast(as.data.table(matches2,keep.rownames=FALSE),ctrl_id~trt_id,value.var="psd_mean")
  Wmat=as.matrix(Wmat)
  rownames(Wmat)=Wmat[,1]
  Wmat=Wmat[,2:ncol(Wmat)]
  storage.mode(Wmat)="numeric"
  
  Wmat[is.na(Wmat)]<-1000000000 #weight to reduce selection of ineligible pairs
  
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
  pairs$trt=colnames(Wmat)[pairs$trt]
  pairs$pairID=paste(pairs$ctrl,pairs$trt,sep="_")
  matches2$pairID=paste(matches2$ctrl_id,matches2$trt_id,sep="_")
  
  #left join to filter out ineligible matches if any
  pairs=left_join(pairs,matches2,by="pairID")
  
  pairs=pairs[complete.cases(pairs),]
  
  #add stop here, unique n trt/ctrl needs be equal
  print("finished Hungarian Matching")
  return(pairs)
}

Filter_Pairs<-function(its_seq,geo_ctrls,pairs){
  its_seq$type="trt"
  geo_ctrls$type="ctrl"
  geo_ctrls$period="before"
  its_seq$trajID=as.character(its_seq$trajID)
  
  dat=dplyr::bind_rows(geo_ctrls,its_seq)
  dat_summary=dat %>% dplyr::group_by(trajID) %>% dplyr::summarise(mint=min(t_),maxt=max(t_))

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
    group_by(pairID, period) %>% 
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
  check_period_overlaps=dat6 %>% 
    dplyr::summarise(nt=n_distinct(type),
                     .by=c(pairID, group_uid)) 
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
  check2=dat7n2 %>% 
    ungroup() %>%
    dplyr::summarize(nt=n_distinct(type),
                     .by = c(pairID, period, season, segID))
  check2=unique(check2[,c(which(colnames(check2)=="pairID"),
                          which(colnames(check2)=="nt"))])
  check2=check2[check2$nt<2,]
  drop_pairIDs=check2$pairID
  
  #remove pairIDs with only control or treatment for respective season
  dat_filtered=dat7n2[!(dat7n2$pairID%in%drop_pairIDs),]

  return(dat_filtered)
  
}

#Helper function for Filter_Pairs
changeperiod=function(df){
  df[df$type=="ctrl",which(colnames(df)=="period")]=
    df[df$type=="trt",which(colnames(df)=="period")][[1]][1]
  return(df)
}
