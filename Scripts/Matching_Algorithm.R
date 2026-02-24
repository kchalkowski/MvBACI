#pseudocode
#traj_t=traj_IDs for trtment
#traj_c=traj_IDs for contrl
#for each treatment trajectory t
  #for each ctrl trajectory c
    #start_overlap=max(t$start_date,c$start_date)
    #end_overlap=min(t$end_date,c$end_date)
    #duration=difftime(end_overlap,start_overlap)
    
    #if c==1,
      #dur_c=duration
    #else 
      #c(dur_c,duration)
  #end c for loop
  #max_IDs=traj_c[which(dur_c==max(dur_c))]
  #df_t=data.frame(matrix(nrow=length(max_IDs),ncol=3))
  #df_t[,1]=traj_t[t]
  #df_t[,2]=max_IDs
  #df_t[,3]=max(dur_c)
  #if t==1
    #df=df_t
    #else
      #df=rbind(df,df_t)
  #close t for loop
  #return df

#Matching criteria
  #maximum overlap of days
  #if two ctrl that have the same overlap of days, keep both for now
  #other situation, to consider later:
    #what if multiple treatments have same matching ctrls? ie same overlap
      #how to optimize for max number of treatment/control pairs?
      #pocket for now-- may not be problem after filtering for correlated paths, etc.
#its_seq,geo_ctrls

overlap_duration <- function(s1, e1, s2, e2) {
  overlap_start <- pmax(s1, s2)
  overlap_end   <- pmin(e1, e2)
  duration      <- pmax(0, difftime(overlap_end,overlap_start,units="days"))
  return(duration)
}


Match_Ctrl_Trt<-function(its_seq,geo_ctrls){
  
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

  
  return(res.filt)
}

