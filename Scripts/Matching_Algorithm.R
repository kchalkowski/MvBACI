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

Match_Ctrl_Trt<-function(tbl_locs_fit3,ctrl_locs_fit3){
  
  #trim est cols and get unique rows for each segID
  trt=unique(tbl_locs_fit3[,c(1,4,5,6,7)])
  ctrl=unique(ctrl_locs_fit3[,c(1,4,5,6,7)])
  
  #*note, need go back and see why there are NAs showing up here
  #remove NAs 
  trt=trt[complete.cases(trt),]
  ctrl=ctrl[complete.cases(ctrl),]
  
  #check that segIDs are unique
  #any(duplicated(trt$segID))
  #any(duplicated(ctrl$segID))
  
  #get IDs for each type
  trt_IDs=trt$trajID
  ctrl_IDs=ctrl$trajID
  
  #loop through treatment trajs
  for(t in 1:length(trt_IDs)){
    print(paste0("matching treatment traj ",t," of ",length(trt_IDs)))
    traj_t=trt[trt$trajID==trt_IDs[t],]
    for(c in 1:length(ctrl_IDs)){
      traj_c=ctrl[ctrl$trajID==ctrl_IDs[c],]
      start_overlap=max(traj_t$traj.start,traj_c$traj.start)
      end_overlap=min(traj_t$traj.end,traj_c$traj.end)
      duration=difftime(end_overlap,start_overlap)
      #if c==1,
      #dur_c=duration
      #else 
      #c(dur_c,duration)
      if(c==1){
        dur_c=duration
      } else{
        dur_c=c(dur_c,duration)
      }
        
    } #end c for loop
    
    max_ctrl_IDs=ctrl_IDs[which(dur_c==max(dur_c))]
    df_t=data.frame(matrix(nrow=length(max_ctrl_IDs),ncol=3))
    df_t[,1]=trt_IDs[t]
    df_t[,2]=max_ctrl_IDs
    df_t[,3]=max(dur_c)
    
    #if t==1
    #df=df_t
    #else
    #df=rbind(df,df_t)
    #close t for loop
    #return df
    if(t==1){
      df=df_t
    } else{
      df=rbind(df,df_t)
    }
    
  } #end t for loop
  return(df)
}

