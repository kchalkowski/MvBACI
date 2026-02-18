Remove_Duplicate_Matches<-function(matches){
  trtIDs=unique(matches[,1])
  for(t in 1:length(trtIDs)){
  #for(t in 1:7){
    print(paste0("matching treatment traj ",t," of ",length(trtIDs)))
    trt_match=matches[matches[,1]==trtIDs[t],]
    print(trt_match)
    if(nrow(trt_match)>1){
      s=sample(1:nrow(trt_match),1)
      trt_match=trt_match[s,]
    }
    rem_ctrl=trt_match[,2]
    print(rem_ctrl)
    matches=matches[matches[,2]!=rem_ctrl,]
    print(nrow(matches))
    if(t==1){
      matches_f=trt_match
    } else{
      matches_f=rbind(matches_f,trt_match)
    } 
  }
    return(matches_f)
}