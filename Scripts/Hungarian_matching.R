#Nt=treatment
#Nc=ctrl
#weights=num days overlapping
#given Nt and Nc weight matrix W
#find the optimal set of m=max{Nt,Nc} matchings (i,j) 
#where i is a set of Nt and j is a set of Nd
#such that the cost of the matchings sum(Wij) is maximized
#use RcppHungarian to do the above

#pseudocode
#make weight matrix W
#convert all weights to negative, since Hungarian solver function operates on minimum values
#HungarianSolver(cost)

#matches<-tar_read(matches)

Hungarian_matching<-function(matches){
  
matches$ctrl_id<-as.character(matches$ctrl_id)
matches$trt_id<-as.character(matches$trt_id)
matches$overlaps<-as.numeric(matches$overlaps)

#trim matches
#matches=matches[matches$X3>0,]
#test=matches %>% group_by(X2) %>% dplyr::summarise(ss=sum(X3>0),max(X3))

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

