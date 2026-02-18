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
matches$X1<-as.character(matches$X1)
matches$X2<-as.character(matches$X2)
matches$X3<-as.numeric(matches$X3)

#need remove row/colnames
Wmat=data.table::dcast(as.data.table(matches,keep.rownames=FALSE),X1~X2,value.var="X3")
Wmat=as.matrix(Wmat)
Wmat=Wmat[,2:ncol(Wmat)]
storage.mode(Wmat)="numeric"
Wmat=Wmat*-1 #flip the sign, since algo below minimizes cost instead of maximizes weight

#Wmat rows are treatment
#Wmat cols are ctrl
Wmat_optim=HungarianSolver(Wmat)

#1st col is treatment
#second col is contrl
pairs=Wmat_optim$pairs

#need convert pairs (which has row/col numbers) to IDs
rownames(Wmat)=unique(matches[,1])
pairs=as.data.frame(pairs)
colnames(pairs)=c("trt","ctrl")
pairs$trt=rownames(Wmat)
pairs$ctrl=colnames(Wmat)[pairs$ctrl]

return(pairs)
}

