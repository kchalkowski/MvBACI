library(targets)
library(tarchetypes)

#set directories
setwd(this.path::this.dir())
outdir<-file.path("Output")

#source targets file
source("_targets.R")

#Get pipeline
tar_manifest()

#Make pipeline
tar_make()

#pg=pgeo[pgeo$period=="during"&pgeo$type=="trt",]
#pg=st_as_sf(pg,coords=c("x_","y_"),crs=st_crs(6393))
#mapview(pg,alpha=0.1)+mapview(rd1f)+mapview(rd2f,color="red")

