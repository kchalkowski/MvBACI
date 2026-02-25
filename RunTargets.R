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

baci_sigma<-tar_read(baci_sigma)
baci_beta<-tar_read(baci_beta)
baci_vx<-tar_read(baci_vx)
baci_vy<-tar_read(baci_vy)

ggsave("./Plots/baci_intxn_plots/baci_sigma.png",
       plot=baci_sigma)
ggsave("./Plots/baci_intxn_plots/baci_beta.png",
       plot=baci_beta)
ggsave("./Plots/baci_intxn_plots/baci_vx.png",
       plot=baci_vx)
ggsave("./Plots/baci_intxn_plots/baci_vy.png",
       plot=baci_vy)



