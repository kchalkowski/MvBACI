

#took from Elie's smoove source code
Get.nutau <- function(fit)
{
  sigma.hat <- exp(fit$par[1])
  sigma.CI <- exp(fit$ci[1,])
  
  tau.hat <- 1/exp(fit$par[2])
  tau.CI <- sort(1/exp(fit$ci[2,]))
  
  nu.hat <- sqrt(pi/tau.hat)*sigma.hat/2
  nu.CI <- sqrt(pi/tau.hat)*sigma.CI/2
  
  results <- data.frame(Estimate = c(tau.hat, nu.hat), rbind(tau.CI, nu.CI))
  names(results) <- c("Estimate", "CI.low", "CI.high")
  return(results)
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
                P = diag(c(1 ^ 2, 1 ^ 2,
                           1 ^ 2, 1 ^ 2)))
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

GetMovementParameters<-function(geo,minrow=10){
  
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
  
  ngeo=ngeo %>% mutate(nrows=purrr::map(data,nrow))
  
  ngeo <- ngeo %>% 
    dplyr::mutate(
      fixpar=list(c(NA,NA))
    )
  
  ngeo <- ngeo %>% 
    dplyr::mutate(
      theta=list(c(2,0))
    )
  
  ngeo=ngeo[ngeo$nrows>minrow,]

  tbl_locs_fit <- ngeo %>% 
    dplyr::mutate(fit = furrr::future_pmap(list(d = data,f=fixpar,t=theta),
                                           fit_crawl,.options=furrr::furrr_options(seed=TRUE)))
  
  tbl_locs_fit2 <- 
    tbl_locs_fit %>% 
    dplyr::mutate(
      params=
        map(fit,dofit)
    )
  
  tbl_locs_fit3 <- 
    tbl_locs_fit2 %>% 
    dplyr::mutate(
      nutau=
        map(fit,Get.nutau)
    )
  
  return(tbl_locs_fit3)
}

#

GetT<-function(dat){
  start_time=min(dat$t_)
  T_=as.numeric(julian(dat$t_, origin = start_time))
  return(T_)
}

GetZ<-function(dat){sf::st_coordinates(dat)[,1] + 1i*sf::st_coordinates(dat)[,2]}

GetZM=function(dat){
  T_=GetT(dat)
  Z=GetZ(dat)
  multicvm <- data.frame("Z" = Z, "T_" = T_) %>% 
    arrange(T_) %>%
    as.data.frame
  return(multicvm)
}

nestedSweepRACVM<-function(ZM){
  Z=ZM$Z
  T_=ZM$T_
  if(max(T_)>=(28+7)*2){
    simSweep=sweepRACVM(Z = Z, T = T_, 
                        windowsize = 7, windowstep = 28, 
                        model = "UCVM", progress=FALSE)
    simCP.table=simSweep |> 
      findCandidateChangePoints(clusterwidth = 2) |>
      getCPtable(modelset = c("ACVM"))
  } else{
    simCP.table=NA
  }
  return(simCP.table)
}


plotPhaseCustom<-function(variable, phaselist, 
                          cols = 1:length(phaselist), 
                          label = TRUE, log = "", ...){
  variabletable <- getPhaseParameter(variable, phaselist)
  low.plot <- variabletable$low
  high.plot <- variabletable$high
  
  with(variabletable,{
    plot(range(start, end), 
         range(low.plot, high.plot, na.rm=TRUE), 
         type="n", log = log, ...)
    if(!is.na(low[1])) rect(start, low.plot, end, high.plot, 
                            col = scales::alpha(cols, .5), bor=NA) 
    
    segments(start, hat, end, hat, lwd = 2, col=cols)
    segments(end[-length(end)], hat[-length(end)], start[-1], hat[-1], col="grey")
    
    if(label) mtext(variable, side = 3, at = start[1], font = 3, adj = 0, cex = 0.8)
  })
  
}

plotPhaseListCustom<- function(phaselist,
                               cols = gplots::rich.colors(length(phaselist)),
                               plot.parameters = TRUE,
                               parameters = NULL, 
                               plot.legend = TRUE, 
                               legend.where = "bottomright",
                               layout = c("horizontal","vertical")){
  
  Z <- attr(phaselist, "Z")
  time <- attr(phaselist, "time")
  phaseTable <- summarizePhases(phaselist)
  
  if(is.null(parameters)){
    allparameters <-  c("eta", "tau", "rms", "mu.x", "mu.y", "omega.x", "omega.y")
    parameters <- names(phaseTable)[names(phaseTable) %in% allparameters]
  }
  
  mars <- par()$mar
  omas <- par()$oma
  omas[1] <- 2
  
  n.param <- length(parameters)
  
  if(plot.parameters){
    if(grepl(layout[1], "vertical")){
      layout(1:(n.param + 1), 
             heights = c(1, rep(1/n.param, n.param)))
      par(mar = c(1, mars[2], 2, mars[4]), oma = omas)
    }
    else{
      par(mar = c(0, mars[2], 2, mars[4]), oma = omas)
      layout(cbind(rep(1, n.param), 1:n.param+1))
    }
  }
  
  T.cuts <- c(phaseTable$start, max(time))
  Z.cols <- cols[cut(time, T.cuts, include.lowest = TRUE)] 
  
  # plot Track
  plot(Z, asp=1, type="l", xpd=FALSE, xlab = "X", ylab = "Y")
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  if(plot.legend){
    legend(legend.where, legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
           fill=cols, ncol=3, bty="n", title = "Phase: model")
  }
  
  # plotParameters
  if(plot.parameters){  
    mars <- par()$mar
    par(mar = c(0, mars[2], 1.5, mars[4]))
    for(p in parameters)
      #plotPhaseParameter(p, phaselist, ylab="", xlab="", col=cols, 
      #                   xaxt= ifelse(p == parameters[length(parameters)], "s", "n"),
      #                   log = ifelse(p =="tau", "y", ""))
      plotPhaseCustom(p,
                      phaselist,
                      ylab="", 
                      xlab="", 
                      cols = gplots::rich.colors(length(phaselist)),
                      xaxt= ifelse(p == parameters[length(parameters)], "s", "n"),
                      log = ifelse(p =="tau", "y", ""))
    
  }
}

nestedestimatePhases<-function(simCPtbl){
  if(all(is.na(simCPtbl))){
    out=NA
  } else{
    out=estimatePhases(simCPtbl,verbose=FALSE)
  }
  return(out)
}

#get phase parameters
#phaselist=movepairs5[1,]$phaseList[[1]]
#getPhaseParameter("tau",dat)
nestedPhaseParameters<-function(phaselist){
  if(all(is.na(phaselist))){
    parms=NA
  } else{
  parameters=c("eta","mu.x","mu.y","tau","rms")
  for(p in 1:length(parameters)){
    parm.p=getPhaseParameter(parameters[p],phaselist)
    parm.p$parm=parameters[p]
    if(!exists("parms")){
      parms=parm.p
    } else{
      parms=rbind(parms,parm.p)
    }
  }
  
  }
  return(parms)
}


GetChangePoints<-function(movepairs){
  
  #format data for smoove
  movepairs=
    movepairs %>% 
    dplyr::mutate(
      ZM=map(data,GetZM)
    )
  
  #for now, removing these pairs
  #need to integrate better into pipeline
  #need filter large jumps in geolocation time
  movepairs=movepairs[movepairs$pairID!="ctrl261_137",]
  movepairs=movepairs[which(movepairs$pairID!="ctrl310_213"),]
  movepairs=movepairs[which(movepairs$pairID!="ctrl414_219"),]
  movepairs=movepairs[which(movepairs$pairID!="ctrl439_243"),]
  
  #run the sweep to get the changepoints
  movepairs=
    movepairs %>%
    dplyr::mutate(
      simCPtbl=map(ZM,nestedSweepRACVM)
    )
  
  #estimate phases
  movepairs=
    movepairs %>%
    dplyr::mutate(
      phaseList=map(simCPtbl,nestedestimatePhases)
    )
  
  #get phase parameters
  movepairs=
    movepairs %>%
    dplyr::mutate(
      parms=map(phaseList,nestedPhaseParameters)
    )

  return(movepairs)
 
}


#Make terms from nutau wider
WideNuTau<-function(nutau){
  
  nutau$term=rownames(nutau)
  nutau=tidyr::pivot_wider(nutau,
                           names_from="term",
                           names_sep="_mean_",
                           values_from=c("Estimate","CI.low","CI.high")) %>%
    as.data.frame()
  
  
  return(nutau)
}

WidePhaseParms<-function(parms){

  if(all(is.na(parms))){
    parms2=NA
  } else{
  parms2=tidyr::pivot_wider(parms,
                           names_from="parm",
                           names_sep="_",
                           id_cols=c("phase","start","end"),
                           values_from=c("hat","low","high")) %>%
    as.data.frame()
  }
  return(parms2)
}

#Tidy movepairs
Tidy_Move_Pairs<-function(movepairs2){
  
  movepairs2 <- 
    movepairs2 %>% 
    dplyr::mutate(
      nutau2=
        map(nutau,WideNuTau)
    )
  
  movepairs2 <- 
    movepairs2 %>% 
    dplyr::mutate(
      parms2=
        map(parms,WidePhaseParms)
    )
  
  movepairs3=movepairs2 |> tidyr::unnest(cols=c(uniqueid,season,period,trajID,segID,type,pairID,group_start,group_end,nutau2,parms2))
  
  movepairs3=movepairs3[,c(1:9,21:44)]
  
  return(movepairs3)  
}
