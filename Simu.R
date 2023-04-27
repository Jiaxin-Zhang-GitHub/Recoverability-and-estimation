#################################################################################
#  Simu.R                                                                       # 
#  simulation for missingness methods in different exposure prevalents          #
#################################################################################

args = commandArgs(trailingOnly=TRUE)
exposure <- args[1]
mDAG <- args[2]
method <- as.numeric(args[3])
scenario <- as.numeric(args[4]) 

library(magrittr) 
library(plyr)
set.seed(2021051*method*(scenario+9))
source(file = "Methods.R")
setting <- paste(exposure, mDAG, sep = ".")
load(paste("data",setting,"Rda", sep = "."))

simu.data <- simu.data[[scenario]]
# Store the data frame for the error return by tryCatch 
error <- data.frame(estimate=NA, std.error=NA, p.value=NA, low=NA, up=NA, outcome = simu.data[[1]][1,10], missingness = simu.data[[1]][1,9], method = method)


if(method == 1){
  CCA <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      gcomp(simu.data[[i]])
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "CCA", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, CCA) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} else if(method == 2){
  MI.Sim <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      MI.Sim(simu.data[[i]])
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "MI.Sim", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, MI.Sim) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} else if(method == 3){
  MI.CART <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      MI.CART(simu.data[[i]])
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "MI.CART", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, MI.CART) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} else if(method == 4){
  MI.RF <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      MI.RF(simu.data[[i]])
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "MI.RF", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, MI.RF) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} else if(method == 5){
  MI.EC <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      MI.EC(simu.data[[i]])
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "MI.EC", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, MI.EC) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} else if(method == 6){
  MI.Com <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      MI.Com(simu.data[[i]])
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "MI.Com", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, MI.Com) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} else if(method == 7){
  MI.SMC <- if(is.data.frame(simu.data[[1]])) {
    lapply(c(1:n.sim), function(i) {
      tryCatch(MI.SMC(simu.data[[i]]), error = function(e) error)
    }) %>% ldply(., data.frame) %>% perform.ind(.,n.sim) %>% cbind(., Method = "MI.SMC", setting = setting)
  }
  load(paste("res",setting,"Rda", sep = "."))
  simu.res <- rbind(simu.res, MI.SMC) 
  save(simu.res, file = paste("res",setting,"Rda", sep = "."))
  
} 