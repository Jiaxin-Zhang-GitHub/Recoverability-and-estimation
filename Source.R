#################################################################################
#  Source.R                                                                     # 
#  functions and parameters for Simu.control.R                                  #
#################################################################################
library(ggrepel)
library(boot)
library(nnet) 
library('MASS')
library("mice")
library(car)
library(magrittr) 
library(dplyr)
library(plyr)
library(ggrepel)
library(tidyr)
library(purrr)
library(parallel) 
library(mefa)
library(stringr)
load("coef.M.Rda")
load("coef.X.Rda")
set.seed(20230221)

# function: get coeffivicients for generation
coeff <- function(i){
  return(t(as.matrix(coef(i))))
}

# function: generate exposure 
gen.X <- function(n, seed, coef.X) {
  set.seed(seed)
  unit1 <- rep(1, n)
  sim <- data.frame(unit1)
  sim$A <- rnorm(n, mean = 0, sd = 1)
  sim$C1 <- rbinom(n, 1, coef.X$b.C1[!is.na(coef.X$b.C1)])
  sim$C2 <- rbinom(n, 1, inv.logit(coef.X$b.C2[!is.na(coef.X$b.C2)] %*% t(sim)))
  sim$C3 <- rbinom(n, 1, inv.logit(coef.X$b.C3[!is.na(coef.X$b.C3)] %*% t(sim)))
  sim$C4 <- rbinom(n, 1, inv.logit(coef.X$b.C4[!is.na(coef.X$b.C4)] %*% t(sim)))
  sim$C5 <- rbinom(n, 1, inv.logit(coef.X$b.C5[!is.na(coef.X$b.C5)] %*% t(sim)))
  sim$X <- rbinom(n, 1, inv.logit(coef.X$b.X[!is.na(coef.X$b.X)] %*% t(sim)))
  return(sim)
}

# function: generate outcomes
gen.Y <- function(n, seed, coef.X) {
  set.seed(seed)
  sim <- gen.X(n, seed, coef.X)
  C.C <- rbind(sim$C1*sim$C4, sim$C2*sim$C4, sim$C3*sim$C4, sim$C4*sim$C5, sim$C3*sim$C5)
  # Outcome Scenario I: no interaction
  sim$I <- rnorm(n, mean = coef.X$b.I[!is.na(coef.X$b.I)] %*% rbind(t(sim)[-c(2),], C.C), sd = 1) %>% `-`(mean(.))
  # Outcome Scenario II: weak interaction between confounder C3 and the exposure X
  sim$II <- rnorm(n, mean = coef.X$b.II[!is.na(coef.X$b.II)] %*% rbind(t(sim)[-c(2,9:9),], C.C, sim$X * sim$C3), sd = 1) %>% `-`(mean(.))
  # Outcome Scenario III: waek interaction between confounder C4 and the exposure X
  sim$III <- rnorm(n, mean = coef.X$b.III[!is.na(coef.X$b.III)] %*% rbind(t(sim)[-c(2,9:10),], C.C, sim$X * sim$C4), sd = 1) %>% `-`(mean(.))
  # Outcome Scenario IV: weak interaction XC3 and XC4
  sim$IV <- rnorm(n, mean = coef.X$b.IV[!is.na(coef.X$b.IV)] %*% rbind(t(sim)[-c(2,9:11),], C.C, sim$X * sim$C3, sim$X * sim$C4), sd = 1) %>% `-`(mean(.))
  # Outcome Scenario V: strong interaction between confounder C3 and the exposure X
  sim$V <- rnorm(n, mean = coef.X$b.V[!is.na(coef.X$b.V)] %*% rbind(t(sim)[-c(2,9:12),], C.C, sim$X * sim$C3), sd = 1) %>% `-`(mean(.))
  # Outcome Scenario VI: strong interaction between confounder C4 and the exposure X
  sim$VI <- rnorm(n, mean = coef.X$b.VI[!is.na(coef.X$b.VI)] %*% rbind(t(sim)[-c(2,9:13),], C.C, sim$X * sim$C4), sd = 1) %>% `-`(mean(.))
  sim <- sim[,-1]
  sim[2:7] <- lapply(sim[2:7], as.factor)
  return(sim)
}

# function: generate missingness indicators
gen.M <- function(coef.X, Y, seed, n, adj.coef){
  sim <- gen.X(n, seed, coef.X)
  C.C <- rbind(sim$C1*sim$C4, sim$C2*sim$C4, sim$C3*sim$C4, sim$C4*sim$C5, sim$C3*sim$C5)
  if(Y == 1) {sim$Y <- rnorm(n, mean = coef.X$b.I[!is.na(coef.X$b.I)] %*% rbind(t(sim)[-2,], C.C), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 2) {sim$Y <- rnorm(n, mean = coef.X$b.II[!is.na(coef.X$b.II)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C3), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 3) {sim$Y <- rnorm(n, mean = coef.X$b.III[!is.na(coef.X$b.III)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C4), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 4) {sim$Y <- rnorm(n, mean = coef.X$b.IV[!is.na(coef.X$b.IV)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C3, sim$X*sim$C4), sd = 1) %>% `-`(mean(.))}
  else if(Y == 5) {sim$Y <- rnorm(n, mean = coef.X$b.V[!is.na(coef.X$b.V)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C3), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 6) {sim$Y <- rnorm(n, mean = coef.X$b.VI[!is.na(coef.X$b.VI)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C4), sd = 1) %>% `-`(mean(.))} 
  sim$X_C3 <- sim$X*sim$C3; sim$X_C4 <- sim$X*sim$C4; sim$X_C5 <- sim$X*sim$C5; sim$X_Y <- sim$X*sim$Y 
  sim$A <- rnorm(n, 0, 1)
  lapply(adj.coef, function(coef){rbinom(n, 1, inv.logit(coef %*% t(sim[,1:13]))) %>% mean(.)}) 
} 

# function: convert value to NA
na <- function(variable, indicator) {
  variable <- ifelse(indicator == 1, NA, variable)
  return(variable)
}

# function: generate incomplete data for a given exposure proportion and m-DAG
gen <- function(X, m.DAG, Y, m, n){
  unit1 <- rep(1, n)
  sim <- data.frame(unit1)
  sim$A <- rnorm(n, mean = 0, sd = 1)
  sim$C1 <- rbinom(n, 1, coef.X[[X]]$b.C1[!is.na(coef.X[[X]]$b.C1)])
  sim$C2 <- rbinom(n, 1, inv.logit(coef.X[[X]]$b.C2[!is.na(coef.X[[X]]$b.C2)] %*% t(sim)))
  sim$C3 <- rbinom(n, 1, inv.logit(coef.X[[X]]$b.C3[!is.na(coef.X[[X]]$b.C3)] %*% t(sim)))
  sim$C4 <- rbinom(n, 1, inv.logit(coef.X[[X]]$b.C4[!is.na(coef.X[[X]]$b.C4)] %*% t(sim)))
  sim$C5 <- rbinom(n, 1, inv.logit(coef.X[[X]]$b.C5[!is.na(coef.X[[X]]$b.C5)] %*% t(sim)))
  sim$X <- rbinom(n, 1, inv.logit(coef.X[[X]]$b.X[!is.na(coef.X[[X]]$b.X)] %*% t(sim)))
  C.C <- rbind(sim$C1*sim$C4, sim$C2*sim$C4, sim$C3*sim$C4, sim$C4*sim$C5, sim$C3*sim$C5)
  if(Y == 1) {sim$Y <- rnorm(n, mean = coef.X[[X]]$b.I[!is.na(coef.X[[X]]$b.I)] %*% rbind(t(sim)[-2,], C.C), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 2) {sim$Y <- rnorm(n, mean = coef.X[[X]]$b.II[!is.na(coef.X[[X]]$b.II)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C3), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 3) {sim$Y <- rnorm(n, mean = coef.X[[X]]$b.III[!is.na(coef.X[[X]]$b.III)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C4), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 4) {sim$Y <- rnorm(n, mean = coef.X[[X]]$b.IV[!is.na(coef.X[[X]]$b.IV)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C3, sim$X*sim$C4), sd = 1) %>% `-`(mean(.))}
  else if(Y == 5) {sim$Y <- rnorm(n, mean = coef.X[[X]]$b.V[!is.na(coef.X[[X]]$b.V)] %*% rbind(t(sim)[-2,], C.C, sim$X * sim$C3), sd = 1) %>% `-`(mean(.))} 
  else if(Y == 6) {sim$Y <- rnorm(n, mean = coef.X[[X]]$b.VI[!is.na(coef.X[[X]]$b.VI)] %*% rbind(t(sim)[-2,], C.C, sim$X*sim$C4), sd = 1) %>% `-`(mean(.))}
  sim$X_C3 <- sim$X*sim$C3; sim$X_C4 <- sim$X*sim$C4; sim$X_C5 <- sim$X*sim$C5; sim$X_Y <- sim$X*sim$Y 
  sim$W <- rnorm(n, 0, 1)
  M.ind <- lapply(coef.M[[X]][[m.DAG]][[Y]][[m]], function(coef){rbinom(n, 1, inv.logit(coef %*% t(sim[,c(1,14,3:13)])))}) %>% as.data.frame(.)
  sim$C4 <- na(sim$C4,M.ind$M.C4); sim$C5 <- na(sim$C5,M.ind$M.C5); sim$X <- na(sim$X,M.ind$M.X); sim$Y <- na(sim$Y,M.ind$M.Y)
  sim$Missingness <- m; sim$Outcome <- Y
  sim[,3:8] <- lapply(sim[,3:8], as.factor)
  sim <- sim[,c(2:9,15,16)]
} 

# function: the ACE estimate given by g-computation
ACE <- function(data, analysis){
  fit <- try(eval(analysis, data), silent = T)
  if(!grepl("Error", fit[1], fixed = T)) {
    mean(predict(fit, newdata = replace(data, "X", as.factor(1)), type = 'response') - 
           predict(fit, newdata = replace(data, "X", as.factor(0)), type = 'response'), na.rm = T)
  } else {NA}
}
# function: g-computation for standard error and p-value
gcomp.fun <- function(data, ind, analysis){
  data <- data[ind,]
  fit <- try(eval(analysis, data), silent = T)
  if(!grepl("Error", fit[1], fixed = T)) {
    mean(predict(fit, newdata = replace(data, "X", as.factor(1)), type = 'response') - 
           predict(fit, newdata = replace(data, "X", as.factor(0)), type = 'response'), na.rm = T)
  } else {NA}
}

gcomp <- function(data, boot.R, analysis){
  boot.result <- boot(data, gcomp.fun, stype = "i", R = boot.R, analysis = analysis, parallel = "multicore", ncpus = 4)
  as.data.frame(cbind(boot.result$t0, sqrt(var(boot.result$t)[1,1]),
                      with(boot.result, 2*pnorm(abs((mean(t)) / sqrt(var(t)[1,1])), lower.tail = F))
  )) %>% `names<-`(c("estimate","std.error","p.value"))
}

# function: get the bootstrapping number via Donald's method
boot.r <- function(data, boot.R, analysis){
  boot.result <- boot(data, gcomp.fun, stype = "i", R = boot.R, analysis = analysis, parallel = "multicore", ncpus = 4)
  as.data.frame(cbind(boot.result$t0, 
                      w_hat.se <- ((sum((boot.result$t - mean(boot.result$t))^4))/((boot.R-1)*((var(boot.result$t)[1,1])^2)) -1)/4,
                      B.se <- as.integer(1.96^2*100*w_hat.se),
                      max.B <- max(boot.R, B.se)
  )) %>% `names<-`(c("estimate","w_hat.se","B.se","max.B"))
}

# list: analysis models 
analysis <- list(substitute(lm(I ~ C1 + C2 + C3 + C4 + C5 + X + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(II ~ C1 + C2 + C3 + C4 + C5 + X + X*C3 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(III ~ C1 + C2 + C3 + C4 + C5 + X + X*C4 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(IV ~ C1 + C2 + C3 + C4 + C5 + X + X*C3 + X*C4 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(V ~ C1 + C2 + C3 + C4 + C5 + X + X*C3 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(VI ~ C1 + C2 + C3 + C4 + C5 + X + X*C4 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)))

# function: check exposure and complete-case proportions
prop <- function(dat){
  X.prop <- mean(as.numeric(as.character(dat$X)), na.rm = T)
  miss.prop <- lapply(dat[,5:8], function(x){sum(is.na(x))/nrow(dat)}) %>% as.data.frame(.)
  comp.prop <- sum(complete.cases(dat))/nrow(dat)
  cbind(X.prop, miss.prop, comp.prop)
}