#################################################################################
#  Methods.R                                                                    # 
#  functions and parameters for Case.R and Simu.R                               #
#################################################################################
library(ggrepel)
library(boot)
library(nnet) 
library('MASS')
library("mice")
library(car)
library(magrittr) 
library(plyr)
library(dplyr)
library(ggrepel)
library(tidyr)
library(purrr)
library(smcfcs)

# setting parameters for bootstrapping numbers, simulation replications, number of multiply imputed data sets and imputation iteration numbers 
boot.R = 240; n.sim = 2000; m = 5; maxit = 5

# Function to conduct g-computation 
# @param data Data for analysis
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios
gcomp <- function(data, Outcome = data[1,10], Missingness = data[1,9]){
  ana <- analysis[Outcome]
  gcomp.fun <- function(data, ind, analysis){
    data <- data[ind,]
    fit <- try(eval(analysis[[1]], data), silent = T)
    if(!grepl("Error", fit[1], fixed = T)) {
      mean(predict(fit, newdata = replace(data, "X", as.factor(1)), type = 'response') - 
             predict(fit, newdata = replace(data, "X", as.factor(0)), type = 'response'), na.rm = T)
    } else {NA}
  }
  boot.result <- boot(data, gcomp.fun, stype = "i", R = boot.R, analysis = ana, parallel = "multicore", ncpus = 4)
  as.data.frame(cbind(boot.result$t0, sqrt(var(boot.result$t)[1,1]),
                      with(boot.result, 2*pnorm(abs((mean(t)) / sqrt(var(t)[1,1])), lower.tail = F)),
                      t(with(boot.result, 2*t0 - mean(t) + qnorm(c(0.025, 0.975)) %o% sqrt(var(t)[1,1]))),
                      Outcome, Missingness
  )) %>% `names<-`(c("estimate", "std.error", "p.value", "low", "up", "outcome", "missingness"))
}

# Function to pool imputation results
# @param MI List of return from multiple imputation methods
# @param Outcome Outcome scenario
# @param Missingness Missingness scenario
# @param Method Missing data method
# @return Pooled results
MI.pool <- function(MI, Outcome, Missingness, Method){
  m <- ifelse(length(MI) == 22, MI$m, length(MI[["impDatasets"]]))
  g.res <- lapply(c(1:m), function(i){
    if(length(MI) == 22) {gcomp(complete(MI, i), Outcome, Missingness)
    } else {gcomp(MI[["impDatasets"]][[i]], Outcome, Missingness)}
  }) %>% ldply(., data.frame)
  est <- mean(g.res$estimate, na.rm = T); b <- var(g.res$estimate, na.rm = T); v <- mean(g.res$std.error^2, na.rm = T)
  t <- v+(1+1/m)*b; df <- (m-1)*(1+v/(b+b/m))^2
  if(is.nan(est) | is.nan(v)) est <- v <- t <- df <- NA
  as.data.frame(cbind(est, sqrt(t), 2*pt(abs((est) / sqrt(t)), df, lower.tail = F), 
                      est - qt(0.975, df)*sqrt(t), est + qt(0.975, df)*sqrt(t), Outcome, Missingness, Method
  )) %>% `names<-`(c("estimate", "std.error", "p.value", "low", "up", "outcome", "missingness", "method")) 
}

# list: analysis models
analysis <- list(substitute(lm(Y ~ C1 + C2 + C3 + C4 + C5 + X + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(Y ~ C1 + C2 + C3 + C4 + C5 + X + X*C3 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(Y ~ C1 + C2 + C3 + C4 + C5 + X + X*C4 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(Y ~ C1 + C2 + C3 + C4 + C5 + X + X*C3 + X*C4 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(Y ~ C1 + C2 + C3 + C4 + C5 + X + X*C3 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)),
                 substitute(lm(Y ~ C1 + C2 + C3 + C4 + C5 + X + X*C4 + C1*C4 + C2*C4 + C3*C4 + C5*C4 + C3*C5)))

# Function to calculate performance measurements
# @param inc Analysis results from missing data methods 
# @param n.sim Number of replication in simulation
# @return performance measurements
perform.ind <- function(inc, n.sim) {
  inc[,1:5] <- apply(inc[,1:5], 2, as.numeric)
  Estimate = mean(inc$estimate, na.rm = T)
  Bias = mean(inc$estimate, na.rm = T) - 0.3
  PrBias = Bias/0.3*100 # unit: %
  MSE = mean((inc$estimate - 0.3)^2)
  EmpSE = sd(inc$estimate, na.rm = T)
  ModSE = sqrt(mean(inc$std.error^2, na.rm = T))
  ReErModSE = (ModSE/EmpSE-1)*100
  Coverage = mean(apply(cbind(inc$low, inc$up), 1, function(x){ifelse(x[1]>0.3 | x[2]<0.3, 0, 1)}), na.rm = T) * 100# unit:%
  Power = mean(inc$p.value < 0.05, na.rm = T) * 100# unit: %
  Bias.MCSE = EmpSE/sqrt(n.sim)
  EmpSE.MCSE = EmpSE/sqrt(2*(n.sim - 1))
  ModSE.MCSE = sqrt(var(inc$std.error^2, na.rm = T)/(4*n.sim*ModSE^2))
  Coverage.MCSE = sqrt(Coverage*(100-Coverage)/n.sim)/100
  ReErModSE.MCSE = sqrt((ModSE.MCSE/ModSE)^2+1/(2*n.sim-2))*(ModSE/EmpSE)*100
  Missingness = inc$missingness[1]; Outcome = inc$outcome[1]
  NA.prop = sum(is.na(inc$estimate))/n.sim
  return(cbind(Estimate, Bias, PrBias, Bias.MCSE, MSE, EmpSE, EmpSE.MCSE, ModSE, ModSE.MCSE, Coverage, Coverage.MCSE, 
               ReErModSE, ReErModSE.MCSE, Power, Missingness, Outcome, NA.prop))
}

# Function to conduct Method II: Simple Multiple Imputation
# @param dat Data to impute
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios, and imputation method
MI.Sim <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]
  mice(data = dat, m = m, maxit = maxit, method = c("norm","","","","logreg","logreg","logreg","norm"), vis = "monotone", print = F) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.Sim")
}

# Function to conduct Method III: Multiple Imputation using classification and regression trees
# @param dat Data to impute
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios, and imputation method
MI.CART <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]
  mice(dat, method = "cart", m = m, maxit = maxit, print = F) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.CART")
}

# Function to conduct Method IV: Multiple Imputation using random forest
# @param dat Data to impute
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios, and imputation method
MI.RF <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]
  mice(dat, method = "rf", m = m, maxit = maxit, print = F) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.RF")
}

# Function to conduct Method V: Exposure-confounder Multiple Imputation
# @param dat Data to impute
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios, and imputation method
MI.EC <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]
  dat$C1.C4 <- (as.numeric(as.character(dat$C4)))*(as.numeric(as.character(dat$C1)))
  dat$C2.C4 <- (as.numeric(as.character(dat$C4)))*(as.numeric(as.character(dat$C2)))
  dat$C3.C4 <- (as.numeric(as.character(dat$C4)))*(as.numeric(as.character(dat$C3)))
  dat$C5.C4 <- (as.numeric(as.character(dat$C5)))*(as.numeric(as.character(dat$C4)))
  dat$C5.C3 <- (as.numeric(as.character(dat$C3)))*(as.numeric(as.character(dat$C5)))
  dat$X.C1 <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C1)))
  dat$X.C2 <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C2)))
  dat$X.C3 <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C3)))
  dat$X.C4 <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C4)))
  dat$X.C5 <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C5)))
  predmat <- make.predictorMatrix(dat)
  predmat["C4",grepl("C4", colnames(predmat), fixed = T)] <- 0
  predmat["C5",grepl("C5", colnames(predmat), fixed = T)] <- 0
  predmat["X",grepl("X", colnames(predmat), fixed = T)] <- 0
  method <- c("norm","","","","logreg","logreg","logreg","norm",
              "~I(as.numeric(as.character(C1))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C2))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C3))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C5))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C5))*as.numeric(as.character(C3)))",
              "~I(as.numeric(as.character(X))*as.numeric(as.character(C1)))",
              "~I(as.numeric(as.character(X))*as.numeric(as.character(C2)))",
              "~I(as.numeric(as.character(X))*as.numeric(as.character(C3)))",
              "~I(as.numeric(as.character(X))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(X))*as.numeric(as.character(C5)))")
  mice(dat, m = m, maxit = maxit, method = method, vis = "monotone", predictorMatrix = predmat, print = F) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.EC")
}

# Function to conduct Method VI: Approximately compatiable Multiple Imputation
# @param dat Data to impute
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios, and imputation method
MI.Com <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]
  dat$C1.C4 <- (as.numeric(as.character(dat$C4)))*(as.numeric(as.character(dat$C1)))
  dat$C2.C4 <- (as.numeric(as.character(dat$C4)))*(as.numeric(as.character(dat$C2)))
  dat$C3.C4 <- (as.numeric(as.character(dat$C4)))*(as.numeric(as.character(dat$C3)))
  dat$C5.C4 <- (as.numeric(as.character(dat$C5)))*(as.numeric(as.character(dat$C4)))
  dat$C5.C3 <- (as.numeric(as.character(dat$C3)))*(as.numeric(as.character(dat$C5)))
  dat$C1.Y <- (as.numeric(as.character(dat$Y)))*(as.numeric(as.character(dat$C1)))
  dat$C2.Y <- (as.numeric(as.character(dat$Y)))*(as.numeric(as.character(dat$C2)))
  dat$C3.Y <- (as.numeric(as.character(dat$Y)))*(as.numeric(as.character(dat$C3)))
  dat$C4.Y <- (as.numeric(as.character(dat$Y)))*(as.numeric(as.character(dat$C4)))
  dat$C5.Y <- (as.numeric(as.character(dat$Y)))*(as.numeric(as.character(dat$C5)))
  dat$X.Y <- (as.numeric(as.character(dat$Y)))*(as.numeric(as.character(dat$X)))
  dat$C3.X <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C3)))
  dat$C4.X <- (as.numeric(as.character(dat$X)))*(as.numeric(as.character(dat$C4)))
  predmat <- make.predictorMatrix(dat)
  predmat["C4",grepl("C4", colnames(predmat), fixed = T)] <- 0
  predmat["C5",grepl("C5", colnames(predmat), fixed = T)] <- 0; predmat["C5",c("C1.Y","C2.Y","X.Y")] <- 0
  predmat["X",grepl("X", colnames(predmat), fixed = T)] <- 0; predmat["X",c("C1.Y","C2.Y","C5.Y")] <- 0
  predmat["Y",grepl("Y", colnames(predmat), fixed = T)] <- 0
  method <- c("norm","","","","logreg","logreg","logreg","norm",
              "~I(as.numeric(as.character(C1))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C2))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C3))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C5))*as.numeric(as.character(C4)))",
              "~I(as.numeric(as.character(C5))*as.numeric(as.character(C3)))",
              "~I(as.numeric(as.character(C1))*as.numeric(as.character(Y)))",
              "~I(as.numeric(as.character(C2))*as.numeric(as.character(Y)))",
              "~I(as.numeric(as.character(C3))*as.numeric(as.character(Y)))",
              "~I(as.numeric(as.character(C4))*as.numeric(as.character(Y)))",
              "~I(as.numeric(as.character(C5))*as.numeric(as.character(Y)))",
              "~I(as.numeric(as.character(X))*as.numeric(as.character(Y)))",
              "~I(as.numeric(as.character(C3))*as.numeric(as.character(X)))",
              "~I(as.numeric(as.character(C4))*as.numeric(as.character(X)))")
  if(!grepl("X * C3", as.character(analysis[Outcome]), fixed = T)) {predmat[c("C4","C5","Y"),"C3.X"] <- 0; predmat["X","C3.Y"] <- 0} 
  if(!grepl("X * C4", as.character(analysis[Outcome]), fixed = T)) {predmat[c("C5","Y"),"C4.X"] <- 0; predmat["C4","X.Y"] <- 0; predmat["X","C4.Y"] <- 0} 
  mice(dat, m = m, maxit = maxit, method = method, vis = "monotone", predictorMatrix = predmat, print = F) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.Com")
}

# Function to conduct Method VI: Substantive model compatible Multiple Imputation
# @param dat Data to impute
# @return ACE estimate, standard error, p-value, low and up bound for 95% confidence interval, outcome and missingness scenarios, and imputation method
MI.SMC <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]; smformula <- paste(analysis[Outcome][[1]][2], "+ A")
  smcfcs(dat, smtype = "lm", smformula = smformula, method = c("","","","","logreg","logreg","logreg",""), m = m, numit = maxit) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.SMC")
}



