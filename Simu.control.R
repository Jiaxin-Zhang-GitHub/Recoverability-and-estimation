#################################################################################
#  Save regression coefficients and missingness coefficients                    #
#################################################################################
load("VAHCS.CleanData.Rda")

# function: read the regression coefficients
coeff <- function(i){
  return(t(as.matrix(coef(i))))
}

## Regression coefficients from VAHCS
b.C1 <- mean(as.numeric(dat$pari)) - 1
b.C2 <- coeff(glm(pard ~ age + pari, data = dat, family = binomial(link = "logit")))
b.C3 <- coeff(glm(asb ~ age + pari + pard, data = dat, family = binomial(link = "logit")))
b.C4 <- coeff(glm(cmd ~ age + pari + pard + asb, data = dat, family = binomial(link = "logit")))
b.C5 <- coeff(glm(alc ~ age + pari + pard + asb + cmd, data = dat, family = binomial(link = "logit")))
b.X <- coeff(glm(thc ~ age + pari + pard + asb + cmd + alc, data = dat, family = binomial(link = "logit")))
b.IV <- coeff(glm(mhs ~ pari + pard + asb + cmd + alc + thc + pari*cmd + pard*cmd + asb*cmd + 
              alc*cmd + asb*alc + thc*asb + thc*cmd, data = dat, family = gaussian(link = "identity")))
b.I = b.II = b.III = b.V = b.VI <- b.IV[1:12]

coef <- list(b.C1, b.C2, b.C3, b.C4, b.C5, b.X, b.I, b.II, b.III, b.IV, b.V, b.VI)
coef <- as.data.frame(sapply(coef, '[', seq(max(sapply(coef, length)))))
names(coef)[1:12] <- c("b.C1", "b.C2", "b.C3", "b.C4", "b.C5", "b.X", "b.I", "b.II", "b.III", "b.IV", "b.V", "b.VI")
coef <- round(coef, digits = 3)
save(coef, file = "coef.X.Rda")

# Missingness indicators
dat$M.C4 <- as.numeric(is.na(dat$cmd))
dat$M.C5 <- as.numeric(is.na(dat$alc))
dat$M.X <- as.numeric(is.na(dat$thc))
dat$M.Y <- as.numeric(is.na(dat$mhs))
dat$W <- rnorm(nrow(dat),0,1)

M.C4.i <- as.data.frame(cbind(coeff(glm(M.C4 ~ W + pari + pard + asb, data = dat, 
          family = binomial(link = "logit"))), t(rep(log(3),4)), t(rep(0,4)))) %>% rep(., 10) %>% 
          t(.) %>% `colnames<-`(LETTERS[1:10]) %>% `rownames<-`(c("Int","W","C1","C2","C3","C4",
          "C5","X","Y","X*C3","X*C4","X*C5","X*Y")) %>% as.data.frame(.)
M.C4.i[c("C4","C5"),c("A","B","C","G","H")] <- 0; M.C4.i["X",c("A","D","F")] <- 0
M.C4.i["Y",c("A","B","D","E","G")] <- 0
M.C4.ii <- M.C4.i[,-1]; M.C4.ii["X*C3",c("B","C","E","G","H","I","J")] <- log(2)
M.C4.iii <- M.C4.i[,-c(1,4,6)]; M.C4.iii["X*C4",c("E","I","J")] <- log(2)
M.C4.iv <- M.C4.i[,-c(1,4,6)]; M.C4.iv["X*C5",c("E","I","J")] <- log(2)
M.C4.v <- M.C4.i[,-c(1,2,4,5)]; M.C4.v["X*Y",c("C","H","I","J")] <- log(2)
M.C4 <- list(M.C4.i,M.C4.ii,M.C4.iii,M.C4.iv,M.C4.v) %>% `names<-`(c("i","ii","iii","iv","v"))
M.C4 <- map(M.C4, round, digits = 3)

M.C5.i <- as.data.frame(cbind(coeff(glm(M.C5 ~ W + pari + pard + asb, data = dat, 
          family = binomial(link = "logit"))), t(rep(log(3),4)), t(rep(0,4)))) %>% rep(., 10) %>% 
          t(.) %>% `colnames<-`(LETTERS[1:10]) %>% `rownames<-`(c("Int","W","C1","C2","C3","C4",
          "C5","X","Y","X*C3","X*C4","X*C5","X*Y")) %>% as.data.frame(.)
M.C5.i[c("C4","C5"),c("A","B","C","G","H")] <- 0; M.C5.i["X",c("A","D","F")] <- 0 
M.C5.i["Y",c("A","B","D","E","G")] <- 0
M.C5.ii <- M.C5.i[,-1]; M.C5.ii["X*C3",c("B","C","E","G","H","I","J")] <- log(2)
M.C5.iii <- M.C5.i[,-c(1,4,6)]; M.C5.iii["X*C4",c("E","I","J")] <- log(2)
M.C5.iv <- M.C5.i[,-c(1,4,6)]; M.C5.iv["X*C5",c("E","I","J")] <- log(2)
M.C5.v <- M.C5.i[,-c(1,2,4,5)]; M.C5.v["X*Y",c("C","H","I","J")] <- log(2)
M.C5 <- list(M.C5.i,M.C5.ii,M.C5.iii,M.C5.iv,M.C5.v) %>% `names<-`(c("i","ii","iii","iv","v"))
M.C5 <- map(M.C5, round, digits = 3)

M.X.i <- as.data.frame(cbind(coeff(glm(M.X ~ W + pari + pard + asb, data = dat, 
         family = binomial(link = "logit"))), t(rep(log(3),4)), t(rep(0,4)))) %>% rep(., 10) %>% 
         t(.) %>% `colnames<-`(LETTERS[1:10]) %>% `rownames<-`(c("Int","W","C1","C2","C3","C4",
         "C5","X","Y","X*C3","X*C4","X*C5","X*Y")) %>% as.data.frame(.)
M.X.i[c("C4","C5"),c("A","D","F")] <- 0; M.X.i["X",c("A","B","C","G","H")] <- 0 
M.X.i["Y",c("A","B","D","E","G")] <- 0
M.X.ii <- M.X.i[,-1]; M.X.ii["X*C3",c("D","E","F","I","J")] <- log(2)
M.X.iii <- M.X.i[,-c(1,4,6)]; M.X.iii["X*C4",c("E","I","J")] <- log(2)
M.X.iv <- M.X.i[,-c(1,4,6)]; M.X.iv["X*C5",c("E","I","J")] <- log(2)
M.X.v <- M.X.i[,-c(1,2,4,5)]; M.X.v["X*Y",c("F","I","J")] <- log(2)
M.X <- list(M.X.i,M.X.ii,M.X.iii,M.X.iv,M.X.v) %>% `names<-`(c("i","ii","iii","iv","v"))
M.X <- map(M.X, round, digits = 3)

M.Y.i <- as.data.frame(cbind(coeff(glm(M.Y ~ W + pari + pard + asb, data = dat, 
         family = binomial(link = "logit"))), t(rep(log(3),3)), t(rep(0,5)))) %>% rep(., 10) %>% 
         t(.) %>% `colnames<-`(LETTERS[1:10]) %>% `rownames<-`(c("Int","W","C1","C2","C3","C4",
         "C5","X","Y","X*C3","X*C4","X*C5","X*Y")) %>% as.data.frame(.)
M.Y.i[c("C4","C5"),c("A","D","F")] <- 0; M.Y.i["X",c("A","D","F")] <- 0 
M.Y.i["Y",c("G","H","J")] <- log(3)
M.Y.ii <- M.Y.i[,-1]; M.Y.ii["X*C3",] <- log(2); M.Y.ii["X*C3",c("D","F")] <- 0
M.Y.iii <- M.Y.i[,-c(1,4,6)]; M.Y.iii["X*C4",] <- log(2)
M.Y.iv <- M.Y.i[,-c(1,4,6)]; M.Y.iv["X*C5",] <- log(2)
M.Y.v <- M.Y.i[,-c(1,2,4,5)]; M.Y.v["X*Y",c("G","H","J")] <- log(2)
M.Y <- list(M.Y.i,M.Y.ii,M.Y.iii,M.Y.iv,M.Y.v) %>% `names<-`(c("i","ii","iii","iv","v"))
M.Y <- map(M.Y, round, digits = 3)

coef.M <- lapply(LETTERS[1:10], function(m.DAG){
  lapply(c(1:5), function(i){
    if(m.DAG %in% colnames(M.C4[[i]])){
    as.data.frame(cbind(M.C4[[i]][,m.DAG], M.C5[[i]][,m.DAG], M.X[[i]][,m.DAG], M.Y[[i]][,m.DAG])) %>% 
        `colnames<-`(c("M.C4","M.C5","M.X","M.Y"))}
  }) %>% `names<-`(c("i","ii","iii","iv","v")) %>% list(.,.,.,.,.,.) %>% `names<-`(c("I","II","III","IV","V","VI"))
}) %>% `names<-`(LETTERS[1:10]) %>% list(.,.) %>% `names<-`(c("10%","50%"))
save(coef.M, file = "coef.M.Rda")


#################################################################################
#  Adjust C3 prevalence and exposure proportion                                 #
#################################################################################
rm(list=ls())
cl <- makeCluster(31)
parLapply(cl, c(1:31), function(x) {source(file = "Source.R")})
C3.prop <- parLapply(cl, c(seq(1.3,1.4,0.001)), function(delta){
  adj.coef <- coef
  adj.coef[1,3] <- adj.coef[1,3] + delta
  lapply(seq(1000,30000,1000), function(seed){
    mean(gen.X(100000, seed, adj.coef)[,5]) 
  }) %>% ldply(., data.frame) %>% 
    # Three exposure prevalent levels: 10% and 50%
    apply(., 2, mean) %>% cbind(adj.coef[1,3], abs(. - 0.3))
}) %>% ldply(., data.frame)
stopCluster(cl)
load("coef.X.Rda")
coef[1,3] <- C3.prop[C3.prop[,3]==min(C3.prop[,3]),2]
coef.X <- list(coef, coef) %>% `names<-`(c("10%","50%"))
save(coef.X, file = "coef.X.Rda")


rm(list=ls())
cl <- makeCluster(24)
parLapply(cl, c(1:24), function(x) {source(file = "Source.R")})
X.prop <- parLapply(cl, c(seq(-1.05,-0.95,0.001), seq(2.8,2.9,0.001)), function(delta){
  adj.coef <- coef.X[["10%"]]
  adj.coef[1,6] <- adj.coef[1,6] + delta
  lapply(seq(1000,30000,1000), function(seed){
    mean(gen.X(100000, seed, adj.coef)[,8]) 
  }) %>% ldply(., data.frame) %>% 
    # Two exposure prevalent levels: 10% and 50%
    apply(., 2, mean) %>% cbind(adj.coef[1,6], abs(. - 0.1), abs(. - 0.5))
}) %>% ldply(., data.frame)
stopCluster(cl)
load("coef.X.Rda")
coef.X[["10%"]][1,6] <- X.prop[X.prop[,3]==min(X.prop[,3]),2] 
coef.X[["50%"]][1,6] <- X.prop[X.prop[,4]==min(X.prop[,4]),2]
save(coef.X, file = "coef.X.Rda")


#################################################################################
#  Adjust coefficients in the outcome generation                                #
#################################################################################
rm(list=ls())
cl <- makeCluster(11)
parLapply(cl, c(1:11), function(x) {source(file = "Source.R")})
load("coef.X.Rda")

# set the initial value
coef.X[["10%"]][7,7:12] <- c(0.300, 0.263, 0.439, 0.358, 0.157, -0.348)
coef.X[["50%"]][7,7:12] <- c(0.300, 0.260, 0.430, 0.359, 0.157, -0.347)

coef.X[["10%"]][7,7:12] <- parLapply(cl, seq(-0.1, 0.1, 0.02), function(delta){
  adj.coef <- coef.X[["10%"]]
  adj.coef[7, 7:12] <- adj.coef[7, 7:12] + delta
  adj.coef[13, 8:12] <- adj.coef[7, 8:12] %>% `*`(c(1/2, -1/2, 1/2, 3, -3))
  adj.coef[14, 10] <- adj.coef[7, 10]*(-1/2)
  lapply(seq(1000,30000,1000), function(seed){
    dat <- adj.coef %>% gen.Y(100000, seed, .) 
    lapply(c(1:6), function(Y){
      ACE(dat, analysis[[Y]])
    }) %>% ldply(., data.frame) %>% t(.)
  }) %>% ldply(., data.frame) %>% 
    # True ACE: 0.3
    apply(., 2, function(ACE){abs(mean(ACE)-0.3)}) %>% t(.) %>% cbind(adj.coef[7, 7:12], .)
}) %>% ldply(., data.frame) %>% {
  lapply(c(1:6), function(i){.[.[,i+6]==min(.[,i+6]),i]})
}
coef.X[["10%"]][13,8:12] <- coef.X[["10%"]][7,8:12] %>% `*`(c(1/2, -1/2, 1/2, 3, -3))
coef.X[["10%"]][14,10] <- coef.X[["10%"]][7,10]*(-1/2)

coef.X[["50%"]][7,7:12] <- parLapply(cl, seq(-0.1, 0.1, 0.02), function(delta){
  adj.coef <- coef.X[["50%"]]
  adj.coef[7, 7:12] <- adj.coef[7, 7:12] + delta
  adj.coef[13, 8:12] <- adj.coef[7, 8:12] %>% `*`(c(1/2, -1/2, 1/2, 3, -3))
  adj.coef[14, 10] <- adj.coef[7, 10]*(-1/2)
  lapply(seq(1000,30000,1000), function(seed){
    dat <- adj.coef %>% gen.Y(100000, seed, .) 
    lapply(c(1:6), function(Y){
      ACE(dat, analysis[[Y]])
    }) %>% ldply(., data.frame) %>% t(.)
  }) %>% ldply(., data.frame) %>% 
    # True ACE: 0.3
    apply(., 2, function(ACE){abs(mean(ACE)-0.3)}) %>% t(.) %>% cbind(adj.coef[7, 7:12], .)
}) %>% ldply(., data.frame) %>% {
  lapply(c(1:6), function(i){.[.[,i+6]==min(.[,i+6]),i]})
}
coef.X[["50%"]][13,8:12] <- coef.X[["50%"]][7,8:12] %>% `*`(c(1/2, -1/2, 1/2, 3, -3))
coef.X[["50%"]][14,10] <- coef.X[["50%"]][7,10]*(-1/2)
stopCluster(cl)
save(coef.X, file = "coef.X.Rda")


#################################################################################
#  Adjust missingness proportion                                                #
#################################################################################
rm(list=ls())
cl <- makeCluster(6)
parLapply(cl, c(1:6), function(x) {source(file = "Source.R")})
# Adjust intercepts of missingness indicators' generation for each exposure and m-DAG setting. 
# Here we illustrated the progress using an example of 10% exposure in m-DAG C.
X="10%"; m.DAG="C"

# Step 1: rough search
rough.search <- parLapply(cl, c(1:6), function(Y){
  X="10%"; m.DAG="C"; n=100000
  lapply(c(1:5), function(missingness){
    if(is.data.frame(coef.M[[X]][[m.DAG]][[Y]][[missingness]])) {
      coef.M[[X]][[m.DAG]][[Y]][[missingness]][1,] <- lapply(seq(-5,-1,0.1), function(delta){
        adj.coef <- rbind(rep(delta,4), coef.M[[X]][[m.DAG]][[Y]][[missingness]][-1,])
        lapply(seq(1000,30000,1000), function(seed){
          gen.M(coef.X[[X]], Y, seed, n, adj.coef) 
        }) %>% ldply(., data.frame) %>% apply(., 2, mean) %>% t(.) %>% `-`(c(0.15,0.15,0.2,0.2)) %>% 
          abs(.) %>% cbind(adj.coef[1,], .)
      }) %>% ldply(., data.frame) %>% {lapply(c(1:4), function(i){.[.[,i+4]==min(.[,i+4]),i]})} %>% 
        ldply(., data.frame) %>% t(.)
    }
  })
})
load("coef.M.Rda")
lapply(c(1:6), function(Y){
  lapply(c(1:5), function(missingness){
    if(is.data.frame(coef.M[[X]][[m.DAG]][[Y]][[missingness]])) {
      coef.M[[X]][[m.DAG]][[Y]][[missingness]][1,] <<- rough.search[[Y]][[missingness]]
    }
  })
})
save(coef.M, file = "coef.M.Rda")

# step 2: exact search
exact.search <- parLapply(cl, c(1:6), function(Y){
  load("coef.M.Rda")
  lapply(c(1:5), function(missingness){
    X="10%"; m.DAG="C";n=100000
    if(is.data.frame(coef.M[[X]][[m.DAG]][[Y]][[missingness]])) {
      coef.M[[X]][[m.DAG]][[Y]][[missingness]][1,] <- lapply(seq(-0.1,0.1,0.001), function(delta){
        adj.coef <- coef.M[[X]][[m.DAG]][[Y]][[missingness]] 
        adj.coef[1,] <- adj.coef[1,] + delta
        lapply(seq(1000,30000,1000), function(seed){
          gen.M(coef.X[[X]], Y, seed, n, adj.coef) 
        }) %>% ldply(., data.frame) %>% apply(., 2, mean) %>% t(.) %>% `-`(c(0.15,0.15,0.2,0.2)) %>% 
          abs(.) %>% cbind(adj.coef[1,], .)
      }) %>% ldply(., data.frame) %>% {lapply(c(1:4), function(i){.[.[,i+4]==min(.[,i+4]),i]})} %>% 
        ldply(., data.frame) %>% t(.)
    }
  })
})
load("coef.M.Rda")
lapply(c(1:6), function(Y){
  lapply(c(1:5), function(missingness){
    if(is.data.frame(coef.M[[X]][[m.DAG]][[Y]][[missingness]])) {
      coef.M[[X]][[m.DAG]][[Y]][[missingness]][1,] <<- exact.search[[Y]][[missingness]]
    }
  })
})
save(coef.M, file = "coef.M.Rda")
stopCluster(cl)


#################################################################################
#  Find out the appropriate sample sizes                                        #
#################################################################################
sample.size <- lapply(c("10%","50%"), function(X){
  lapply(seq(500, 3000, 100), function(n){
    lapply(seq(0, 200000, 100), function(seed){
      dat <- gen.Y(n, seed, coef.X[[X]])
      lapply(c(1:6), function(Y){
        ifelse(gcomp(dat, boot.R = 240, analysis[[Y]])$p.value < 0.05, 1, 0)
      }) %>% ldply(., data.frame) %>% t(.)
    }) %>% ldply(., data.frame) %>% apply(., 2, mean) %>% t(.) %>% cbind(n, ., X)
  }) %>% ldply(., data.frame)
}) %>% ldply(., data.frame) %>% `colnames<-`(c("n", as.character(as.roman(1:6)),"exposure%"))
save(sample.size, file = "sample.size.Rda")
# For 10% exposure, the sample size that achieves 80% power in each outcome scenario is: 1400, 2200, 2000, 2700, 2200 and 2200
# For 50% exposure, the sample size that achieves 80% power in all outcome scenarios are: 700


#################################################################################
#  Find out the number of bootstrapping                                         #
#################################################################################
rm(list=ls())
cl <- makeCluster(16)
parLapply(cl, c(1:16), function(x) {source(file = "Source.R")})
boot.n <- parLapply(cl, seq(192, 272, 10), function(boot.R){
  lapply(c(1:4), function(Y){
    lapply(seq(0, 200000, 100), function(seed){
      if(Y == 1){dat.10 <- gen.Y(1400, seed, coef.X[["10%"]])} else if(Y == 2){
        dat.10 <- gen.Y(2200, seed, coef.X[["10%"]])} else if(Y == 3){dat.10 <- gen.Y(2000, seed, coef.X[["10%"]])} 
      else if(Y == 4){dat.10 <- gen.Y(2700, seed, coef.X[["10%"]])} else if(Y == 5){
        dat.10 <- gen.Y(2200, seed, coef.X[["10%"]])} else if(Y == 6){dat.10 <- gen.Y(2200, seed, coef.X[["10%"]])}
      dat.50 <- gen.Y(700, seed, coef.X[["50%"]]) 
      cbind(boot.r(dat.10, boot.R, analysis[[Y]])$max.B, boot.r(dat.50, boot.R, analysis[[Y]])$max.B)
    }) %>% ldply(., data.frame) %>% apply(., 2, mean) %>% t(.) %>% cbind(boot.R, Y, .)
  }) %>% ldply(., data.frame) 
}) %>% ldply(., data.frame) 
stopCluster(cl)
# The bootstrapping number for a proper standard error in each outcome scenario is 240


#################################################################################
#  Generate incomplete data for missingness methods                             #
#################################################################################
rm(list=ls())
source(file = "Source.R")
set.seed(20220128)
n.sim <- 2000; setting = "50%.C" 
# Generate simulation data for all settings. The example of 10% exposure prevalence in m-DAG C.
simu.data <- lapply(c(1:30), function(n){vector("list", n.sim)})

lapply(c(1:n.sim), function(i){
  lapply(c(1:6), function(Y){
    lapply(c(1:5), function(m){
      X <- str_split(setting, "\\.")[[1]][1]; m.DAG <- str_split(setting, "\\.")[[1]][2]
      if(X == "50%"){n = 700} else if(Y == 1){n = 1400} else if(Y == 2|5){n = 2200} 
      else if(Y == 3|6){n = 2000} else if(Y == 4){n = 2700}
      if(is.data.frame(coef.M[[X]][[m.DAG]][[Y]][[m]])) {
        simu.data[[(Y-1)*5+m]][[i]] <<- gen(X, m.DAG, Y, m, n = n)
      }
    })
  })
}) 
save(simu.data, file = paste("data",setting,"Rda", sep = "."))

# Save an empty dataset for simulation results 
simu.res <- as.data.frame(matrix(0,1,18)) %>% `names<-`(c("Estimate", "Bias", "PrBias", "Bias.MCSE", 
            "EmpSE", "EmpSE.MCSE", "ModSE", "ModSE.MCSE", "Coverage", "Coverage.MCSE", "ReErModSE", 
            "ReErModSE.MCSE", "Power", "Missingness", "Outcome", "NA.prop", "Method", "scenario"))
save(simu.res, file = paste("res",setting,"Rda", sep = "."))

# Check exposure and complete-case proportions
lapply(c(1:20), function(i){
  map(simu.data[[i]], prop) %>% ldply(., data.frame) %>% apply(., 2, mean)
}) %>% ldply(., data.frame)




