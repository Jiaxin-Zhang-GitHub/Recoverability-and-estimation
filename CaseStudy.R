#################################################################################
#  CaseStudy.R                                                                  # 
#  VAHCS - Case Study                                                           #
#################################################################################
library(magrittr) 
library(plyr)
library(stringr)

load("VAHCS.CleanData.Rda")
colnames(dat) <- c("A","C1","C2","C3","C4","C5","X","Y")
set.seed(1957427)
source(file = "Methods.R")
boot.R = 240; m = 100; maxit = 10; Missingness = NA; Outcome = 4
# Overwrite SMC approach for case study 
MI.SMC <- function(dat) {
  Outcome = dat[1,10]; Missingness = dat[1,9]; dat <- dat[,1:8]; smformula <- paste(analysis[Outcome][[1]][2],"+ A")
  smcfcs(dat, smtype = "lm", smformula = smformula, method = c("norm","","","","logreg","logreg","logreg",""), m = m, numit = maxit) %>%
    MI.pool(., Outcome, Missingness, Method = "MI.SMC")
}

# Check regression details in the complete-case analysis 
summary(eval(analysis[4][[1]], dat))

# Diagnostic check for MI 
MI.imp <- mice(data = dat, m = m, maxit = maxit, method = c("norm","","","","logreg","logreg","logreg","norm"), vis = "monotone", print = F) 
summary(complete(MI.imp)) 
plot(MI.imp)
stripplot(MI.imp)

smformula <- paste(analysis[Outcome][[1]][2],"+ A")
SMC.imp <- smcfcs(dat, smtype = "lm", smformula = smformula, method = c("norm","","","","logreg","logreg","logreg",""), m = m, numit = maxit)
plot(SMC.imp)

# Case study results
set.seed(1957427)
dat <- cbind(dat, Missingness, Outcome)
case <- rbind(cbind(gcomp(dat),method="CCA"), MI.Sim(dat), MI.CART(dat), MI.RF(dat), MI.EC(dat), MI.Com(dat), MI.SMC(dat))[,-c(6,7)] 
# Produce Table 6 of the manuscript
# Columns: estimate (of the ACE) & std.error & p-value & 95% confidence interval (low up) & missing data approaches
# Rows: results form each method: complete-case analysis (CCA); simple multiple imputation (MI.Sim); 
# MI using classification and regression trees (MI.CART); MI using random forest (MI.RF);
# exposure-confounder MI (MI.EC); approximately compatible MI (MI.Com); substantive model compatible MI (MI.SMC)
case
save(case, file="Case.Rda")

