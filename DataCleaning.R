#################################################################################
#  DataCleaning.R                                                               # 
#  VAHCS - Data Cleaning                                                        #
#################################################################################
# Exposure:     thcw2to6any  
# Outcome:      cmdtot7      
# Confounders:  alchirsk2to6any  asb2to6any  par_div  par_inchs  cmd2to6any  
# Auxiliary:    age_2

library(haven)
library(tidyr)
library(magrittr) 
library('tableone')

# Note: For ethical reasons, the original raw data set is not allowed to be public. Please contact the author for further information.
dat <- read_dta("VAHCS.Raw.dta") 

# Revise alchirsk2to6any and cmd2to6any; keep relative variables; keep female records; 
# drop missing values in thcw2to6any, asb2to6any and par_div
dat[dat[, "alchirsk2to6any"] == 0, "alchirsk2to6any"] <- apply(dat[dat[, "alchirsk2to6any"] == 0, 
                                                                   c("alc_hirisk2","alc_hirisk3","alc_hirisk4","alc_hirisk5","alc_hirisk6")], 1, sum) #alc
dat[dat[, "cmd2to6any"] == 0, "cmd2to6any"] <- apply(dat[dat[, "cmd2to6any"] == 0, c("cis2_2","cis2_3","cis2_4","cis2_5","cis2_6")], 1, sum) #cmd
dat[is.na(dat[, "thcw2to6any"]), "thcw2to6any"] <- 0
dat[dat[, "thcw2to6any"] == 0, "thcw2to6any"] <- apply(dat[dat[, "thcw2to6any"] == 0, c("thcw2","thcw3","thcw4","thcw5","thcw6")], 1, sum) #thc
dat <- dat %>% subset.data.frame(., sex == 1) %>% subset.data.frame(., complete.cases(asb2to6any, par_div, par_inchs)) %>%
  .[,c("age_2", "par_inchs", "par_div", "asb2to6any", "cmd2to6any", "alchirsk2to6any", "thcw2to6any", "cmdtot7")] 
summary(dat)

# Standarise the log-transformed age_2 and cmdtot7
dat$age_2 <- log(dat$age_2) %>% `-`(mean(., na.rm = T)) %>% `/`(sd(., na.rm = T))
dat$zcmd <- log(dat$cmdtot7+1) %>% `-`(mean(., na.rm = T)) %>% `/`(sd(., na.rm = T))

dat <- dat[,-8]
dat[2:7] <- lapply(dat[2:7], as.factor)
names(dat)[1:8] <- c("age","pari","pard","asb","cmd","alc","thc","mhs")
dat <- as.data.frame(dat)
save(dat, file = "VAHCS.CleanData.Rda")

# Table One
factorVars <- c("alc", "asb", "cmd", "pard", "pari")
vars <- c("age", "pari", "pard", "asb", "cmd", "alc", "mhs")
tableOne <- CreateTableOne(vars = vars, strata = "thc", data = dat, factorVars = factorVars, includeNA = F, test = F)
# Produce Table 1 of the manuscript 
# Columns: item & statistics in exposed and unexposed group & missingness proportion
# Rows: number of observations in exposure; mean(SD) of age; number(%) of parental education;
# number(%) of parental divorce; number(%) of antisocial behavior; number(%) of adolescent depression and anxiety;
# number(%) of alcohol use; mean(SD) of adulthood mental health score
VAHCS.table1 <- print(tableOne, missing = T, explain = T, printToggle = T)
write.csv(VAHCS.table1, file = "VAHCS-Table1.csv")
