README

Code to perform case and simulation study

Data simulation code for manuscript:
Title: "Recoverability and estimation of causal effects under typical multivariable missingness mechanisms".

Authors: Jiaxin Zhang, S.Ghazaleh Dashti, John B. Carlin, Katherine J. Lee and Margarita Moreno-Betancur

Code was written by Jiaxin Zhang
In case of questions or comments please contact jiaxin.zhang@mcri.edu.au

This repository contains the following data and files that can be used to reproduce all results of the manuscrtipt.

-DataCleaning.R
Data Cleaning for the original data from the Victorian Adolescent Health Cohort Study (VAHCS). Note: For ethical reasons, the original raw data set is not allowed to be public. Please contact the author for further information. The final dataset used for case and simultaion study is save as VAHCS.CleanData.Rda, which was summarised as VAHCS-Table1.csv and corresponds to Table 1 in the manuscript. 

-CaseStudy.R
Conduct the case study. The results are save as Case.Rda and corresponds to Table 6 in the manuscript.

-Source.R
The functions used for data generation in Simu.control.R.

-Simu.control.R
Control parameterics for simulation. In detail:
Line 1-96: Save regression coefficients (coef.VAHCS.Rda) and missingness coefficients (coef.M.Rda) 
Line 97-137: Adjust confoudners and exposure prevalence (coef.X.Rda)
Line 148-190: Adjust coefficients in the outcome generation (coef.X.Rda)
Line 191-256: Adjust missingness proportion (coef.M.Rda)
Line 257-274: Find out the appropriate sample sizes (sample.size.Rda)
Line 275-296: Find out the number of bootstrapping 
Line 297-330: Generate incomplete data for missingness methods (e.g. data.50%.C.Rda and res.50%.C.Rda)

-Methods.R
Funtions for handling missing data and conduting analysis that used in case and simution study. 

-Simu.R
Conduct simulation study over HPC. Save the simulation results in, e.g., res.50%.C.Rda.
Please use the Simu.pbs to submit Simu.R. The replications are:
for exposure in 10% 50%
do
for mDAG in A B C D E F G H I J
do
for method in `seq 1 7 `
do
for scenario in `seq 1 30`
do
qsub -v exposure=$exposure,mDAG=$mDAG,method=$method,scenario=$scenario simu.pbs
done
done
done
done

-Plot.R
Summarise and plot the simulation results (res.10%.Rda and res.50%.Rda), corresponds to Figures 3-5 in the manuscript.

-dosearch.R
Investigate recoverability using `dosearch' package.


The session information in R is as below:
> sessionInfo()
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS:   /hpc/software/installed/R/4.1.2/lib64/R/lib/libRblas.so
LAPACK: /hpc/software/installed/R/4.1.2/lib64/R/lib/libRlapack.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] grid      parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] scales_1.2.0       gtable_0.3.0       RColorBrewer_1.1-3 ggsci_2.9          ggthemes_4.2.4     ggpubr_0.4.0      
 [7] smcfcs_1.6.1       mefa_3.2-8         purrr_0.3.4        dplyr_1.0.8        car_3.0-12         carData_3.0-5     
[13] mice_3.14.0        MASS_7.3-56        nnet_7.3-17        boot_1.3-28        ggrepel_0.9.1      ggplot2_3.4.1     
[19] dosearch_1.0.8     dagitty_0.3-1      stringr_1.4.0      plyr_1.8.7         tableone_0.13.2    magrittr_2.0.3    
[25] tidyr_1.2.0        haven_2.4.3       
