README

Code to perform case and simulation study

Data simulation code for manuscript:
Title: "Recoverability and estimation of causal effects under typical multivariable missingness mechanisms".

Authors: Jiaxin Zhang, S.Ghazaleh Dashti, John B. Carlin, Katherine J. Lee and Margarita Moreno-Betancur

Code was written by Jiaxin Zhang
In case of questions or comments, please contact jiaxin.zhang@mcri.edu.au

This repository contains the following data and files that can be used to reproduce all results in the manuscript.

-DataCleaning.R
Data Cleaning for the original data from the Victorian Adolescent Health Cohort Study (VAHCS). Note: For ethical reasons, the original raw data set is not allowed to be public. Please contact the author for further information. The final dataset used for the case and simulation study is saved in data/VAHCS.CleanData.Rda. The data description and summarised statistics were saved in data/VAHCS-Table1.csv and correspond to Table 1 in the manuscript. 

-CaseStudy.R
Conduct the case study. The results are saved in results/Case.Rda and correspond to Table 6 in the manuscript.

-Source.R
The functions used for data generation in code/Simu.control.R.

-Simu.control.R
Control parameters for the simulation. In detail:
Line 1-96: Save regression coefficients (data/coef.VAHCS.Rda) and missingness coefficients (data/coef.M.Rda) 
Line 99-137: Adjust confounders and exposure prevalence (data/coef.X.Rda)
Line 140-190: Adjust coefficients in the outcome generation (data/coef.X.Rda)
Line 193-258: Adjust missingness proportion (data/coef.M.Rda)
Line 261-276: Find out the appropriate sample sizes (data/sample.size.Rda)
Line 279-298: Find out the number of bootstrapping 
Line 301-336: Generate incomplete data for missingness methods (e.g. data/data.50%.C.Rda and results/res.50%.C.Rda)
Please note the generated incomplete data, like data.50%.C.Rda, is a large dataset and results in difficulties in sharing data and code for reproducing checks. Therefore, the shared files exclude such datasets. But please run the commands in Line 301-336 to generate and save the dataset in the data fold for the later call in the Simu.R.

-Methods.R
Functions for handling missing data and conducting analyses that are used in case and simulation study. 

-Simu.R
Conduct simulation study over HPC. Save the simulation results in, e.g., results/res.50%.C.Rda.
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
Summarise and plot the simulation results (results/res.10%.Rda and results/res.50%.Rda), corresponding to Figures 3-5 in the manuscript.

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
