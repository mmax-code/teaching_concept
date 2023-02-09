# Raising awareness of uncertain choices in empirical data analysis: A teaching concept towards replicable research practices

This repository contains materials to reproduce the results of the manuscript "Raising awareness of uncertain choices in empirical data analysis: A teaching concept towards replicable research practices". 

# Code and Data

Results.R: Code to reproduce the t-test, Wilcoxon test results and figures from Section 3. 

Model_Uncertainty.R: Code to reproduce the VoE plots from the supplementary materials. 

Simulation.R: This file contains the code to simulate the data handed out to the students. As we wanted to avoid collaboration of the students during the assessment, we created four different groups with slightly different underlying true effects. Within each group some of the other regression coefficients are changed slightly, too. 

data.txt: Data containing the true underlying effect, estimates from Phase I and II, and a grade and sex dummy.  


# Reproducibility

To reproduce the main results, please adjust the working directory accordingly and run Results.R 
To reproduce the VoE plots from the supplementary material, please run Model_Uncertainty.R 

Note:
Model_Uncertainty.R creates the simulated dataset on its own again at the beginning of the script and doesn't need any data input.

# Session Info

R version 4.0.4 (2021-02-15)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 11 (bullseye)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] viridis_0.5.1        viridisLite_0.3.0    ggpointdensity_0.1.0 dplyr_1.0.8         
 [5] broom_0.8.0          fastDummies_1.6.3    mvtnorm_1.1-1        rlist_0.4.6.1       
 [9] reshape2_1.4.4       ggplot2_3.3.3       

