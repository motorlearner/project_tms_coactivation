# Assessing muscle co-activation dynamics by counting motor-evoked potentials

This repository contains all the data and code underlying my research master project at Vrije University Amsterdam.

The data was collected by Fang Jin. Please see:

> __A new protocol for multiple muscle mapping using nTMS__  
> Fang Jin, Sjoerd Bruijn, Andreas Daffertshofer  
> bioRxiv 2021.07.29.454279; doi: <https://doi.org/10.1101/2021.07.29.45427>

## How to run

Fork/clone the repository, then simply run R scripts `plots.R` and `stats.R` to reproduce plots and statistical analysis; `simulation.R` and `simulation_pmi_n.R` to run the simulations. 

### data

There is one csv file per subject. Each row corresponds to a new TMS pulse. Columns 1-8 hold the binary responses for each muscle. Column 9 identifies the stimulation protocol (i.e. which muscle's RMT was used to set the stimulation intensity).

### R

There are six R scripts.

- `theme.R` creates a custom ggplot2 theme for plotting. This script will be called by other scripts. 
- `getdata_binary.R` extracts the relevant information from the binary data. This script will be called by other scripts.
- `plots.R` creates and saves plots from the data. 
- `stats.R` performs the statistical analysis on the data. 
- `simulation.R` runs the stimulation, and creates and saves corresponding plots. 
- `simulation_pmi_n.R` runs the supplementary simulation, and creates and saves the corresponding plot. 

### plots

This is where the plots are saved. 

## Session Info

R version 4.1.2 (2021-11-01)  
Platform: x86_64-w64-mingw32/x64 (64-bit)  
Running under: Windows 10 x64 (build 19042)  

Matrix products: default  

locale:  
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252  
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    

attached base packages:  
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:  
 [1] rcompanion_2.4.13 PMCMRplus_1.9.3   rstatix_0.7.0     patchwork_1.1.1   ggridges_0.5.3    forcats_0.5.1     stringr_1.4.0     dplyr_1.0.7      
 [9] purrr_0.3.4       readr_2.1.1       tidyr_1.1.4       tibble_3.1.6      ggplot2_3.3.5     tidyverse_1.3.1   here_1.0.1       

loaded via a namespace (and not attached):  
 [1] matrixStats_0.61.0   fs_1.5.2             lubridate_1.8.0      httr_1.4.2           rprojroot_2.0.2      tools_4.1.2         
 [7] backports_1.4.1      utf8_1.2.2           R6_2.5.1             nortest_1.0-4        DBI_1.1.2            colorspace_2.0-2    
[13] ggdist_3.1.1         withr_2.4.3          tidyselect_1.1.2     Exact_3.1            compiler_4.1.2       cli_3.1.1           
[19] rvest_1.0.2          expm_0.999-6         xml2_1.3.3           sandwich_3.0-1       BWStest_0.2.2        scales_1.1.1        
[25] lmtest_0.9-39        mvtnorm_1.1-3        proxy_0.4-26         multcompView_0.1-8   pkgconfig_2.0.3      fastmap_1.1.0       
[31] dbplyr_2.1.1         rlang_1.0.1          readxl_1.3.1         rstudioapi_0.13      SuppDists_1.1-9.7    farver_2.1.0        
[37] generics_0.1.2       zoo_1.8-9            jsonlite_1.7.3       car_3.0-12           distributional_0.3.0 magrittr_2.0.1      
[43] modeltools_0.2-23    Matrix_1.3-4         Rcpp_1.0.8           DescTools_0.99.44    munsell_0.5.0        fansi_1.0.2         
[49] abind_1.4-5          lifecycle_1.0.1      stringi_1.7.6        multcomp_1.4-18      carData_3.0-5        MASS_7.3-54         
[55] rootSolve_1.8.2.3    plyr_1.8.6           grid_4.1.2           parallel_4.1.2       crayon_1.5.0         lmom_2.8            
[61] lattice_0.20-45      haven_2.4.3          splines_4.1.2        hms_1.1.1            pillar_1.7.0         boot_1.3-28         
[67] gld_2.6.4            kSamples_1.2-9       codetools_0.2-18     stats4_4.1.2         reprex_2.0.1         glue_1.6.0          
[73] data.table_1.14.2    modelr_0.1.8         vctrs_0.3.8          tzdb_0.2.0           cellranger_1.1.0     gtable_0.3.0        
[79] assertthat_0.2.1     cachem_1.0.6         coin_1.4-2           libcoin_1.0-9        broom_0.7.12         Rmpfr_0.8-7         
[85] e1071_1.7-9          class_7.3-19         survival_3.2-13      memoise_2.0.1        gmp_0.6-4            TH.data_1.1-0       
[91] ellipsis_0.3.2
