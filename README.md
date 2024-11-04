# Seasonally variable thermal performance curves prevent adverse effects of heatwaves

M.C. Sasaki<sup>1,2</sup>, M. Finiguerra<sup>3</sup>, H.G. Dam<sup>1</sup> 

1. University of Connecticut, Department of Marine Sciences  
2. University of Vermont, Department of Biology  
3. University of Connecticut, Ecology and Evolutionary Biology Department  

This project examined how seasonal variation in thermal performance curves affects vulnerability to heatwaves in the estuarine copepods *Acartia tonsa* and *Acartia hudsonica*. Copepods were collected from Eastern Long Island Sound on roughly monthly intervals during 2014 and 2015. 

<a href="https://doi.org/10.1101/2023.05.09.540050"><img src="https://img.shields.io/badge/Preprint%20Here-Here?style=for-the-badge&labelColor=grey&color=6F92B8"/></a>    <a href="https://doi.org/10.5281/zenodo.13988768"><img src="https://img.shields.io/badge/Dataset%20Here-Here?style=for-the-badge&labelColor=grey&color=EAB221"/></a>

## Directory Structure 
The root directory contains the README and Licensing files, along with a .Rproj file and four sub-directories: Data, Manuscript, Output, and Scripts.  

-   `Data/` contains the raw data used in this analysis.  

-   `Manuscript/` contains the R Markdown file, templates, and bibliography used to produce the manuscript draft. 

-   `Output/` contains the various products of the project (processed data, figures, knit reports, and a PDF copy of the manuscript. Note, the `Reports/` directory contains the R Markdown file used to generate the figures used in the manuscript.  

-   `Scripts/` contains two R scripts. 
    -   `01_Data_analysis.R` is used to process the raw data. The two primary components are calculating the thermal performance curve parameters from the field observations and estimating the effect size comparisons for the simulated heatwave experiments. 
    -   `02_make_report.R` is use to control the project workflow. Through this script, you can choose to run the process the data, make the figures, or knit the manuscript. This script should be used rather than running isolated fragments individually. 


## Data Description 

The `Data/` directory contains the eight data sets used in this study.  

-   `F0_EPR.xlsx` contains data from egg production experiments during the simulated heatwave experiments for the field-collected females.   
    -   *Month* - The month copepods were collected	  
    -   *Treatment*	- Which treatment copepods were exposed to (heatwave or control)  
    -   *Temperature* - The temperature copepods were exposed to (12, 17, 22, or 27) 
    -   *Female* - ID within each treatment (from 1 to 40)  
    -   *Day*	- The time period measurements were from (Days 1 to 3 or Days 5 to 7). This represents short and long heatwaves, respectively.
    -   *Hatched*	- The number of hatched eggs counted 
    -   *Unhatched*	- The number of unhatched eggs counted
    -   *Total* - The total number of eggs produced (hatched and unhatched combined)
    -   *Success* - The hatching success (number of hatched eggs divided by the total number of eggs)
    

-   `F0_Female_size.xlsx` contains body size data for the field-collected females used in the simulated heatwave experiments.   
    -   *Month* - The month copepods were collected	  
    -   *Treatment*	- Which treatment copepods were exposed to (heatwave or control)  
    -   *Temperature* - The temperature copepods were exposed to (12, 17, 22, or 27) 
    -   *Number* - ID within each treatment (from 1 to 40)  
    -   *Size*	- The prosome length (in mm)
        

-   `F1_EPR.xlsx` contains data from egg production experiments during the simulated heatwave experiments for the F1 generation.   
    -   *Month* - The month copepods were collected	  
    -   *Parental_treatment*	- Which treatment the offspring's parents were exposed to (heatwave or control)  
    -   *Parental_temperature* - The temperature parental copepods were exposed to (12, 17, 22, or 27) 
    -   *Offspring_temperature* - The temperature offspring were reared at (12, 17, or 22)
    -   *Female* - ID within each treatment (from 1 to 40)  
    -   *Day*	- The time period eggs were collected from the parental copepods (Days 1 to 3 or Days 5 to 7). This represents short and long heatwaves, respectively. 
    -   *Hatched*	- The number of hatched eggs counted 
    -   *Unhatched*	- The number of unhatched eggs counted
    -   *Total* - The total number of eggs produced (hatched and unhatched combined)
    -   *Success* - The hatching success (number of hatched eggs divided by the total number of eggs)
        

-   `F0_sizes.xlsx` contains data from egg production experiments during the simulated heatwave experiments for the F1 generation.   
    -   *Month* - The month copepods were collected	  
    -   *Parental_treatment*	- Which treatment the offspring's parents were exposed to (heatwave or control)  
    -   *Parental_temperature* - The temperature parental copepods were exposed to (12, 17, 22, or 27) 
    -   *Day*	- The time period eggs were collected from the parental copepods (Days 1 to 3 or Days 5 to 7). This represents short and long heatwaves, respectively. 
    -   *Offspring_temperature* - The temperature offspring were reared at (12, 17, or 22)
    -   *Number* - ID within each treatment (from 1 to 40)  
    -   *Size*	- The prosome length (in mm)  
        

-   `Hyp1_hudsonica_epr.xlsx` contains data from egg production experiments for field-collected *Acartia hudsonica*.   
    -   *Month* - The month copepods were collected	  
    -   *Coll_temp*	- The temperature measured at the time of collection  
    -   *Temp* - The temperature egg production assays were performed at
    -   *EPR* - The total number of eggs produced (hatched and unhatched combined)
    -   *HF* - The hatching success (number of hatched eggs divided by the total number of eggs)
    -   *RF* - The relative fecundity (referred to as offspring production in the manuscript) calculated as the EPR x the HF.
        

-   `Hyp1_hudsonica_surv.xlsx` contains temperature survivorship data for field-collected *Acartia hudsonica*.   
    -   *Month* - The month copepods were collected	  
    -   *Coll_temp*	- The temperature measured at the time of collection  
    -   *Temp* - The static stress temperature each individual was exposed to
    -   *Surv* - Whether the individual survived a 24-hour exposure to the temperature (0 or 1, died or survived, respectively)
        

-   `Hyp1_tonsa_epr.xlsx` contains data from egg production experiments for field-collected *Acartia tonsa*.   
    -   *Month* - The month copepods were collected	  
    -   *Coll_temp*	- The temperature measured at the time of collection  
    -   *Temp* - The temperature egg production assays were performed at
    -   *EPR* - The total number of eggs produced (hatched and unhatched combined)
    -   *HF* - The hatching success (number of hatched eggs divided by the total number of eggs)
    -   *RF* - The relative fecundity (referred to as offspring production in the manuscript) calculated as the EPR x the HF.
        

-   `Hyp1_tonsa_surv.xlsx` contains temperature survivorship data for field-collected *Acartia tonsa*.   
    -   *Month* - The month copepods were collected	  
    -   *Coll_temp*	- The temperature measured at the time of collection  
    -   *Temp* - The static stress temperature each individual was exposed to
    -   *Surv* - Whether the individual survived a 24-hour exposure to the temperature (0 or 1, died or survived, respectively)


## Workflow

The workflow is operated via the 02_Make_report.R script in the Scripts directory. It is not recommended that you run analyses or knit documents from the files themselves as the file paths are internally set and may not be correct otherwise. At the top of this script, you are able to indicate whether:

1. The raw data should be processed to generate thermal performance curves and estimate effect sizes for the simulated heatwave experiments.  

2. The summary file (located in the Output/Reports directory) should be knit. This markdown file will generate the figures used in the manuscript, as well as an HTML and a GitHub flavored markdown document.

3. The manuscript file (located in the Manuscripts directory) should be knit. This markdown file will produce formatted PDF and word document versions of the manuscript. 


## Versioning   

R version 4.2.2 (2022-10-31)  

Platform: x86_64-apple-darwin17.0 (64-bit)  

Running under: macOS Ventura 13.3.1 
  
**Attached base packages:** stats, graphics, grDevices, utils, datasets, methods, base     

**Other attached packages:** forcats_0.5.2, stringr_1.5.0, dplyr_1.0.10, purrr_1.0.0, readr_2.1.3, tidyr_1.2.1, tibble_3.1.8, tidyverse_1.3.2, ggrepel_0.9.2, ggpubr_0.5.0, ggplot2_3.4.0, car_3.1-1, carData_3.0-5, dabestr_0.3.0, magrittr_2.0.3, minpack.lm_1.2-2, MASS_7.3-58.1, boot_1.3-28, broom_1.0.2, nls.multstart_1.2.0, rTPC_1.0.2, readxl_1.4.1, knitr_1.41, rmarkdown_2.20, Rcpp_1.0.10, lubridate_1.9.0, assertthat_0.2.1, digest_0.6.31, utf8_1.2.2, R6_2.5.1, cellranger_1.1.0, backports_1.4.1, reprex_2.0.2, evaluate_0.19, httr_1.4.4, pillar_1.8.1, rlang_1.0.6, googlesheets4_1.0.1, rstudioapi_0.14, googledrive_2.0.0, munsell_0.5.0, compiler_4.2.2, modelr_0.1.10, xfun_0.36, pkgconfig_2.0.3, htmltools_0.5.4, tidyselect_1.2.0, fansi_1.0.3, crayon_1.5.2, tzdb_0.3.0, dbplyr_2.2.1, withr_2.5.0, grid_4.2.2, jsonlite_1.8.4, gtable_0.3.1, lifecycle_1.0.3, DBI_1.1.3, scales_1.2.1, cli_3.5.0, stringi_1.7.8, ggsignif_0.6.4, fs_1.5.2, xml2_1.3.3, ellipsis_0.3.2, generics_0.1.3, vctrs_0.5.1, tools_4.2.2, glue_1.6.2, hms_1.1.2, rsconnect_0.8.28, abind_1.4-5, fastmap_1.1.0, timechange_0.1.1, colorspace_2.0-3, gargle_1.2.1, rvest_1.0.3, rstatix_0.7.1, haven_2.5.1


## Funding

This study was funded by grants from Connecticut Sea Grant (R/LR-25) and the National Science Foundation (OCE-1947965) awarded to H.G.D., and a NSF postdoctoral fellowship (OCE-2205848) awarded to M.C.S.
