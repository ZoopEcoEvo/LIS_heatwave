library(rmarkdown)
library(knitr)
library(readxl)
library(rTPC)
library(nls.multstart)
library(broom)
library(MASS)
library(minpack.lm)
library(dabestr)
library(boot)
library(car)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(tidyverse)

process_data = F
generate_figures = F
knit_manuscript = T

#### Process Data ####
if(process_data == T){
  #Field Data
  h1_epr <- read_excel("Data/Hyp1_tonsa_epr.xlsx")
  surv = read_excel("Data/Hyp1_tonsa_surv.xlsx")
  
  huds_h1_epr <- read_excel("Data/Hyp1_hudsonica_epr.xlsx")
  huds_surv = read_excel("Data/Hyp1_hudsonica_surv.xlsx")
  
  #Simulated Heatwave Data
  F0_epr <- read_excel("Data/F0_EPR.xlsx")
  F0_fbs <- read_excel("Data/F0_female_size.xlsx")
  
  #Transgenerational Effects
  F1_fbs <- read_excel("Data/F1_sizes.xlsx")
  F1_epr <- read_excel("Data/F1_EPR.xlsx")
  
  source(file = "Scripts/01_data_analysis.R")
}

#### Generate Figures ####
if(generate_figures == T){
  #Field Data
  h1_epr <- read_excel("Data/Hyp1_tonsa_epr.xlsx")
  surv = read_excel("Data/Hyp1_tonsa_surv.xlsx")
  
  huds_h1_epr <- read_excel("Data/Hyp1_hudsonica_epr.xlsx")
  huds_surv = read_excel("Data/Hyp1_hudsonica_surv.xlsx")
  
  #Simulated Heatwave Data
  F0_epr <- read_excel("Data/F0_EPR.xlsx")
  F1_epr = read_excel("Data/F1_EPR.xlsx")
  F1_fbs = read_excel("Data/F1_sizes.xlsx")
  
  #Transgenerational Effects
  file_list = dir(path = "Output/Data/")
  for(i in 1:length(file_list)){
    string = file_list[i]
    var_name = str_split(string = string, pattern = "[.]")[[1]]
    
    if(var_name[2] == "csv"){
      assign(var_name[1],
             read.csv(paste("Output/Data/", string, sep = "")))
    }else{
      assign(var_name[1],
             readRDS(paste("Output/Data/", string, sep = "")))
    }
  }
  
  render(input = "Output/Reports/project_figures.Rmd", #Input the path to your .Rmd file here
         output_file = "project_figures", #Name your figure summary file here; as it is, report name includes the date
         output_format = "all") # NOTE: This will render the document once per output specified in the YAML
                                # As a result, this can take a long time, depending on the complexity of the .Rmd
}

#### Prepare Manuscript ####
if(knit_manuscript == T){
  render(input = "Manuscript/Sasaki_et_al_2023.Rmd", #Input the path to your .Rmd file here
         output_file = paste("draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create drafts specified with the date
         output_format = "all",
         output_dir = "Output/Drafts/",
         clean = TRUE) #Set the path to the desired output directory here
}

