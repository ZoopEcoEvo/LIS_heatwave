library(rmarkdown)
library(knitr)
library(readxl)
library(rTPC)
library(nls.multstart)
library(broom)
library(boot)
library(MASS)
library(minpack.lm)
library(dabestr)
library(car)
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
         output_format = "github_document") # NOTE: This will render the document once per output specified in the YAML
                                # As a result, this can take a long time, depending on the complexity of the .Rmd
}

#### Prepare Manuscript ####
if(knit_manuscript == T){
  comb_params = read.csv("Output/Data/comb_params.csv")
  
  par_table = comb_params %>%  
    filter(term != "a") %>% 
    mutate(estimate = round(estimate, digits = 1),
           conf_lower = round(conf_lower, digits = 1),
           conf_upper = round(conf_upper, digits = 1),
           tab_entry = paste(estimate, " [", conf_lower, " - ", conf_upper, "]", sep = "")) %>%  
    pivot_wider(id_cols = c(curve_id, species),names_from = c(metric, term), values_from = estimate) %>% 
    mutate(species = if_else(species == "tonsa", "*A. tonsa*", "*A. hudsonica*"),
           curve_id = fct_relevel(curve_id, "January", "February", "March", "April", "May", "June", 
                                  "July", "August", "September", "October", "November_1", "November_2")) %>% 
    select(-EPR_rmax, -HF_rmax) %>% 
    arrange(curve_id)
  
  colnames(par_table) = c("Collection", "Species", "EPR Topt", 
                          "HS Topt",
                          "Prod. Rmax", "Prod. Topt")

  # colnames(par_table) = c("Collection", "Species", "Max EPR Rate", "EPR Topt", 
  #                         "Max HS", "HS Topt",
  #                         "Max Offspring Production", "Production Topt")
  # 
  # write.csv(par_table, file = "Output/Data/par_table.csv")
  
  combined_tolerance = read.csv("Output/Data/combined_tolerance.csv")
  
  render(input = "Manuscript/Sasaki_et_al_2023.Rmd", #Input the path to your .Rmd file here
         output_file = "Sasaki_etal_2023_heatwaves", #Name your file here; as it is, this line will create drafts specified with the date
         output_format = "all",
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         clean = TRUE) 
}

