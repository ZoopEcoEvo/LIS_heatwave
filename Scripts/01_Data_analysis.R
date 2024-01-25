#This is the data analysis for Sasaki et al. - "Seasonally variable thermal performance curves prevent adverse effects of heatwaves"
#Code should be run via the project 'Heatwave.RProj'

#### Estimating Field LD50 Values ####
### Acartia tonsa ###
tonsa_tolerance = surv %>% 
  group_by(Month, Coll_temp) %>% #Breaks data down into experimental groups (populations, collections, dev. temps)
  group_modify(~ data.frame( #For each of these groups, store these values in a data frame
    "LD50" = unclass( #Use a glm to estimate TPCs and then use those TPCs to estimate LD50
      dose.p(p = 0.5, 
             glm(data = .x, Surv ~ Temp, family = binomial(link = "logit")))),
    "SE" = attr( #What is the SE of the LD50 estimate
      dose.p(p = 0.5,
             glm(data = .x, Surv ~ Temp, family=binomial(link = "logit"))), 
      "SE"))) %>% 
  mutate("species" = "tonsa", 
         "margin" = LD50 - Coll_temp)

huds_tolerance = huds_surv %>% 
  group_by(Month, Coll_temp) %>% #Breaks data down into experimental groups (populations, collections, dev. temps)
  group_modify(~ data.frame( #For each of these groups, store these values in a data frame
    "LD50" = unclass( #Use a glm to estimate TPCs and then use those TPCs to estimate LD50
      dose.p(p = 0.5, 
             glm(data = .x, Surv ~ Temp, family = binomial(link = "logit")))),
    "SE" = attr( #What is the SE of the LD50 estimate
      dose.p(p = 0.5,
             glm(data = .x, Surv ~ Temp, family=binomial(link = "logit"))), 
      "SE"))) %>% 
  mutate("species" = "hudsonica", 
         "margin" = LD50 - Coll_temp)

combined_tolerance = bind_rows(tonsa_tolerance, huds_tolerance)
write.csv(combined_tolerance, file = "Output/Data/combined_tolerance.csv", row.names = F)

#### Field Thermal Performance Curve Parameters ####
### Acartia hudsonica ###
tonsa_d = h1_epr %>%
  dplyr::select("curve_id" = Month, "growth_temp" = Coll_temp,  "temp" = Temp, EPR, HF, RF) %>% 
  pivot_longer(cols = c("EPR", "HF", "RF"),
               values_to = "rate", 
               names_to = "metric") %>% 
  drop_na() 


# fit two chosen model formulation in rTPC
tonsa_d_fits <- nest(tonsa_d, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)))

# create new list column of for high resolution data
tonsa_d_preds <- mutate(tonsa_d_fits, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  # get rid of original data column
  dplyr::select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(gaussian)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  dplyr::select(curve_id, metric, model_name, preds) %>%
  # unlist the preds list column
  unnest(preds)

tonsa_d_fits <- mutate(tonsa_d_fits, coefs = map(gaussian, coef))

tonsa_d_fits <- mutate(tonsa_d_fits, nls_fit = map2(data, coefs, ~nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                                                                        data = .x,
                                                                        start = .y,
                                                                        lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                                        upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'))))

tonsa_d_fits <- mutate(tonsa_d_fits, bootstrap = list(rep(NA, n())))

for(i in 1:nrow(tonsa_d_fits)){
  temp_data <- tonsa_d_fits$data[[i]]
  temp_fit <- nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                    data = temp_data,
                    start = tonsa_d_fits$coefs[[i]],
                    lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'),
                    upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'))
  boot <- Boot(temp_fit, method = 'residual')
  tonsa_d_fits$bootstrap[[i]] <- boot
  rm(list = c('temp_fit', 'temp_data', 'boot'))
}

# get the raw values of each bootstrap
tonsa_d_fits <- mutate(tonsa_d_fits, output_boot = map(bootstrap, function(x) x$t))

# calculate predictions with a gnarly written function
tonsa_d_fits <- mutate(tonsa_d_fits, preds = map2(output_boot, data, function(x, y){
  temp <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = gaussian_1987(temp, rmax, topt, a))
  return(temp)
}))

# select, unnest and calculate 95% CIs of predictions
tonsa_boot_conf_preds <- dplyr::select(tonsa_d_fits, curve_id, metric, preds) %>%
  unnest(preds) %>%
  group_by(curve_id, metric, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            .groups = 'drop')

# get tidied parameters using broom::tidy
# get confidence intervals of parameters
tonsa_cis = mutate(tonsa_d_fits[c(1:16),], params = map(nls_fit, broom::tidy),
                   cis = map(bootstrap, function(x){
                     temp <- confint(x, method = 'bca') %>%
                       as.data.frame() %>%
                       rename(conf_lower = 1, conf_upper = 2) %>%
                       rownames_to_column(., var = 'term')
                     return(temp)
                   }))

late_nov_tonsa = tonsa_d_fits[c(17,18),]
late_nov_params = mutate(late_nov_tonsa, params = map(nls_fit, broom::tidy))

tonsa_d_fits = bind_rows(tonsa_cis, late_nov_params)

### Acartia hudsonica ###
huds_d = huds_h1_epr %>%
  dplyr::select("curve_id" = Month, "growth_temp" = Coll_temp,  "temp" = Temp, EPR, HF, RF) %>% 
  pivot_longer(cols = c("EPR", "HF", "RF"),
               values_to = "rate", 
               names_to = "metric") %>% 
  drop_na() 


# fit two chosen model formulation in rTPC
huds_d_fits <- nest(huds_d, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)))

# create new list column of for high resolution data
huds_d_preds <- mutate(huds_d_fits, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  # get rid of original data column
  dplyr::select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(gaussian)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  dplyr::select(curve_id, metric, model_name, preds) %>%
  # unlist the preds list column
  unnest(preds)

huds_d_fits <- mutate(huds_d_fits, coefs = map(gaussian, coef))

huds_d_fits <- mutate(huds_d_fits, nls_fit = map2(data, coefs, ~nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                                                                      data = .x,
                                                                      start = .y,
                                                                      lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                                                      upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'))))

huds_d_fits <- mutate(huds_d_fits, bootstrap = list(rep(NA, n())))

for(i in 1:nrow(huds_d_fits)){
  temp_data <- huds_d_fits$data[[i]]
  temp_fit <- nlsLM(rate ~ gaussian_1987(temp, rmax, topt, a),
                    data = temp_data,
                    start = huds_d_fits$coefs[[i]],
                    lower = get_lower_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'),
                    upper = get_upper_lims(temp_data$temp, temp_data$rate, model_name = 'gaussian_1987'))
  boot <- Boot(temp_fit, method = 'residual')
  huds_d_fits$bootstrap[[i]] <- boot
  rm(list = c('temp_fit', 'temp_data', 'boot'))
}


# get the raw values of each bootstrap
huds_d_fits <- mutate(huds_d_fits, output_boot = map(bootstrap, function(x) x$t))

# calculate predictions with a gnarly written function
huds_d_fits <- mutate(huds_d_fits, preds = map2(output_boot, data, function(x, y){
  temp <- as.data.frame(x) %>%
    drop_na() %>%
    mutate(iter = 1:n()) %>%
    group_by_all() %>%
    do(data.frame(temp = seq(min(y$temp), max(y$temp), length.out = 100))) %>%
    ungroup() %>%
    mutate(pred = gaussian_1987(temp, rmax, topt, a))
  return(temp)
}))

# select, unnest and calculate 95% CIs of predictions
huds_boot_conf_preds <- dplyr::select(huds_d_fits, curve_id, metric, preds) %>%
  unnest(preds) %>%
  group_by(curve_id, metric, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            .groups = 'drop')

# get tidied parameters using broom::tidy
# get confidence intervals of parameters
huds_d_fits <- mutate(huds_d_fits, params = map(nls_fit, broom::tidy),
                      cis = map(bootstrap, function(x){
                        temp <- confint(x, method = 'bca') %>%
                          as.data.frame() %>%
                          rename(conf_lower = 1, conf_upper = 2) %>%
                          rownames_to_column(., var = 'term')
                        return(temp)
                      }))

# join parameter and confidence intervals in the same dataset 
tonsa_params = left_join(dplyr::select(tonsa_d_fits, curve_id, growth_temp, metric, params) %>% unnest(params),
                         dplyr::select(tonsa_d_fits, curve_id, growth_temp, metric, cis) %>% unnest(cis)) %>%
  mutate(species = "tonsa")

huds_params = left_join(dplyr::select(huds_d_fits, curve_id, growth_temp, metric, params) %>% unnest(params),
                        dplyr::select(huds_d_fits, curve_id, growth_temp, metric, cis) %>% unnest(cis)) %>%
  mutate(species = "hudsonica")

comb_params = bind_rows(tonsa_params, huds_params) %>% 
  mutate(curve_id = fct_relevel(curve_id, "January", "February", "March", "April", "May", "June", 
                                "July", "August", "September", "October", "November_1", "November_2")) %>% 
  mutate("margin" = estimate - growth_temp)

write.csv(comb_params, file = "Output/Data/comb_params.csv", row.names = F)

### Combining data
comb_preds = bind_rows(bind_cols(tonsa_d_preds, species = "tonsa"), 
                       bind_cols(huds_d_preds, species = "hudsonica")) %>% 
  mutate(curve_id = fct_relevel(curve_id, "January", "February", "March", "April", "May", "June",
                                "July", "August", "September", "October", "November_1", "November_2"))

comb_d = bind_rows(bind_cols(tonsa_d, species = "tonsa"), 
                   bind_cols(huds_d, species = "hudsonica")) %>% 
  mutate(curve_id = fct_relevel(curve_id, "January", "February", "March", "April", "May", "June",
                                "July", "August", "September", "October", "November_1", "November_2"))

comb_surv = bind_rows(bind_cols(surv, species = "tonsa"), 
                      bind_cols(huds_surv, species = "hudsonica")) %>% 
  mutate(curve_id = fct_relevel(Month, "January", "February", "March", "April", "May", "June",
                                "July", "August", "September", "October", "November_1", "November_2"))

comb_boot_conf_preds = bind_rows(bind_cols(tonsa_boot_conf_preds, species = "tonsa"),
                                 bind_cols(huds_boot_conf_preds, species = "hudsonica")) %>%
  mutate(curve_id = fct_relevel(curve_id, "January", "February", "March", "April", "May", "June",
                                "July", "August", "September", "October", "November_1", "November_2"))

write.csv(comb_preds, file = "Output/Data/comb_preds.csv", row.names = F)
write.csv(comb_d, file = "Output/Data/comb_d.csv", row.names = F)
write.csv(comb_surv, file = "Output/Data/comb_surv.csv", row.names = F)
write.csv(comb_boot_conf_preds, file = "Output/Data/comb_boot_conf_preds.csv", row.names = F)






#### F0 - Simulated Heatwave ####
# Factors: Month, Treatment, Duration
# Replicates: Female ID
# Variables: Hatched (poisson), Success (quasibinomial?), Size

f0_model_females = F0_epr %>% 
  group_by(Month, Treatment, Female) %>% 
  count() %>% 
  filter(n == 2) %>% 
  mutate('female_ID' = paste(Month, Treatment, Female, sep = "_"))

f0_model_data = F0_epr %>% 
  mutate('female_ID' = paste(Month, Treatment, Female, sep = "_")) %>% 
  filter(female_ID %in% f0_model_females$female_ID)

ggplot(f0_model_data, 
       aes(x = Day, y = Hatched, colour = Month, group = female_ID)) + 
  facet_grid(Month~Treatment, scales = "free_y") + 
  geom_line() + 
  theme_bw()

F0_hatched.model = lme4::glmer(data = f0_model_data, family = poisson, 
                               Hatched ~ Treatment * Day + 
                                 (1|Month) + (1|female_ID))

summary(F0_hatched.model)
car::Anova(F0_hatched.model, type = "III")


#### F1 - Transgenerational effects ####
# Factors: Month, Parental Treatment, Duration, Offspring Temperature
# Replicates: Month
# Variables: Hatched (poisson), Success (quasibinomial?), Size

f1_model_data = F1_epr %>% 
  mutate(Offspring_temp = as.factor(Offspring_temp))

ggplot(f1_model_data, 
       aes(x = Offspring_temp, y = Hatched, colour = Parental_treatment)) + 
  facet_grid(Month~Day) + 
  #geom_violin(position = position_dodge(width = 1)) + 
  geom_boxplot(position = position_dodge(width = 0.5),
               width = 0.3) + 
  geom_point(position = position_dodge(width = 0.5),
             alpha = 0.5) + 
  scale_colour_manual(values = c("Heatwave" = "coral3", "Control" = "skyblue3")) + 
  theme_bw()

F1_hatched.model = lme4::glmer(data = f1_model_data, family = poisson, 
                               Hatched ~ Parental_treatment * Day * Offspring_temp + 
                                 (1 + Parental_treatment|Month))

summary(F1_hatched.model)
coefficients(F1_hatched.model)
car::Anova(F1_hatched.model, type = "III")








F0_epr = F0_epr %>% 
  mutate("Duration" = case_when(
    Day == "1_to_3" ~ "Short", 
    Day == "5_to_7" ~ "Long"),
    Duration = fct_relevel(Duration, "Short", "Long"),
    "treat_ID" = paste(Month, Treatment, sep = "_")) 

short = F0_epr %>%  
  filter(Day == "1_to_3")

long = F0_epr %>%  
  filter(Day == "5_to_7")


total_short = dabest(short, treat_ID, Total, 
                     idx = list(c("June_Control", "June_Heatwave"),
                                c("August_Control", "August_Heatwave"),
                                c("November_Control", "November_Heatwave")),
                     id.column = Female, paired = F) %>% 
  hedges_g()

saveRDS(total_short, file = "Output/Data/F0_total_short.RData")

hs_short = dabest(short, treat_ID, Success, 
                  idx = list(c("June_Control", "June_Heatwave"),
                             c("August_Control", "August_Heatwave"),
                             c("November_Control", "November_Heatwave")),
                  id.column = Female, paired = F) %>% 
  hedges_g()

saveRDS(hs_short, file = "Output/Data/F0_hs_short.RData")

RF_short = dabest(short, treat_ID, Hatched, 
                  idx = list(c("June_Control", "June_Heatwave"),
                             c("August_Control", "August_Heatwave"),
                             c("November_Control", "November_Heatwave")),
                  id.column = Female, paired = F) %>% 
  hedges_g()

saveRDS(RF_short, file = "Output/Data/F0_RF_short.RData")


total_long = dabest(long, treat_ID, Total, id.column = Female, paired = F, 
                    idx = list(c("June_Control", "June_Heatwave"),
                               c("August_Control", "August_Heatwave"),
                               c("November_Control", "November_Heatwave")))%>% 
  hedges_g()

saveRDS(total_long, file = "Output/Data/F0_total_long.RData")

hs_long = dabest(long, treat_ID, Success, id.column = Female, paired = F, 
                 idx = list(c("June_Control", "June_Heatwave"),
                            c("August_Control", "August_Heatwave"),
                            c("November_Control", "November_Heatwave")))%>% 
  hedges_g()

saveRDS(hs_long, file = "Output/Data/F0_hs_long.RData")

RF_long = dabest(long, treat_ID, Hatched, id.column = Female, paired = F, 
                 idx = list(c("June_Control", "June_Heatwave"),
                            c("August_Control", "August_Heatwave"),
                            c("November_Control", "November_Heatwave")))%>% 
  hedges_g()

saveRDS(RF_long, file = "Output/Data/F0_RF_long.RData")


female_size_comp = F0_fbs %>% 
  mutate(treat_ID = paste(Month, Treatment, sep = "_")) %>% 
  dabest(treat_ID, Size, 
         idx = list(c("June_Control", "June_Heatwave"),
                    c("August_Control", "August_Heatwave"),
                    c("November_Control", "November_Heatwave"))) %>% 
  hedges_g()

saveRDS(female_size_comp, file = "Output/Data/female_size_comp.RData")



#Trait effect size summaries
long_sum = RF_long$result
short_sum = RF_short$result
long_sum$duration = "long"
short_sum$duration = "short"

F0_rf_summary = bind_rows(long_sum, short_sum) %>% 
  dplyr::select(-bootstraps) %>% 
  mutate("month" = str_split_fixed(control_group, pattern = "_", n = 2)[,1],
         "trait" = "production",
         "generation" = "F0")

write.csv(F0_rf_summary, file = "Output/Data/F0_rf_summary.csv", row.names = F)

long_total_sum = total_long$result
short_total_sum = total_short$result
long_total_sum$duration = "long"
short_total_sum$duration = "short"

F0_total_summary = bind_rows(long_total_sum, short_total_sum) %>% 
  dplyr::select(-bootstraps) %>% 
  mutate("month" = str_split_fixed(control_group, pattern = "_", n = 2)[,1],
         "trait" = "total",
         "generation" = "F0")

write.csv(F0_total_summary, file = "Output/Data/F0_total_summary.csv", row.names = F)


long_hs_sum = hs_long$result
short_hs_sum = hs_short$result
long_hs_sum$duration = "long"
short_hs_sum$duration = "short"

F0_hs_summary = bind_rows(long_hs_sum, short_hs_sum) %>% 
  dplyr::select(-bootstraps) %>% 
  mutate("month" = str_split_fixed(control_group, pattern = "_", n = 2)[,1],
         "trait" = "success",
         "generation" = "F0")

write.csv(F0_hs_summary, file = "Output/Data/F0_hs_summary.csv", row.names = F)

size_sum = female_size_comp$result
F0_size_summary = size_sum %>% 
  dplyr::select(-bootstraps) %>% 
  mutate("month" = str_split_fixed(control_group, pattern = "_", n = 2)[,1],
         "trait" = "size",
         "generation" = "F0", 
         "duration" = "long")

write.csv(F0_size_summary, file = "Output/Data/F0_size_summary.csv", row.names = F)

### Month effect size summaries
fem_pres = table(factor(F0_epr$Female), F0_epr$Day, F0_epr$Month)
june_female = fem_pres[,1,2] == fem_pres[,2,2]
June_F0 = F0_epr[F0_epr$Month == "June",]
June_F0$treat_ID = paste(June_F0$Duration, June_F0$Treatment, sep = "_")
june_RF = dabest(June_F0, treat_ID, Hatched, paired = T, id.column = Female, 
                 idx = list(c("Short_Control", "Long_Control"),
                            c("Short_Heatwave", "Long_Heatwave"))) %>% 
  hedges_g()

saveRDS(june_RF, file = "Output/Data/F0_june_RF.RData")

aug_female = fem_pres[,1,1] == fem_pres[,2,1]
aug_missing = unname(which(aug_female == F))
August_F0 = F0_epr[F0_epr$Month == "August",]
August_F0$treat_ID = paste(August_F0$Duration, August_F0$Treatment, sep = "_")
aug_missing_pos = which(August_F0$Female %in% aug_missing)

aug = August_F0[-aug_missing_pos,]
aug_RF = dabest(aug, treat_ID, Hatched, paired = T, id.column = Female, 
                idx = list(c("Short_Control", "Long_Control"),
                           c("Short_Heatwave", "Long_Heatwave"))) %>% 
  hedges_g()

saveRDS(aug_RF, file = "Output/Data/F0_aug_RF.RData")

nov_female = fem_pres[,1,3] == fem_pres[,2,3]
nov_missing = unname(which(nov_female == F))
November_F0 = F0_epr[F0_epr$Month == "November",]
November_F0$treat_ID = paste(November_F0$Duration, November_F0$Treatment, sep = "_")
nov_missing_pos = which(November_F0$Female %in% nov_missing)

nov = November_F0[-nov_missing_pos,]
nov_RF = dabest(nov, treat_ID, Hatched, paired = T, id.column = Female, 
                idx = list(c("Short_Control", "Long_Control"),
                           c("Short_Heatwave", "Long_Heatwave"))) %>% 
  hedges_g()

saveRDS(nov_RF, file = "Output/Data/F0_nov_RF.RData")


june_sum = june_RF$result
august_sum = aug_RF$result
november_sum = nov_RF$result
june_sum$month = "June"
august_sum$month = "August"
november_sum$month = "November"

F0_dur_summary = bind_rows(june_sum, august_sum, november_sum) %>% 
  dplyr::select(-bootstraps) %>% 
  mutate("treatment" = str_split_fixed(control_group, pattern = "_", n = 2)[,2])

write.csv(F0_dur_summary, file = 'Output/Data/F0_dur_summary.csv', row.names = F)


#### F1 - Transgen Plasticity ####
F1_epr$Month = factor(F1_epr$Month, levels = c("June", "August", "November"))

F1_epr$Duration = F1_epr$Day
F1_epr$Duration[F1_epr$Duration == "1_to_3"] = "Short"
F1_epr$Duration[F1_epr$Duration == "5_to_7"] = "Long"

F1_fbs$Duration = F1_fbs$Day
F1_fbs$Duration[F1_fbs$Duration == "1_to_3"] = "Short"
F1_fbs$Duration[F1_fbs$Duration == "5_to_7"] = "Long"


month_list = unique(F1_fbs$Month)
F1_fbs$off_ID = paste(F1_fbs$Parental_treatment, F1_fbs$Offspring_temp, sep = "@")

bs_effect_size = data.frame(matrix(nrow = 0, ncol = 0))
for(i in 1:length(month_list)){
  mdata = F1_fbs[F1_fbs$Month == month_list[i],]
  
  short_bs_data = mdata[mdata$Duration == "Short",]
  long_bs_data = mdata[mdata$Duration == "Long",]
  
  if(month_list[i] == "November"){
    short_bs_db = dabest(short_bs_data, off_ID, Size, 
                         idx = list(c("Control@17", "Heatwave@17")))%>% 
      hedges_g()
  }else{
    short_bs_db = dabest(short_bs_data, off_ID, Size, 
                         idx = list(c("Control@12",  "Heatwave@12"),
                                    c("Control@17", "Heatwave@17"),
                                    c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }
  
  long_bs_db = dabest(long_bs_data, off_ID, Size, 
                      idx = list(c("Control@12",  "Heatwave@12"),
                                 c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22")))%>% 
    hedges_g()
  
  short_results = short_bs_db$result %>% 
    dplyr::select(-bootstraps)
  long_results = long_bs_db$result%>% 
    dplyr::select(-bootstraps)
  
  short_results$duration = "short"
  long_results$duration = "long"
  
  short_results$month = as.character(month_list[i])
  long_results$month = as.character(month_list[i])
  
  bs_effect_size = bind_rows(bs_effect_size, short_results, long_results)
  
  saveRDS(short_bs_db, paste("Output/Data/", month_list[i], "_bs_short", ".RData", sep = ""))
  saveRDS(long_bs_db, paste("Output/Data/", month_list[i], "_bs_long", ".RData", sep = ""))
}

bs_effect_size = separate(bs_effect_size, control_group, sep = "@", into = c(NA, "off_temp"), remove = F)
bs_effect_size$month = factor(bs_effect_size$month, levels = c("June", "August", "November"))
bs_effect_size$duration = factor(bs_effect_size$duration, levels = c("short", "long"))
bs_effect_size$trait = "body size"
bs_effect_size$generation = "F1"
write.csv(bs_effect_size, file = "Output/Data/F1_bs_effect_size.csv", row.names = F)

month_list = unique(F1_epr$Month)
F1_epr$off_ID = paste(F1_epr$Parental_treatment, F1_epr$Offspring_temp, sep = "@")
rf_effect_size = data.frame(matrix(nrow = 0, ncol = 0))
for(i in 1:length(month_list)){
  mdata = F1_epr[F1_epr$Month == month_list[i],]
  
  short_data = mdata[mdata$Duration == "Short",]
  long_data = mdata[mdata$Duration == "Long",]
  
  if(month_list[i] == "November"){
    short_db = dabest(short_data, off_ID, Hatched, 
                      idx = list(c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }else{
    
    short_db = dabest(short_data, off_ID, Hatched, 
                      idx = list(c("Control@12",  "Heatwave@12"),
                                 c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }
  
  long_db = dabest(long_data, off_ID, Hatched, 
                   idx = list(c("Control@12",  "Heatwave@12"),
                              c("Control@17", "Heatwave@17"),
                              c("Control@22", "Heatwave@22"))) %>% 
    hedges_g()
  
  short_results = short_db$result %>% 
    dplyr::select(-bootstraps)
  long_results = long_db$result%>% 
    dplyr::select(-bootstraps)
  
  short_results$duration = "short"
  long_results$duration = "long"
  
  short_results$month = as.character(month_list[i])
  long_results$month = as.character(month_list[i])
  
  rf_effect_size = bind_rows(rf_effect_size, short_results, long_results)
  
  saveRDS(short_db, paste("Output/Data/", month_list[i], "_rf_short", ".RData", sep = ""))
  saveRDS(long_db, paste("Output/Data/", month_list[i], "_rf_long", ".RData", sep = ""))
}

rf_effect_size = separate(rf_effect_size, control_group, sep = "@", into = c(NA, "off_temp"), remove = F)
rf_effect_size$ID = paste(rf_effect_size$off_temp, rf_effect_size$duration, sep = "_")
rf_effect_size$month = factor(rf_effect_size$month, levels = c("June", "August", "November"))
rf_effect_size$duration = factor(rf_effect_size$duration, levels = c("short", "long"))
rf_effect_size$trait = "production"
rf_effect_size$generation = "F1"
write.csv(rf_effect_size, file = "Output/Data/F1_rf_effect_size.csv", row.names = F)


month_list = unique(F1_epr$Month)
total_effect_size = data.frame(matrix(nrow = 0, ncol = 0))
for(i in 1:length(month_list)){
  mdata = F1_epr[F1_epr$Month == month_list[i],]
  
  short_data = mdata[mdata$Duration == "Short",]
  long_data = mdata[mdata$Duration == "Long",]
  
  if(month_list[i] == "November"){
    short_db = dabest(short_data, off_ID, Total, 
                      idx = list(c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }else{
    
    short_db = dabest(short_data, off_ID, Total, 
                      idx = list(c("Control@12",  "Heatwave@12"),
                                 c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }
  
  long_db = dabest(long_data, off_ID, Total, 
                   idx = list(c("Control@12",  "Heatwave@12"),
                              c("Control@17", "Heatwave@17"),
                              c("Control@22", "Heatwave@22"))) %>% 
    hedges_g()
  
  short_results = short_db$result %>% 
    dplyr::select(., colnames(short_db$result[-14]))
  long_results = long_db$result%>% 
    dplyr::select(., colnames(long_db$result[-14]))
  
  short_results$duration = "short"
  long_results$duration = "long"
  
  short_results$month = as.character(month_list[i])
  long_results$month = as.character(month_list[i])
  
  total_effect_size = bind_rows(total_effect_size, short_results, long_results)
  
  saveRDS(short_db, paste("Output/Data/", month_list[i], "_total_short", ".RData", sep = ""))
  saveRDS(long_db, paste("Output/Data/", month_list[i], "_total_long", ".RData", sep = ""))
}

total_effect_size = separate(total_effect_size, control_group, sep = "@", into = c(NA, "off_temp"), remove = F)
total_effect_size$ID = paste(total_effect_size$off_temp, total_effect_size$duration, sep = "_")
total_effect_size$month = factor(total_effect_size$month, levels = c("June", "August", "November"))
total_effect_size$duration = factor(total_effect_size$duration, levels = c("short", "long"))
write.csv(total_effect_size, file = "Output/Data/F1_total_effect_size.csv", row.names = F)


hs_effect_size = data.frame(matrix(nrow = 0, ncol = 0))
for(i in 1:length(month_list)){
  mdata = F1_epr[F1_epr$Month == month_list[i],]
  
  short_data = mdata[mdata$Duration == "Short",]
  long_data = mdata[mdata$Duration == "Long",]
  
  if(month_list[i] == "November"){
    short_db = dabest(short_data, off_ID, Success, 
                      idx = list(c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }else{
    
    short_db = dabest(short_data, off_ID, Success, 
                      idx = list(c("Control@12",  "Heatwave@12"),
                                 c("Control@17", "Heatwave@17"),
                                 c("Control@22", "Heatwave@22"))) %>% 
      hedges_g()
  }
  
  long_db = dabest(long_data, off_ID, Success, 
                   idx = list(c("Control@12",  "Heatwave@12"),
                              c("Control@17", "Heatwave@17"),
                              c("Control@22", "Heatwave@22"))) %>% 
    hedges_g()
  
  short_results = short_db$result %>% 
    dplyr::select(., colnames(short_db$result[-14]))
  long_results = long_db$result%>% 
    dplyr::select(., colnames(long_db$result[-14]))
  
  short_results$duration = "short"
  long_results$duration = "long"
  
  short_results$month = as.character(month_list[i])
  long_results$month = as.character(month_list[i])
  
  hs_effect_size = bind_rows(hs_effect_size, short_results, long_results)
  
  saveRDS(short_db, paste("Output/Data/", month_list[i], "_hs_short", ".RData", sep = ""))
  saveRDS(long_db, paste("Output/Data/", month_list[i], "_hs_long", ".RData", sep = ""))
}


hs_effect_size = separate(hs_effect_size, control_group, sep = "@", into = c(NA, "off_temp"), remove = F)
hs_effect_size$ID = paste(hs_effect_size$off_temp, hs_effect_size$duration, sep = "_")
hs_effect_size$month = factor(hs_effect_size$month, levels = c("June", "August", "November"))
hs_effect_size$duration = factor(hs_effect_size$duration, levels = c("short", "long"))
write.csv(hs_effect_size, file = "Output/Data/F1_hs_effect_size.csv", row.names = F)
