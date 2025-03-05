## load packages
pacman::p_load(dplyr, haven, tidyr, readxl, writexl, survival, arsenal)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Emails to authors/Czech data/Data")

## load data
hcv_data_analysis <- read_excel("czech_data_analysis.xlsx")

# stratify by sex
hcv_data_analysis_men <- subset(hcv_data_analysis, sex == 1)
hcv_data_analysis_women <- subset(hcv_data_analysis, sex == 2)

# baseline characteristics

# male and female
baseline <- tableby(~ sex + age + sw_ever + pris_ever + dur_inj + freq_inj, data=hcv_data_analysis)
summary(baseline, text=TRUE)

# stratified
baseline_sex <- tableby(sex ~ age + sw_ever + pris_ever + dur_inj + freq_inj, data=hcv_data_analysis)
summary(baseline_sex, text=TRUE)

# overall incidence rate
total_days_hcv <- sum(hcv_data_analysis$days_end)
total_cases <- sum(hcv_data_analysis$hcv_rslt)
incidence_rate <- (total_cases / total_days_hcv) * 365.25 *100

cat("Incidence rate of HCV per 100 person years:", incidence_rate)

# selling sex work incidence rate
total_days_sw <- sum(hcv_data_analysis$days_end[hcv_data_analysis$sw_ever == 1])
total_cases_sw <- sum(hcv_data_analysis$hcv_rslt[hcv_data_analysis$sw_ever == 1])
incidence_rate_sw <- (total_cases_sw / total_days_sw) * 365.25 *100
cat("Incidence rate of HCV per 100 person years among sex workers:", incidence_rate_sw)

# no sex work incidence rate
total_days_nosw <- sum(hcv_data_analysis$days_end[hcv_data_analysis$sw_ever == 0])
total_cases_nosw <- sum(hcv_data_analysis$hcv_rslt[hcv_data_analysis$sw_ever == 0])
incidence_rate_nosw <- (total_cases_nosw / total_days_nosw) * 365.25 *100
cat("Incidence rate of HCV per 100 person years among non-sex workers:", incidence_rate_nosw)

# selling sex work incidence rate - men
total_days_sw_men <- sum(hcv_data_analysis_men$days_end[hcv_data_analysis_men$sw_ever == 1])
total_cases_sw_men <- sum(hcv_data_analysis_men$hcv_rslt[hcv_data_analysis_men$sw_ever == 1])
incidence_rate_sw_men <- (0.5 / total_days_sw_men) * 365.25 *100
cat("Incidence rate of HCV per 100 person years among male sex workers:", incidence_rate_sw_men)

# no sex work incidence rate - men
total_days_nosw_men <- sum(hcv_data_analysis_men$days_end[hcv_data_analysis_men$sw_ever == 0])
total_cases_nosw_men <- sum(hcv_data_analysis_men$hcv_rslt[hcv_data_analysis_men$sw_ever == 0])
incidence_rate_nosw_men <- ((total_cases_nosw_men+0.5) / total_days_nosw_men) * 365.25 *100
cat("Incidence rate of HCV per 100 person years among male non-sex workers:", incidence_rate_nosw_men)

# selling sex work incidence rate - women
total_days_sw_women <- sum(hcv_data_analysis_women$days_end[hcv_data_analysis_women$sw_ever == 1])
total_cases_sw_women <- sum(hcv_data_analysis_women$hcv_rslt[hcv_data_analysis_women$sw_ever == 1])
incidence_rate_sw_women <- (total_cases_sw_women / total_days_sw_women) * 365.25 *100
cat("Incidence rate of HCV per 100 person years among female sex workers:", incidence_rate_sw_women)

# no sex work incidence rate - women
total_days_nosw_women <- sum(hcv_data_analysis_women$days_end[hcv_data_analysis_women$sw_ever == 0])
total_cases_nosw_women <- sum(hcv_data_analysis_women$hcv_rslt[hcv_data_analysis_women$sw_ever == 0])
incidence_rate_nosw_women <- ((total_cases_nosw_women) / total_days_nosw_women) * 365.25 *100
cat("Incidence rate of HCV per 100 person years among female non-sex workers:", incidence_rate_nosw_women)

# unadjusted hazard ratio selling sw HCV ever - men and women
sw_model_crude = coxph(
  Surv(time = days_start, time2 = days_end, event = hcv_rslt) ~ sw_ever, 
  data = hcv_data_analysis
)

summary(sw_model_crude)

# unadjusted hazard ratio selling sw HCV ever - men
sw_model_crude_men = coxph(
  Surv(time = days_start, time2 = days_end, event = hcv_rslt) ~ sw_ever, 
  data = hcv_data_analysis_men
)

summary(sw_model_crude_men)

# unadjusted hazard ratio selling sw HCV ever - women
sw_model_crude_women = coxph(
  Surv(time = days_start, time2 = days_end, event = hcv_rslt) ~ sw_ever, 
  data = hcv_data_analysis_women
)

summary(sw_model_crude_women)

# adjusted hazard ratio selling sw HCV ever - women
sw_model_adj_women = coxph(
  Surv(time = days_start, time2 = days_end, event = hcv_rslt) ~ sw_ever + pris_ever + freq_inj, 
  data = hcv_data_analysis_women
)

summary(sw_model_adj_women)

