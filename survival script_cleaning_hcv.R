## load packages
pacman::p_load(dplyr, haven, tidyr, readxl, writexl, survival)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Emails to authors/Czech data/Data")

## load data
hcv_data_wide <- read_excel("czech_data_raw.xlsx")

# recode sex work to sensible binary numbers
hcv_data_wide$sw_ever[hcv_data_wide$sw_ever == 1] <- 0
hcv_data_wide$sw_ever[hcv_data_wide$sw_ever %in% c(2, 3)] <- 1
hcv_data_wide$sw_ever[is.na(hcv_data_wide$sw_ever)] <- 0

# recode missing data
hcv_data_wide$pris_ever[is.na(hcv_data_wide$pris_ever)] <- 0
hcv_data_wide$dur_inj[is.na(hcv_data_wide$dur_inj)] <- 7
hcv_data_wide$freq_inj[is.na(hcv_data_wide$freq_inj)] <- 7

# numeric to factor
hcv_data_wide$sex <- factor(hcv_data_wide$sex)
hcv_data_wide$pris_ever <- factor(hcv_data_wide$pris_ever)
hcv_data_wide$dur_inj <- factor(hcv_data_wide$dur_inj)
hcv_data_wide$freq_inj <- factor(hcv_data_wide$freq_inj)
hcv_data_wide$sw_ever <- factor(hcv_data_wide$sw_ever)

# save data
write_xlsx(hcv_data_wide,"czech_data_analysis.xlsx")
