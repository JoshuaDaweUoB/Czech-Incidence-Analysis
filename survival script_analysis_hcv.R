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

# stratified
baseline_sex <- tableby(sex ~ age + sw_ever + pris_ever + dur_inj + freq_inj, data=hcv_data_analysis)
summary(baseline_sex, text=TRUE)

# overall incidence rate
total_days_hcv <- sum(hcv_data_analysis$days_end)
total_cases <- sum(hcv_data_analysis$hcv_rslt)
incidence_rate <- (total_cases / total_days_hcv) * 365.25 *100

cat("Incidence rate of HCV per 100 person years:", incidence_rate)

# Relevel the injecting frequency variable to be binary
hcv_data_analysis$freq_inj <- ifelse(as.numeric(hcv_data_analysis$freq_inj) >= 5, 1, 0)
hcv_data_analysis$freq_inj <- factor(hcv_data_analysis$freq_inj, levels = c(0, 1))

# Relevel the injecting duration variable to be binary
hcv_data_analysis$dur_inj <- ifelse(as.numeric(hcv_data_analysis$dur_inj) >= 4, 1, 0)
hcv_data_analysis$dur_inj <- factor(hcv_data_analysis$dur_inj, levels = c(0, 1))

# Function to calculate incidence rates and rate ratios
calculate_incidence_and_rate_ratio <- function(data, time_bin, group_label) {
  # selling sex work incidence rate
  total_days_hcv_sw <- sum(data$days_end[data[[time_bin]] == 1])
  total_cases_sw <- sum(data$hcv_rslt[data[[time_bin]] == 1])
  incidence_rate_sw <- (total_cases_sw / total_days_hcv_sw) * 365.25 * 100

  # Calculate 95% confidence intervals for sex workers
  incidence_rate_sw_se <- sqrt(total_cases_sw) / total_days_hcv_sw * 365.25 * 100
  ci_lower_sw <- incidence_rate_sw - 1.96 * incidence_rate_sw_se
  ci_upper_sw <- incidence_rate_sw + 1.96 * incidence_rate_sw_se

  cat("Incidence rate of HCV per 100 person years among sex workers (", group_label, "):", incidence_rate_sw, "\n")
  cat("95% CI:", ci_lower_sw, "-", ci_upper_sw, "\n")

  # no sex work incidence rate
  total_days_hcv_nosw <- sum(data$days_end[data[[time_bin]] == 0])
  total_cases_nosw <- sum(data$hcv_rslt[data[[time_bin]] == 0])
  incidence_rate_nosw <- (total_cases_nosw / total_days_hcv_nosw) * 365.25 * 100

  # Calculate 95% confidence intervals for non-sex workers
  incidence_rate_nosw_se <- sqrt(total_cases_nosw) / total_days_hcv_nosw * 365.25 * 100
  ci_lower_nosw <- incidence_rate_nosw - 1.96 * incidence_rate_nosw_se
  ci_upper_nosw <- incidence_rate_nosw + 1.96 * incidence_rate_nosw_se

  cat("Incidence rate of HCV per 100 person years among non-sex workers (", group_label, "):", incidence_rate_nosw, "\n")
  cat("95% CI:", ci_lower_nosw, "-", ci_upper_nosw, "\n")

  # Calculate rate ratio and its 95% confidence interval
  rate_ratio <- incidence_rate_sw / incidence_rate_nosw
  rate_ratio_se <- sqrt((1 / total_cases_sw) + (1 / total_cases_nosw))
  ci_lower_rr <- exp(log(rate_ratio) - 1.96 * rate_ratio_se)
  ci_upper_rr <- exp(log(rate_ratio) + 1.96 * rate_ratio_se)

  cat("Rate ratio of HCV (sex workers vs non-sex workers) (", group_label, "):", rate_ratio, "\n")
  cat("95% CI:", ci_lower_rr, "-", ci_upper_rr, "\n")

  # Create a summary dataset for Poisson regression
  summary_data <- data %>%
    group_by(!!sym(time_bin), freq_inj, dur_inj, pris_ever) %>%
    summarise(
      total_cases = sum(hcv_rslt),
      total_days = sum(days_end),
      .groups = 'drop'
    ) %>%
    mutate(
      rate = total_cases / total_days * 365.25 * 100
    )

  # Fit Poisson regression model controlling for incarceration
  poisson_model1 <- glm(total_cases ~ get(time_bin) + pris_ever + offset(log(total_days)), 
                        family = poisson(link = "log"), 
                        data = summary_data)

  # Extract rate ratio and confidence intervals
  rate_ratio1 <- exp(coef(poisson_model1)[2])
  ci1 <- exp(confint(poisson_model1)[2, ])

  cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for incarceration (", group_label, "):", rate_ratio1, "\n")
  cat("95% CI:", ci1[1], "-", ci1[2], "\n")

  # Fit Poisson regression model controlling for incarceration and injecting duration
  poisson_model2 <- glm(total_cases ~ get(time_bin) + pris_ever + dur_inj + offset(log(total_days)), 
                        family = poisson(link = "log"), 
                        data = summary_data)

  # Extract rate ratio and confidence intervals
  rate_ratio2 <- exp(coef(poisson_model2)[2])
  ci2 <- exp(confint(poisson_model2)[2, ])

  cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for incarceration and injecting duration (", group_label, "):", rate_ratio2, "\n")
  cat("95% CI:", ci2[1], "-", ci2[2], "\n")
}

# Apply the function to the hcv_data_analysis dataframe
calculate_incidence_and_rate_ratio(hcv_data_analysis, "sw_ever", "Overall")

# Function to calculate incidence rates and rate ratios
calculate_incidence_and_rate_ratio_females <- function(data, time_bin, group_label, sex_label) {
  # Filter data by sex
  data <- data[data$sex == sex_label, ]
  
  # selling sex work incidence rate
  total_days_hcv_sw <- sum(data$days_end[data[[time_bin]] == 1])
  total_cases_sw <- sum(data$hcv_rslt[data[[time_bin]] == 1])
  incidence_rate_sw <- (total_cases_sw / total_days_hcv_sw) * 365.25 * 100

  # Calculate 95% confidence intervals for sex workers
  incidence_rate_sw_se <- sqrt(total_cases_sw) / total_days_hcv_sw * 365.25 * 100
  ci_lower_sw <- incidence_rate_sw - 1.96 * incidence_rate_sw_se
  ci_upper_sw <- incidence_rate_sw + 1.96 * incidence_rate_sw_se

  cat("Incidence rate of HCV per 100 person years among sex workers (", group_label, ", ", sex_label, "):", incidence_rate_sw, "\n")
  cat("95% CI:", ci_lower_sw, "-", ci_upper_sw, "\n")

  # no sex work incidence rate
  total_days_hcv_nosw <- sum(data$days_end[data[[time_bin]] == 0])
  total_cases_nosw <- sum(data$hcv_rslt[data[[time_bin]] == 0])
  incidence_rate_nosw <- (total_cases_nosw / total_days_hcv_nosw) * 365.25 * 100

  # Calculate 95% confidence intervals for non-sex workers
  incidence_rate_nosw_se <- sqrt(total_cases_nosw) / total_days_hcv_nosw * 365.25 * 100
  ci_lower_nosw <- incidence_rate_nosw - 1.96 * incidence_rate_nosw_se
  ci_upper_nosw <- incidence_rate_nosw + 1.96 * incidence_rate_nosw_se

  cat("Incidence rate of HCV per 100 person years among non-sex workers (", group_label, ", ", sex_label, "):", incidence_rate_nosw, "\n")
  cat("95% CI:", ci_lower_nosw, "-", ci_upper_nosw, "\n")

  # Calculate rate ratio and its 95% confidence interval
  rate_ratio <- incidence_rate_sw / incidence_rate_nosw
  rate_ratio_se <- sqrt((1 / total_cases_sw) + (1 / total_cases_nosw))
  ci_lower_rr <- exp(log(rate_ratio) - 1.96 * rate_ratio_se)
  ci_upper_rr <- exp(log(rate_ratio) + 1.96 * rate_ratio_se)

  cat("Rate ratio of HCV (sex workers vs non-sex workers) (", group_label, ", ", sex_label, "):", rate_ratio, "\n")
  cat("95% CI:", ci_lower_rr, "-", ci_upper_rr, "\n")

  # Create a summary dataset for Poisson regression
  summary_data <- data %>%
    group_by(!!sym(time_bin), freq_inj, dur_inj, pris_ever) %>%
    summarise(
      total_cases = sum(hcv_rslt),
      total_days = sum(days_end),
      .groups = 'drop'
    ) %>%
    mutate(
      rate = total_cases / total_days * 365.25 * 100
    )

  # Fit Poisson regression model controlling for incarceration
  poisson_model1 <- glm(total_cases ~ get(time_bin) + pris_ever + offset(log(total_days)), 
                        family = poisson(link = "log"), 
                        data = summary_data)

  # Extract rate ratio and confidence intervals
  rate_ratio1 <- exp(coef(poisson_model1)[2])
  ci1 <- exp(confint(poisson_model1)[2, ])

  cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for incarceration (", group_label, ", ", sex_label, "):", rate_ratio1, "\n")
  cat("95% CI:", ci1[1], "-", ci1[2], "\n")

  # Fit Poisson regression model controlling for incarceration and injecting duration
  poisson_model2 <- glm(total_cases ~ get(time_bin) + pris_ever + dur_inj + offset(log(total_days)), 
                        family = poisson(link = "log"), 
                        data = summary_data)

  # Extract rate ratio and confidence intervals
  rate_ratio2 <- exp(coef(poisson_model2)[2])
  ci2 <- exp(confint(poisson_model2)[2, ])

  cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for incarceration and injecting duration (", group_label, ", ", sex_label, "):", rate_ratio2, "\n")
  cat("95% CI:", ci2[1], "-", ci2[2], "\n")
}

# Apply the function to the hcv_data_analysis dataframe for females
calculate_incidence_and_rate_ratio_females(hcv_data_analysis, "sw_ever", "Overall", 2) # Females

# Filter data for males
hcv_data_analysis_men <- subset(hcv_data_analysis, sex == 1)

# Manually calculate rate ratio for lifetime sex work with additional 0.5 cases
# Calculate incidence rate for lifetime sex work exposure with additional 0.5 cases
total_days_hcv_sw_lifetime <- sum(hcv_data_analysis_men$days_end[hcv_data_analysis_men$sw_ever == 1])
total_cases_sw_lifetime <- sum(hcv_data_analysis_men$hcv_rslt[hcv_data_analysis_men$sw_ever == 1]) + 0.5
incidence_rate_sw_lifetime <- (total_cases_sw_lifetime / total_days_hcv_sw_lifetime) * 365.25 * 100

# Calculate 95% confidence intervals for sex workers
incidence_rate_sw_lifetime_se <- sqrt(total_cases_sw_lifetime) / total_days_hcv_sw_lifetime * 365.25 * 100
ci_lower_sw_lifetime <- incidence_rate_sw_lifetime - 1.96 * incidence_rate_sw_lifetime_se
ci_upper_sw_lifetime <- incidence_rate_sw_lifetime + 1.96 * incidence_rate_sw_lifetime_se

cat("Incidence rate of HCV per 100 person years among sex workers (lifetime exposure):", incidence_rate_sw_lifetime, "\n")
cat("95% CI:", ci_lower_sw_lifetime, "-", ci_upper_sw_lifetime, "\n")

# Calculate incidence rate for unexposed males with additional 0.5 cases
total_days_hcv_nosw_lifetime <- sum(hcv_data_analysis_men$days_end[hcv_data_analysis_men$sw_ever == 0])
total_cases_nosw_lifetime <- sum(hcv_data_analysis_men$hcv_rslt[hcv_data_analysis_men$sw_ever == 0]) + 0.5
incidence_rate_nosw_lifetime <- (total_cases_nosw_lifetime / total_days_hcv_nosw_lifetime) * 365.25 * 100

# Calculate 95% confidence intervals for non-sex workers
incidence_rate_nosw_lifetime_se <- sqrt(total_cases_nosw_lifetime) / total_days_hcv_nosw_lifetime * 365.25 * 100
ci_lower_nosw_lifetime <- incidence_rate_nosw_lifetime - 1.96 * incidence_rate_nosw_lifetime_se
ci_upper_nosw_lifetime <- incidence_rate_nosw_lifetime + 1.96 * incidence_rate_nosw_lifetime_se

cat("Incidence rate of HCV per 100 person years among non-sex workers (lifetime exposure):", incidence_rate_nosw_lifetime, "\n")
cat("95% CI:", ci_lower_nosw_lifetime, "-", ci_upper_nosw_lifetime, "\n")

# Calculate rate ratio and its 95% confidence interval
rate_ratio_lifetime <- incidence_rate_sw_lifetime / incidence_rate_nosw_lifetime
rate_ratio_lifetime_se <- sqrt((1 / total_cases_sw_lifetime) + (1 / total_cases_nosw_lifetime))
ci_lower_rr_lifetime <- exp(log(rate_ratio_lifetime) - 1.96 * rate_ratio_lifetime_se)
ci_upper_rr_lifetime <- exp(log(rate_ratio_lifetime) + 1.96 * rate_ratio_lifetime_se)

cat("Rate ratio of HCV (sex workers vs non-sex workers) (lifetime exposure):", rate_ratio_lifetime, "\n")
cat("95% CI:", ci_lower_rr_lifetime, "-", ci_upper_rr_lifetime, "\n")