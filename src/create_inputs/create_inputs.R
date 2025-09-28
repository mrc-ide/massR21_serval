# Task to create inputs for the malariasimulation runs 

# packages
library(cali)
library(site)
library(ggplot2)
library(tidyr)
library(dplyr)
library(malariasimulation)
library(orderly2)

orderly2::orderly_resource('extract_site_data.R')
source('extract_site_data.R')
orderly2::orderly_resource('parameterize_site.R')
source('parameterize_site.R')

# Define outputs of this task 
# orderly_artefact()


# Pull sites ----
#### First pass - constant intervention coverage:
siteg <- readRDS('inputs/gmb_serval_site.rds')
gmb_tx <- siteg$interventions |>
  filter(year>=2022) |>
  summarise(tx_cov = mean(tx_cov),
            prop_act = mean(prop_act),
            smc_cov = mean(smc_cov))

gmb_seas <- siteg$seasonality$seasonality_parameters

gmb_site_info <- list(
  demography = siteg$demography,
  vectors = siteg$vectors,
  seasonality = gmb_seas,
  eir = siteg$eir$eir,
  tx_cov = gmb_tx$tx_cov,
  prop_act = gmb_tx$prop_act,
  smc_cov = gmb_tx$smc_cov,
  baseline_inci = 1.31, # per 100 person months
  baseline_prev = 0.15
)

#### First pass - constant intervention coverage:
siteb <- readRDS('inputs/bfa_serval_site.rds')
bfa_tx <- siteb$interventions |>
  filter(year>=2022) |>
  summarise(tx_cov = mean(tx_cov),
            prop_act = mean(prop_act),
            smc_cov = mean(smc_cov))

bfa_seas <- siteb$seasonality$seasonality_parameters

bfa_site_info <- list(
  demography = siteb$demography,
  vectors = siteb$vectors,
  seasonality = bfa_seas,
  eir = siteb$eir$eir,
  tx_cov = bfa_tx$tx_cov,
  prop_act = bfa_tx$prop_act,
  smc_cov = bfa_tx$smc_cov,
  baseline_inci = 5.84, # per 100 person months
  baseline_prev = 0.6
)

# Basic model run parameters 
population <- 1e5 

burnin <- 25*365

sim_length <- 15*365

min_ages <- c(0, 5 * 365, 15 * 365, 0)
max_ages <- c(5 * 365, 15 * 365, 100 * 365, 100 * 365)


run_parameters <- data.frame(
  population = population, 
  burnin = burnin, 
  sim_length = sim_length, 
  min_ages = I(list(min_ages)), 
  max_ages = I(list(max_ages))
)

# Parameterize baseline - BFA
baseline_bfa_params <- parameterize_site(site_data = bfa_site_info,
                                         site_name = 'BFA',
                                         run_parameters = run_parameters, 
                                         parameter_draw = 0, 
                                         scenario = 'baseline')
baseline_gmb_params <- parameterize_site(site_data = bfa_site_info,
                                         site_name = 'GMB',
                                         run_parameters = run_parameters, 
                                         parameter_draw = 0, 
                                         scenario = 'baseline')


# Calibrate to trial incidence ----
bfa_calibrated <- calibration(
  params = baseline_bfa_params$param_list,
  site_name = 'BFA',
  site_data = bfa_site_info,
  EIR_limits = c(0, 1500),
  max_attempts = 20
)

gmb_calibrated <- calibration(
  params = baseline_gmb_params$param_list,
  site_name = 'GMB',
  site_data = bfa_site_info,
  EIR_limits = c(0, 1500),
  max_attempts = 20
)

saveRDS(bfa_calibrated, file = 'bfa_eir_calibrated.rds')
saveRDS(gmb_calibrated, file = 'gmb_eir_calibrated.rds')
# Scenario parameters

# adult_scaling <- 0.5
# 
# ado_scaling <- 0.6
# 
# vax_min_age <- 6 * (365 / 12)
# vax_max_age <- 100 * 365
# 
# scenario_inputs



