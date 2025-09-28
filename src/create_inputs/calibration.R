# Parameterize

#' @param iso site name
#' @param target_inci target incidence in whole population - from report 
calibration <- function(params, 
                        site_name,
                        site_data,
                        EIR_limits,
                        ...){
  
  # Prepare a summary function that returns the mean incidence from each simulation output: 
  summary_mean_incidence <- function (x) {
    
    x$year <- ceiling(x$timestep / 365)
    # Filter to most recent year 
    x <- x[x$year == max(x$year),]
    
    # Get month variable 
    x$month <- ceiling(x$timestep / (365/12))
    
    x <- x %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(clinical = sum(n_inc_clinical_0_36500),
                       mean_pop = mean(n_age_0_36500),
                       inci = clinical / mean_pop)
      
    # Calculate the mean monthly incidence in most recent year:
    # because of stable population, person-months is just the population * 12 months 
    inci_0_100 <- mean(x$inci * 100)
    
    # Return the calculated PfPR2-10:
    return(inci_0_100)
  }
  
  target_inci <- site_data$baseline_inci # clinical cases per 100 person-months in 2024 baseline 
  
  baseline_prev <- site_data$baseline_prev
  
  # Number of timesteps to simulate over. 
  params$timesteps <- 10 * 365
  
  # Establish a tolerance value:
  inci_tolerance <- 0.01
  
  # Set upper and lower EIR bounds for the calibrate function to check (remembering EIR is
  # the variable that is used to tune to the target):
  eir_limits = EIR_limits
  
  # Run the calibrate() function:
  cali_EIR <- cali::calibrate(target = target_inci,
                              summary_function = summary_mean_incidence,
                              parameters = params,
                              eq_prevalence = baseline_prev, 
                              eir_limits = eir_limits)
  
  print(paste0('calibrated EIR for site ', site_name, ' :', cali_EIR))
  
  params<- malariasimulation::set_equilibrium(params, init_EIR = cali_EIR)
  eir_info<- data.frame('site_name' = site_name, 'EIR' = cali_EIR)
  
  return(list('params' = params, 'eir_info' = eir_info))
}
