run_sim <- function(site_data, # site inforamtion from site file, with calibrateD EIR
                    site_name, # GMB or BFA
                    run_parameters, # small df of pop, burnin, etc.
                    parameter_draw, # 0-50
                    scenario){ # mass, mass+MDA, none
  
  message('parameterizing')
  params_scenario_list <- parameterize_site(site_data = site_data,
                    site_name = site_name,
                    run_parameters = run_parameters, 
                    parameter_draw = parameter_draw, 
                    scenario = scenario)
  
  params_scenario <- params_scenario_list$param_list
  params_scenario$progress_bar <- TRUE
  
  # if this is a stochastic run, set parameter draw ------------------------------
  if (parameter_draw > 0){
    params_scenario<- params_scenario |>
      malariasimulation::set_parameter_draw(parameter_draw) 
  }
  
  set.seed(28)
  
  params_scenario <- malariasimulation::set_equilibrium(
    params_scenario,
    init_EIR = site_data$init_EIR
  )
  
  message('starting simulation')
  model <- retry::retry(
    malariasimulation::run_simulation(timesteps = run_parameters$sim_length + run_parameters$burnin,
                                      parameters = params_scenario),
    max_tries = 5,
    when = 'error reading from connection|embedded nul|unknown type',
    interval = 3)
  
  raw_output <- postie::drop_burnin(model, burnin = run_parameters$burnin)
  
  message('calculating rates')
  
  rates <- postie::get_rates(
    raw_output,
    baseline_year = 2020
  ) %>%# add identifying information to output
    mutate(site_name = site_name,
           scenario = scenario,
           parameter_draw = parameter_draw,
           population = run_parameters$population,
           burnin = run_parameters$burnin)
  
  annual_output <- rates %>%
    dplyr::summarise(
      clinical = stats::weighted.mean(clinical, person_days),
      severe = stats::weighted.mean(severe, person_days),
      mortality = stats::weighted.mean(mortality, person_days),
      yll = stats::weighted.mean(yll, person_days),
      yld = stats::weighted.mean(yld, person_days),
      dalys = stats::weighted.mean(dalys, person_days),
      person_days = sum(person_days),
      time = mean(time),
      .by = c(year, age_lower, age_upper)
    )
  
  # These are all in cases per person per day 
  annual_rates <- annual_output %>%
    mutate(
      mortality = if_else(is.na(mortality), 0, mortality),
      clinical = if_else(is.na(clinical), 0, clinical),
    )%>%# add identifying information to output
    mutate(site_name = site_name,
           scenario = scenario,
           parameter_draw = parameter_draw,
           population = run_parameters$population,
           burnin = run_parameters$burnin)
  
  monthly_output <- rates %>% 
    dplyr::summarise(
      clinical = stats::weighted.mean(clinical, person_days),
      severe = stats::weighted.mean(severe, person_days),
      mortality = stats::weighted.mean(mortality, person_days),
      yll = stats::weighted.mean(yll, person_days),
      yld = stats::weighted.mean(yld, person_days),
      dalys = stats::weighted.mean(dalys, person_days),
      person_days = sum(person_days),
      time = mean(time),
      .by = c(month, year, age_lower, age_upper)
    ) %>%# add identifying information to output
    mutate(site_name = site_name,
           scenario = scenario,
           parameter_draw = parameter_draw,
           population = run_parameters$population,
           burnin = run_parameters$burnin)
  
  monthly_rates <- monthly_output %>%
    mutate(
      mortality = if_else(is.na(mortality), 0, mortality),
      clinical = if_else(is.na(clinical), 0, clinical),
    )  %>%# add identifying information to output
    mutate(site_name = site_name,
           scenario = scenario,
           parameter_draw = parameter_draw,
           population = run_parameters$population,
           burnin = run_parameters$burnin)
  
  message('calculating prevalence')
  # Get prevalence 
  prev <- raw_output %>%
    postie::get_prevalence(
      diagnostic = 'pcr',
      baseline_year = 2020
    )
  
  prev_monthly <- prev %>%
    dplyr::summarise(
      dplyr::across(dplyr::everything(), mean),
      time = mean(.data$time),
      .by = c('month', 'year')
    )
  
  prev_annual <- prev %>%
    dplyr::summarise(
      dplyr::across(dplyr::everything(), mean),
      time = mean(.data$time),
      .by = c('year')
    )
  
  monthly_output <- left_join(monthly_rates, prev_monthly,
                              by = c('month','year'))
  
  # daily_output <- left_join(rates, prev)
  
  annual_output <- left_join(annual_rates, prev_annual)
  
  return(list('monthly' = monthly_output,
              'annual' = annual_output,
              # 'daily' = daily_output,
              # 'raw' = raw_output,
              "parameters" = params_scenario))
}