# function to parameterize each site 

#' @param site_data site data for the site
#' @param inputs model inputs
#' @param parameter_draw value between 0 and 50
#' @param scenario baseline (used for calibration), or vaccine scenarios ('mass' or 'mass+MDA')
parameterize_site <- function(site_data, 
                              site_name,
                              run_parameters,
                              parameter_draw,
                              scenario){
  
  # Basic parameters
  params <- malariasimulation::get_parameters(
    overrides = list(
      human_population = run_parameters$population,
      model_seasonality = TRUE, 
      g0 = site_data$seasonality$g0,
      g = c(site_data$seasonality$g1, site_data$seasonality$g2, site_data$seasonality$g3),
      h = c(site_data$seasonality$h1, site_data$seasonality$h2, site_data$seasonality$h3),
      individual_mosquitoes = FALSE
    )
  )
  
  # Demography
  ages <- round(unique(site_data$demography$age_upper) * 365)
  timesteps <- 365 * (unique(site_data$demography$year) - 2000)
  deathrates <- site_data$demography$adjusted_mortality_rates / 365
  deathrates_matrix <- matrix(deathrates, nrow = length(timesteps), byrow = TRUE)
  # Add parameters
  params <- malariasimulation::set_demography(
    parameters = params,
    agegroups = ages,
    timesteps = timesteps,
    deathrates = deathrates_matrix
  )
  
  
  # Vectors
  params <- malariasimulation::set_species(
    parameters = params,
    species = list(malariasimulation::gamb_params, malariasimulation::arab_params, malariasimulation::fun_params),
    proportions = site_data$vectors$vector_species$prop) 
  # may want to update phi_bednets and phi_indoors (see https://github.com/kellymccain28/catchup_extraboosters/blob/main/src/1_create_parameter_list/generate_params.R)
  
  # Treatment
  # Shape (alpha) 2.516 and scale (beta) 46.68 From Gina's # this is from site::add_drugs
  # fit to Cisse et al data
  SP_params <- c(0.9, 0.32, 2.516, 46.68)
  
  params <- malariasimulation::set_drugs(
    parameters = params,
    drugs = list(
      malariasimulation::AL_params, 
      malariasimulation::SP_AQ_params,
      SP_params))
  
  # below treatment code from site::add_treatment
  # Non ACT (SP)
  params <- malariasimulation::set_clinical_treatment(
    parameters = params,
    drug = 3,
    timesteps = 1, # assuming no treatment changes
    coverages = site_data$tx_cov * (1 - site_data$prop_act)
  )
  # ACT (AL)
  params <- malariasimulation::set_clinical_treatment(
    parameters = params,
    drug = 1,
    timesteps = 1, # assuming no treatment changes
    coverages = site_data$tx_cov * site_data$prop_act
  )
  
  
  # SMC
  peak <- malariasimulation::peak_season_offset(params)
  # 4 doses, centered around peak
  first <- round(run_parameters$burnin + c(peak + c(-1, 0, 1, 2) * (365/12)), 0)
  firststeps <- sort(rep(first, (run_parameters$burnin + run_parameters$sim_length)/365))
  yearsteps <- rep(c(0, seq(365, (run_parameters$burnin + run_parameters$sim_length) - 365, 365)), length(first))
  smc_timesteps <- yearsteps + firststeps
  
  params <- malariasimulation::set_smc(
    params, 
    drug = 2, 
    timesteps = sort(smc_timesteps),
    coverages = rep(site_data$smc_cov, length(smc_timesteps)),
    min_ages = rep(91, length(smc_timesteps)), 
    max_ages = rep(1825, length(smc_timesteps))
  )
  
  params$progress_bar <- TRUE
  
  # set age groups
  params$clinical_incidence_rendering_min_ages = unlist(run_parameters$min_ages)
  params$clinical_incidence_rendering_max_ages = unlist(run_parameters$max_ages)
  params$severe_incidence_rendering_min_ages = unlist(run_parameters$min_ages)
  params$severe_incidence_rendering_max_ages = unlist(run_parameters$max_ages)
  params$age_group_rendering_min_ages = unlist(run_parameters$min_ages)
  params$age_group_rendering_max_ages = unlist(run_parameters$max_ages)
  params$prevalence_rendering_min_ages = unlist(run_parameters$min_ages)
  params$prevalence_rendering_max_ages = unlist(run_parameters$max_ages)
  
  # Scenario parameters
  
  adult_scaling <- 0.2
  ado_scaling <- 0.5
  
  vax_min_age <- 6 * (365 / 12)
  vax_max_age <- 100 * 365
  
  mass_timestep <- round(run_parameters$burnin + (peak - (365/12) * 3.5), 0)
  
  primary_coverage <- rep(0.75, length(mass_timestep))
  boost_coverage <- rep(0.75, length(mass_timestep)) # this is not reported in report 
  
  # add mass vaccination 
  if(scenario == 'mass' | scenario == 'mass+MDA'){
    
    pars <- malariasimulation::set_mass_pev(
      params, 
      profile = malariasimulation::r21_profile,
      timesteps = mass_timestep,
      coverages = primary_coverage,
      min_ages = vax_min_age, 
      max_ages = vax_max_age, 
      min_wait = 0,
      booster_spacing = 365,
      booster_coverage = matrix(boost_coverage, nrow = length(mass_timestep), ncol = 1),
      booster_profile = list(malariasimulation::r21_booster_profile),
      adult_scaling = adult_scaling, 
      adolesc_scaling = ado_scaling
    )
    
  } 
  
  if (scenario == 'mass+MDA'){
    
    MDAcov <- 0.8
    
    mdatimesteps <- mass_timestep
    AL_params_modified <- AL_params
    AL_params_modified$drug_rel_c <- 0
    params <- set_drugs(
      parameters = params, 
      list(AL_params, SP_AQ_params, DHA_PQP_params)
    )
    
    proppop_notpregnant <- 1 - 0.078/2 # from DHS data - see Get_pregnancy_rate.R in catchup repo
    
    # https://www.who.int/publications/i/item/9789241513104 - for drug, min_ages, and coverage (pregnancy)
    params <- set_mda(
      parameters = params,
      drug = 1, # AL for MDA 
      timesteps = mdatimesteps,
      coverages = rep(MDAcov * proppop_notpregnant, length(mdatimesteps)), # excluding pregnant women
      min_ages = rep(6 * round(365/12), length(mdatimesteps)), # starting from 6 months of age 
      max_ages = rep(100 * 365, length(mdatimesteps))
    ) 
  }
  
  
  inputs <- list(
    'param_list' = params,
    'site_name' = site_name,
    'scenario' = scenario,
    'parameter_draw' = parameter_draw,
    'population' = run_parameters$population,
    'burnin' =  run_parameters$burnin
  )
  
  return(inputs)
}
