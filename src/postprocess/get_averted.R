get_averted <- function(df, # monthly dataset from runs (x per person per day but averaged per month)
                        site){
  
  monthly_ <- monthly %>%
    filter(site_name == site & scenario != 'none')
  
  nonemonthly <- monthly %>% 
    filter(site_name == site & scenario == 'none') %>%
    rename_with(~paste0(.x, "_baseline"), 
                c(clinical:person_days, pcr_prevalence_0_5:pcr_prevalence_0_100, cases, deaths)) %>%
    select(month, year, age_lower, age_upper, parameter_draw, site_name, week, day,
           contains('baseline'))
  
  monthly_averted <- left_join(monthly_, nonemonthly) %>%
    mutate(dalys_averted = dalys_baseline - dalys, 
           cases_averted = cases_baseline - cases,
           deaths_averted = deaths_baseline - deaths, 
           severe_averted = severe_baseline - severe,
           clinical_diff = clinical_baseline - clinical, # cases/pop difference between vaccinated and not vaccinated
           prev_0_100_diff = pcr_prevalence_0_100_baseline - pcr_prevalence_0_100)# %>%
    # select(-contains('baseline'))
  
  return(monthly_averted)
}
