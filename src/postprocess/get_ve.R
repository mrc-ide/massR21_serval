get_ve <- function(df, # country-specific monthly dataset
                   site){
  
  df2 <- df %>% ungroup() %>%
    filter(site_name == site) %>%
    filter(year == 2020 & (month >=5 & month <= 11)) %>%
    group_by(age_lower, age_upper, parameter_draw, scenario, adult_scaling, ado_scaling) %>%
    summarize(clinical_mean = weighted.mean(clinical, person_days)) %>%
    ungroup()
  
  # Then calculate VE
  noneinci <- df2 %>% 
    filter(scenario == 'none') %>%
    select(age_lower, age_upper, parameter_draw,
           clinical_mean) %>%
    rename(clinical_mean_none = clinical_mean)
  
  all <- left_join(df2 %>% filter(scenario != 'none'),
                   noneinci)
  
  ve_by_age <- all %>%
    group_by(age_lower, age_upper, parameter_draw, scenario, adult_scaling, ado_scaling) %>%
    mutate(
      IRR = clinical_mean / clinical_mean_none, 
      ve = (1 - IRR) 
    )
  
  return(ve_by_age)
}
