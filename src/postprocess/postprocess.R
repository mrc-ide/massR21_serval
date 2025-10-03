# task to process the outputs of the runs 

library(ggplot2)
library(lubridate)


orderly_resource('summarise_over_draws.R')
source('summarise_over_draws.R')

orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA')",
                   files = c(no_interventionBFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB')",
                   files = c(no_interventionGMB.rds = 'model_outputs.rds'))

noneBFA <- readRDS('no_interventionBFA.rds')
noneGMB <- readRDS('no_interventionGMB.rds')

monthly <- bind_rows(lapply(noneBFA, `[[`, 1), lapply(noneGMB, `[[`, 1))
# daily <- bind_rows(lapply(noneBFA, `[[`, 3), lapply(noneGMB, `[[`, 3))
annual <- bind_rows(lapply(noneBFA, `[[`, 2), lapply(noneGMB, `[[`, 2))

monthly_agg <- monthly %>%
  dplyr::mutate(date = lubridate::mdy(paste(month, '01', year, sep = '-'))) %>%
  mutate(clinical_monthly = clinical * 30 * 100) %>%
  summarise_over_draws('date','month', 'year')

# daily_agg <- daily %>%
#   mutate(clinical_monthly = clinical * 30 * 100) %>%
#   mutate(clinical_annual = clinical * 365 * 100) %>%
#   group_by(time, month, year, age_lower, age_upper, site_name, scenario) %>%
#   summarise(across(c(clinical:dalys, clinical_monthly, clinical_annual, pcr_prevalence_0_5:pcr_prevalence_0_100),
#                    list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
#                         median = ~quantile(.x, 0.5, na.rm = TRUE),
#                         upper = ~quantile(.x, 0.975, na.rm = TRUE)),
#                    .names = "{.col}_{.fn}") ) %>%
#   # rename those variables with _median to be just the variable name
#   rename_with(.fn = \(x)sub("_median","", x))%>%
#   mutate(age_grp = paste0(age_lower, '-', age_upper))

annual_agg <- annual %>%
  mutate(clinical_annual = clinical * 365 * 100) %>%
  summarise_over_draws('year')
  
#Annual
ggplot(annual_agg) +
  geom_ribbon(aes(x = year, ymin = clinical_annual_lower, ymax = clinical_annual_upper, color = age_grp, fill = age_grp), alpha = 0.3) + 
  geom_line(aes(x = year, y = clinical_annual, color = age_grp)) +
  geom_point(aes(x = year, y = clinical_annual, color = age_grp)) + 
  facet_wrap(~site_name, scales = 'free')
ggplot(annual_agg) +
  geom_ribbon(aes(x = year, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper), alpha = 0.3) + 
  geom_line(aes(x = year, y = pcr_prevalence_0_100)) +
  geom_point(aes(x = year, y = pcr_prevalence_0_100)) + 
  facet_wrap(~site_name, scales = 'free')


ggplot(daily_agg %>% filter(age_lower == 0 & age_upper == 100)) + 
  geom_line(aes(x = time, y = pcr_prevalence_0_100))


ggplot(monthly_agg %>% filter(year < 2026 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = clinical_monthly_lower, ymax = clinical_monthly_upper, color = age_grp, fill = age_grp), alpha = 0.3) + 
  geom_line(aes(x = date, y = clinical_monthly, color = age_grp)) +
  geom_point(aes(x = date, y = clinical_monthly, color = age_grp))  + 
  facet_grid(site_name ~ age_grp, scales = 'free')
ggplot(monthly_agg %>% filter(year < 2026 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper, color = age_grp, fill = age_grp), alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100, color = age_grp)) +
  geom_point(aes(x = date, y = pcr_prevalence_0_100, color = age_grp))  + 
  facet_grid(site_name ~ age_grp, scales = 'free')


monthly_agg %>% ungroup() %>%
  filter(year == 2024 & age_grp == '0-100') %>% 
  group_by(site_name) %>%
  summarise(across(c(clinical_monthly, clinical_monthly_lower, clinical_monthly_upper, 
                     pcr_prevalence_0_100, pcr_prevalence_0_100_lower, pcr_prevalence_0_100_upper),
                                      list(mean = ~mean(.x, na.rm = TRUE),
                                           median = ~quantile(.x, 0.5, na.rm = TRUE)),
                                      .names = "{.col}_{.fn}") ) %>% t()
 


