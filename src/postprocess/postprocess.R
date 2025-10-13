# task to process the outputs of the runs 

library(ggplot2)
library(lubridate)
library(data.table)

orderly_resource(c('summarise_over_draws.R',
                   'get_averted.R'))
source('summarise_over_draws.R')
source('get_averted.R')

orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA' &&
                   parameter:scenario == 'none')",
                   files = c(no_interventionBFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB' &&
                   parameter:scenario == 'none')",
                   files = c(no_interventionGMB.rds = 'model_outputs.rds'))

orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA' &&
                   parameter:scenario == 'mass')",
                   files = c(massBFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB' &&
                   parameter:scenario == 'mass')",
                   files = c(massGMB.rds = 'model_outputs.rds'))

orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA' &&
                   parameter:scenario == 'mass+MDA')",
                   files = c(massMDABFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB' &&
                   parameter:scenario == 'mass+MDA')",
                   files = c(massMDAGMB.rds = 'model_outputs.rds'))


noneBFA <- readRDS('no_interventionBFA.rds') 
noneGMB <- readRDS('no_interventionGMB.rds')
massBFA <- readRDS('massBFA.rds')
massGMB <- readRDS('massGMB.rds')
massMDABFA <- readRDS('massMDABFA.rds')
massMDAGMB <- readRDS('massMDAGMB.rds')

dir.create('plots/')

monthly <- bind_rows(lapply(noneBFA, `[[`, 1), 
                     lapply(noneGMB, `[[`, 1),
                     lapply(massBFA, `[[`, 1),
                     lapply(massGMB, `[[`, 1),
                     lapply(massMDABFA, `[[`, 1),
                     lapply(massMDAGMB, `[[`, 1)) 
# daily <- bind_rows(lapply(noneBFA, `[[`, 3), lapply(noneGMB, `[[`, 3))
annual <- bind_rows(lapply(noneBFA, `[[`, 2), 
                    lapply(noneGMB, `[[`, 2), 
                    lapply(massBFA, `[[`, 2), 
                    lapply(massGMB, `[[`, 2), 
                    lapply(massMDABFA, `[[`, 2), 
                    lapply(massMDAGMB, `[[`, 2))

bfamonthly <- get_averted(monthly, 
                          site = 'BFA')

gmbmonthly <- get_averted(monthly, 
                          site = 'GMB')

# bfaannual <- annual %>% filter(site_name == 'BFA')
# gmbannual <- annual %>% filter(site_name == 'GMB')

bfamonthly_agg <- bfamonthly %>%
  dplyr::mutate(date = lubridate::mdy(paste(month, '01', year, sep = '-'))) %>%
  mutate(clinical_monthly = clinical * 30 * 100) %>%
  summarise_over_draws_fast('date', 'month', 'year', 'ado_scaling', 'adult_scaling') %>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling))
gmbmonthly_agg <- gmbmonthly %>%
  dplyr::mutate(date = lubridate::mdy(paste(month, '01', year, sep = '-'))) %>%
  mutate(clinical_monthly = clinical * 30 * 100) %>%
  summarise_over_draws_fast('date', 'month', 'year', 'ado_scaling', 'adult_scaling') %>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling))
monthly_agg <- bind_rows(bfamonthly_agg, gmbmonthly_agg) # combine for ease of plotting (only 1 df)

annual_agg <- annual %>%
  mutate(clinical_annual = clinical * 365 * 100) %>%
  summarise_over_draws_fast('year', 'ado_scaling', 'adult_scaling')%>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling))

#Annual
# ggplot(annual_agg %>% filter(site_name == 'BFA' & age_grp == '0-100')) +
#   geom_ribbon(aes(x = year, ymin = clinical_annual_lower, ymax = clinical_annual_upper, 
#                   color = scenario, fill = scenario), alpha = 0.15) + 
#   geom_line(aes(x = year, y = clinical_annual, color = scenario)) +
#   geom_point(aes(x = year, y = clinical_annual, color = scenario)) + 
#   facet_wrap(~site_name + scaling)
# ggplot(annual_agg %>% filter(site_name == 'GMB' & age_grp == '0-100')) +
#   geom_ribbon(aes(x = year, ymin = clinical_annual_lower, ymax = clinical_annual_upper, 
#                   color = scenario, fill = scenario), alpha = 0.15) + 
#   geom_line(aes(x = year, y = clinical_annual, color = scenario)) +
#   geom_point(aes(x = year, y = clinical_annual, color = scenario)) + 
#   facet_wrap(~site_name + scaling)

ggplot(annual_agg %>% filter(site_name == 'BFA' & age_grp == '0-100')) +
  geom_ribbon(aes(x = year, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper,
                  fill = scenario), 
              alpha = 0.3) + 
  geom_line(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) + 
  facet_wrap(~scaling) + theme_minimal()
ggsave('plots/prev_annual_BFA.png', height = 12, width = 20)
ggplot(annual_agg %>% filter(site_name == 'GMB' & age_grp == '0-100')) +
  geom_ribbon(aes(x = year, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper,
                  fill = scenario), 
              alpha = 0.3) + 
  geom_line(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) + 
  facet_wrap(~scaling)+ theme_minimal()
ggsave('plots/prev_annual_GMB.png', height = 12, width = 20)




# ggplot(daily_agg %>% filter(age_lower == 0 & age_upper == 100)) + 
#   geom_line(aes(x = time, y = pcr_prevalence_0_100))

# BFA
ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2024 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = clinical_monthly_lower, ymax = clinical_monthly_upper,
                  fill = scenario), alpha = 0.3) +
  geom_line(aes(x = date, y = clinical_monthly, color = scenario), alpha = 0.3) +
  geom_point(aes(x = date, y = clinical_monthly, color = scenario), alpha = 0.3)  +
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~scaling) + theme_minimal()
ggsave('plots/clin_monthly_BFA.png', height = 12, width = 20)
# GMB
ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2024 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = clinical_monthly_lower, ymax = clinical_monthly_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = clinical_monthly, color = scenario)) +
  geom_point(aes(x = date, y = clinical_monthly, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~scaling) + theme_minimal()
ggsave('plots/clin_monthly_GMB.png', height = 12, width = 20)

# BFA
ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = date, y = pcr_prevalence_0_100, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/prev_monthly_BFA.png', height = 12, width = 20)
# GMB
ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = date, y = pcr_prevalence_0_100, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/prev_monthly_GMB.png', height = 12, width = 20)



# Cases averted 
# BFA
ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = cases_averted_lower, ymax = cases_averted_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = cases_averted, color = scenario)) +
  geom_point(aes(x = date, y = cases_averted, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/casesaverted_monthly_BFA.png', height = 12, width = 20)
# GMB
ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = cases_averted_lower, ymax = cases_averted_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = cases_averted, color = scenario)) +
  geom_point(aes(x = date, y = cases_averted, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/casesaverted_monthly_GMB.png', height = 12, width = 20)

# Prevalence difference  
# BFA
ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = prev_0_100_diff_lower, ymax = prev_0_100_diff_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = prev_0_100_diff, color = scenario)) +
  geom_point(aes(x = date, y = prev_0_100_diff, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/prev_0_100_diff_monthly_BFA.png', height = 12, width = 20)
# GMB
ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = prev_0_100_diff_lower, ymax = prev_0_100_diff_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = prev_0_100_diff, color = scenario)) +
  geom_point(aes(x = date, y = prev_0_100_diff, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/prev_0_100_diff_monthly_GMB.png', height = 12, width = 20)


# for calibration 
# monthly_agg %>% ungroup() %>%
#   filter(year == 2024 & age_grp == '0-100' & month >=5 & month <= 11 ) %>% 
#   group_by(site_name, scenario, ado_scaling, adult_scaling) %>%
#   summarise(across(c(clinical_monthly, clinical_monthly_lower, clinical_monthly_upper, 
#                      pcr_prevalence_0_100, pcr_prevalence_0_100_lower, pcr_prevalence_0_100_upper),
#                                       list(mean = ~mean(.x, na.rm = TRUE),
#                                            median = ~quantile(.x, 0.5, na.rm = TRUE)),
#                                       .names = "{.col}_{.fn}") ) %>% t()



