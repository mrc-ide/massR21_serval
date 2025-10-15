# task to process the outputs of the runs 
library(orderly2)
library(ggplot2)
library(lubridate)
library(data.table)
library(dplyr)

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

# Read in data and combine ----
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
                     lapply(massMDAGMB, `[[`, 1)) %>%
  mutate(cases = clinical * person_days,
         dalys = dalys * person_days, 
         deaths = mortality * person_days, 
         severe = severe * person_days,
         age_grp = paste0(age_lower, '-',age_upper))
# daily <- bind_rows(lapply(noneBFA, `[[`, 3), lapply(noneGMB, `[[`, 3))
annual <- bind_rows(lapply(noneBFA, `[[`, 2), 
                    lapply(noneGMB, `[[`, 2), 
                    lapply(massBFA, `[[`, 2), 
                    lapply(massGMB, `[[`, 2), 
                    lapply(massMDABFA, `[[`, 2), 
                    lapply(massMDAGMB, `[[`, 2)) %>%
  mutate(cases = clinical * person_days,
         dalys = dalys * person_days, 
         deaths = mortality * person_days, 
         severe = severe * person_days,
         age_grp = paste0(age_lower, '-',age_upper))

# Vaccine efficacy ----
bfa_ve <- get_ve(monthly, 
                 site = 'BFA') %>%
  mutate(age_grp = paste0(age_lower, '-',age_upper))
gmb_ve <- get_ve(monthly, 
                 site = 'GMB') %>%
  mutate(age_grp = paste0(age_lower, '-',age_upper))


# Get outcomes averted by month ----
bfamonthly <- get_averted(monthly, 
                          site = 'BFA') %>%
  left_join(bfa_ve)

gmbmonthly <- get_averted(monthly, 
                          site = 'GMB') %>%
  left_join(gmb_ve)



ggplot(bfamonthly %>% filter(site_name == 'BFA' & year == 2020 & age_grp == '0-100' & month >= 5 & month <=11 & parameter_draw == 0)) +
  # No int groups
  geom_line(aes(x = month, 
                y = clinical_baseline), 
                color = 'darkgreen', alpha = 0.4) +
  geom_point(aes(x = month, 
                 y = clinical_baseline), 
                 color = 'darkgreen', alpha = 0.4)  +
  # Vaccination groups
  geom_line(aes(x = month, 
                y = clinical, 
                color = scenario), alpha = 0.4) +
  geom_point(aes(x = month, 
                 y = clinical, 
                 color = scenario), alpha = 0.4)  +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~adult_scaling + ado_scaling) + theme_minimal()
ggplot(monthly %>% filter(site_name == 'GMB' & year == 2020 & age_grp == '0-100' & month >= 5 & month <=11 & parameter_draw == 0)) +
  # No int groups
  geom_line(aes(x = month, 
                y = clinical_baseline, 
                color = 'darkgreen'), alpha = 0.4) +
  geom_point(aes(x = month, 
                 y = clinical_baseline, 
                 color = 'darkgreen'), alpha = 0.4)  +
  # Vaccination groups
  geom_line(aes(x = month, 
                y = clinical, 
                color = scenario), alpha = 0.4) +
  geom_point(aes(x = month, 
                 y = clinical, 
                 color = scenario), alpha = 0.4)  +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~adult_scaling + ado_scaling) + theme_minimal()
# Plot IRR (int/none)
ggplot(bfa_ve %>% filter(parameter_draw == 0)) +
  geom_col(aes(x = age_grp,
               y = IRR,
               color = scenario,
               fill = scenario),
           position = 'dodge') +
  facet_wrap(~adult_scaling + ado_scaling) + 
  labs(title = 'BFA')
ggsave(paste0('plots/IRR_BFA.png'), height = 12, width = 20)
ggplot(gmb_ve %>% filter(parameter_draw == 0)) +
  geom_col(aes(x = age_grp,
               y = IRR,
               color = scenario,
               fill = scenario),
           position = 'dodge') +
  facet_wrap(~adult_scaling + ado_scaling) +
  labs(title = 'GMB')
ggsave(paste0('plots/IRR_GMB.png'), height = 12, width = 20)


# Aggregate monthly dfs over parameter draws ----
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


# Aggregate annual df over parameter draws 
annual_agg <- annual %>%
  mutate(clinical_annual = clinical * 365 * 100) %>%
  summarise_over_draws_fast('year', 'ado_scaling', 'adult_scaling')%>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling))



# # Plot summary vaccine efficacy and IRR over the time period as the trial, by age group  ----
# Plot IRR (int/none)
# ggplot(monthly_agg %>% filter(site_name=='BFA')) + 
#   geom_col(aes(x = age_grp, 
#                y = IRR, 
#                color = scenario, 
#                fill = scenario),
#            position = 'dodge') + 
#   geom_errorbar(aes(x = age_grp, 
#                ymin = IRR_lower,
#                ymax = IRR_upper, 
#                group = scenario),
#                color = 'black', width = 0.5,
#            position = 'dodge') + 
#   facet_wrap(~scaling)
# ggplot(monthly_agg %>% filter(site_name == 'GMB')) + 
#   geom_col(aes(x = age_grp, 
#                y = IRR, 
#                color = scenario, 
#                fill = scenario),
#            position = 'dodge') + 
#   geom_errorbar(aes(x = age_grp, 
#                     ymin = IRR_lower,
#                     ymax = IRR_upper, 
#                     group = scenario),
#                 color = 'black', width = 0.5,
#                 position = 'dodge') + 
#   facet_wrap(~scaling)


# Plot clin inci by month ----
plot_clin_inci_bymonth <- function(monthly_agg,
                                   site,
                                   yearfilter = 2020,
                                   age_grpfilter = '0-100'){
  ggplot(monthly_agg %>% filter(site_name == site & year == yearfilter & age_grp == age_grpfilter)) +
    # Baseline
    geom_ribbon(aes(x = date, 
                    ymin = clinical_baseline_lower, 
                    ymax = clinical_baseline_upper),
                fill = 'green', alpha = 0.1) +
    geom_line(aes(x = date, 
                  y = clinical_baseline), 
              color = 'green', alpha = 0.4) +
    geom_point(aes(x = date, 
                   y = clinical_baseline), 
               color = 'darkgreen', alpha = 0.4)  +
    # Vaccination groups
    geom_ribbon(aes(x = date, 
                    ymin = clinical_lower, 
                    ymax = clinical_upper,
                    fill = scenario), alpha = 0.1) +
    geom_line(aes(x = date, 
                  y = clinical, 
                  color = scenario), alpha = 0.4) +
    geom_point(aes(x = date, 
                   y = clinical, 
                   color = scenario), alpha = 0.4)  +
    scale_x_date(breaks = '6 months', 
                 labels = scales::label_date_short()) +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~scaling) + theme_minimal()
  
  age_grpname = case_when(age_grpfilter == '0-100' ~ 'all_ages',
                          TRUE ~ stringr::str_replace(age_grpfilter, '-', ''))
  
  ggsave(paste0('plots/clin_monthly_', site, '_', age_grpname, '.png'), height = 12, width = 20)
}
# BFA clinical incidence per month
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2020,
                       age_grpfilter = '0-100')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2020,
                       age_grpfilter = '0-5')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2020,
                       age_grpfilter = '5-15')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2020,
                       age_grpfilter = '15-100')
# GMB clinical incidence per month 
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2020,
                       age_grpfilter = '0-100')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2020,
                       age_grpfilter = '0-5')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2020,
                       age_grpfilter = '5-15')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2020,
                       age_grpfilter = '15-100')


# Monthly prevalence ----
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



# Cases averted ----
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

# Prevalence difference  ----
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




# Annual plots ----
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


# for calibration 
# monthly_agg %>% ungroup() %>%
#   filter(year == 2024 & age_grp == '0-100' & month >=5 & month <= 11 ) %>% 
#   group_by(site_name, scenario, ado_scaling, adult_scaling) %>%
#   summarise(across(c(clinical_monthly, clinical_monthly_lower, clinical_monthly_upper, 
#                      pcr_prevalence_0_100, pcr_prevalence_0_100_lower, pcr_prevalence_0_100_upper),
#                                       list(mean = ~mean(.x, na.rm = TRUE),
#                                            median = ~quantile(.x, 0.5, na.rm = TRUE)),
#                                       .names = "{.col}_{.fn}") ) %>% t()



