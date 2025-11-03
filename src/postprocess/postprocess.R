# task to process the outputs of the runs 
library(orderly2)
library(ggplot2)
library(lubridate)
library(data.table)
library(dplyr)

orderly_resource(c('summarise_over_draws.R',
                   'get_averted.R',
                   'get_ve.R'))
source('summarise_over_draws.R')
source('get_averted.R')
source('get_ve.R')

# None
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA' &&
                   parameter:scenario == 'none')",
                   files = c(no_interventionBFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB' &&
                   parameter:scenario == 'none')",
                   files = c(no_interventionGMB.rds = 'model_outputs.rds'))
# Mass
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA' &&
                   parameter:scenario == 'mass' &&
                   parameter:description == 'BFA test range of scaling factors, avg coverage, limited scaling')",
                   files = c(massBFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB' &&
                   parameter:scenario == 'mass' &&
                   parameter:description == 'GMB test range of scaling factors, avg coverage, limited scaling'
                   )",
                   files = c(massGMB.rds = 'model_outputs.rds'))
# Mass+MDA
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'BFA' &&
                   parameter:scenario == 'mass+MDA' &&
                   parameter:description == 'BFA test range of scaling factors, avg coverage, limited scaling')",
                   files = c(massMDABFA.rds = 'model_outputs.rds'))
orderly_dependency(name = 'run_simulations',
                   "latest(parameter:country == 'GMB' &&
                   parameter:scenario == 'mass+MDA' &&
                   parameter:description == 'GMB test range of scaling factors, avg coverage, limited scaling'
                   )",
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


# Get incidence per 100 person months over May-November 2024 
avgincibfa <- monthly %>% filter(site_name == 'BFA' & year == 2024 & age_grp == '0-100' & month >= 5 & month <=11 &
                     scenario == 'none') %>%
  group_by(u5_scaling, adult_scaling, ado_scaling, scenario, parameter_draw) %>%
  summarise(clinical = mean(clinical)*100 * 30) %>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling, ', ', u5_scaling)) %>%
  mutate(mean_clin = mean(clinical), median_clin = median(clinical)) %>%
  ggplot() + 
  geom_bar(aes(x = scenario, y = clinical, group = parameter_draw, fill = parameter_draw),
           position = 'dodge', stat = 'identity') #+ theme(legend.position = 'none')
ggsave('average_inci_BFA.png', avgincibfa)
avgincigmb <- monthly %>% filter(site_name == 'GMB' & year == 2024 & age_grp == '0-100' & month >= 5 & month <=11 &
                     scenario == 'none') %>%
  group_by(u5_scaling, adult_scaling, ado_scaling, scenario, parameter_draw) %>%
  summarise(clinical = mean(clinical)*100 * 30) %>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling, ', ', u5_scaling)) %>%
  mutate(mean_clin = mean(clinical), median_clin = median(clinical)) %>%
  ggplot() + 
  geom_bar(aes(x = scenario, y = clinical, group = parameter_draw, fill = parameter_draw),
           position = 'dodge', stat = 'identity')
ggsave('average_inci_GMB.png', avgincigmb)
# ggplot(bfamonthly %>% filter(site_name == 'BFA' & year == 2024 & age_grp == '0-100' & month >= 5 & month <=11 & parameter_draw == 0)) +
#   # No int groups
#   geom_line(aes(x = month,
#                 y = clinical_baseline),
#                 color = 'darkgreen', alpha = 0.4) +
#   geom_point(aes(x = month,
#                  y = clinical_baseline),
#                  color = 'darkgreen', alpha = 0.4)  +
#   # Vaccination groups
#   geom_line(aes(x = month,
#                 y = clinical,
#                 color = scenario), alpha = 0.4) +
#   geom_point(aes(x = month,
#                  y = clinical,
#                  color = scenario), alpha = 0.4)  +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_wrap(~adult_scaling + ado_scaling + u5_scaling) + theme_minimal()
# ggplot(gmbmonthly %>% filter(site_name == 'GMB' & year == 2024 & age_grp == '0-100' & month >= 5 & month <=11 & parameter_draw == 0)) +
#   # No int groups
#   geom_line(aes(x = month, 
#                 y = clinical_baseline, 
#                 color = 'darkgreen'), alpha = 0.4) +
#   geom_point(aes(x = month, 
#                  y = clinical_baseline, 
#                  color = 'darkgreen'), alpha = 0.4)  +
#   # Vaccination groups
#   geom_line(aes(x = month, 
#                 y = clinical, 
#                 color = scenario), alpha = 0.4) +
#   geom_point(aes(x = month, 
#                  y = clinical, 
#                  color = scenario), alpha = 0.4)  +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_wrap(~adult_scaling + ado_scaling + u5_scaling) + theme_minimal()
target_irrs <- data.frame(
  target = c(0.4, 0.73, 0.7, 0.51, 0.53, 0.48),
  country = c(rep('BFA',3), rep('GMB',3)),
  age_group = rep(c('u5','5-14','15+'), 2)
)
# bfa_ve_agg <- bfa_ve %>%
#   group_by(scenario, age_grp, adult_scaling, ado_scaling, u5_scaling) %>%
#   summarize((across(IRR, 
#              list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
#                   median = ~quantile(.x, 0.5, na.rm = TRUE),
#                   upper = ~quantile(.x, 0.975, na.rm = TRUE)),
#              .names = "{.col}_{.fn}") ) %>%
#   # rename those variables with _median to be just the variable name 
#   dplyr::rename_with(.fn = \(x)sub("_median","", x)) )
bfa_ve <- bfa_ve %>%
  mutate(adult_scaling = paste0('Adult: ', adult_scaling),
         ado_scaling = paste0('Ado: ', ado_scaling),
         u5_scaling = paste0('U5: ',u5_scaling)) %>%
  mutate(age_grp = factor(age_grp, levels = c('0-5','5-15','15-100','0-100')))
gmb_ve <- gmb_ve%>%
  mutate(adult_scaling = paste0('Adult: ', adult_scaling),
         ado_scaling = paste0('Ado: ', ado_scaling),
         u5_scaling = paste0('U5: ',u5_scaling)) %>%
  mutate(age_grp = factor(age_grp, levels = c('0-5','5-15','15-100','0-100')))
# Plot IRR (int/none)
irrbfa <- ggplot(bfa_ve %>% filter(parameter_draw == 0, u5_scaling == 'U5: 1')) +
  geom_col(aes(x = scenario,
               y = IRR,
               fill = age_grp),
           position = 'dodge') +
  geom_hline(data = target_irrs %>% filter(country == 'BFA'), 
             aes(yintercept = target, color = age_group), linetype = 2, linewidth = 1) +
  scale_fill_manual(values = c('0-100' = 'tan',
                               '0-5'='limegreen',
                               '5-15'='purple',
                               '15-100'='darkblue'))+
  scale_color_manual(values = c('0-100' = 'tan',
                                'u5'='limegreen',
                                '5-14'='purple',
                                '15+'='darkblue'))+
  facet_grid(adult_scaling ~ ado_scaling) + 
  labs(title = 'BFA')
ggsave(paste0('plots/IRR_BFA.png'), irrbfa, height = 12, width = 20) # 0.1 and 0.1 for adults and ado are good, nothing is good for U5s
irrgmb <- ggplot(gmb_ve %>% filter(parameter_draw == 0, u5_scaling == 'U5: 1')) +
  geom_col(aes(x = scenario,
               y = IRR,
               fill = age_grp),
           position = 'dodge') +
  geom_hline(data = target_irrs %>% filter(country == 'GMB'), 
             aes(yintercept = target, color = age_group), linetype = 2, linewidth = 1) +
  scale_fill_manual(values = c('0-100' = 'tan',
                               '0-5'='limegreen',
                               '5-15'='purple',
                               '15-100'='darkblue'))+
  scale_color_manual(values = c('0-100' = 'tan',
                                'u5'='limegreen',
                                '5-14'='purple',
                                '15+'='darkblue'))+
  facet_grid(adult_scaling ~ ado_scaling) +
  labs(title = 'GMB')
ggsave(paste0('plots/IRR_GMB.png'), irrgmb, height = 12, width = 20)


# Aggregate monthly dfs over parameter draws ----
bfamonthly_agg <- bfamonthly %>%
  dplyr::mutate(date = lubridate::mdy(paste(month, '01', year, sep = '-'))) %>%
  mutate(clinical_monthly = clinical * 30 * 100) %>%
  summarise_over_draws_fast('date', 'month', 'year', 'ado_scaling', 'adult_scaling', 'u5_scaling') %>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling, ', ', u5_scaling))
gmbmonthly_agg <- gmbmonthly %>%
  dplyr::mutate(date = lubridate::mdy(paste(month, '01', year, sep = '-'))) %>%
  mutate(clinical_monthly = clinical * 30 * 100) %>%
  summarise_over_draws_fast('date', 'month', 'year', 'ado_scaling', 'adult_scaling', 'u5_scaling') %>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling, ', ', u5_scaling))
monthly_agg <- bind_rows(bfamonthly_agg, gmbmonthly_agg) # combine for ease of plotting (only 1 df)

saveRDS(bfamonthly_agg, 'bfamonthly_agg.rds')
saveRDS(gmbmonthly_agg, 'gmbmonthly_agg.rds')

# Aggregate annual df over parameter draws 
annual_agg <- annual %>%
  mutate(clinical_annual = clinical * 365 * 100) %>%
  summarise_over_draws_fast('year', 'ado_scaling', 'adult_scaling', 'u5_scaling')%>%
  mutate(scaling = paste0(ado_scaling, ', ', adult_scaling, ', ', u5_scaling))



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
                                   yearfilter = 2024,
                                   age_grpfilter = '0-100'){
  ggplot(monthly_agg %>% filter(site_name == site & year == yearfilter & age_grp == age_grpfilter)) +
    # Baseline
    geom_ribbon(aes(x = date,
                    ymin = clinical_baseline_lower * 30 * 100,
                    ymax = clinical_baseline_upper * 30 * 100),
                fill = 'green', alpha = 0.1) +
    geom_line(aes(x = date,
                  y = clinical_baseline * 30 * 100),
              color = 'green', alpha = 0.4) +
    geom_point(aes(x = date,
                   y = clinical_baseline * 30 * 100),
               color = 'darkgreen', alpha = 0.4)  +
    # Vaccination groups
    geom_ribbon(aes(x = date,
                    ymin = clinical_lower * 30 * 100,
                    ymax = clinical_upper * 30 * 100,
                    fill = scenario), alpha = 0.1) +
    geom_line(aes(x = date,
                  y = clinical * 30 * 100,
                  color = scenario), alpha = 0.4) +
    geom_point(aes(x = date,
                   y = clinical * 30 * 100,
                   color = scenario), alpha = 0.4)  +
    scale_x_date(breaks = '6 months',
                 labels = scales::label_date_short()) +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~scaling) + theme_minimal() + 
    labs(title = site,
         y = 'Clinical incidence per 100 person-months')

  age_grpname = case_when(age_grpfilter == '0-100' ~ 'all_ages',
                          TRUE ~ stringr::str_replace(age_grpfilter, '-', ''))

  ggsave(paste0('plots/clin_monthly_', site, '_', age_grpname, '.png'), height = 12, width = 20)
}
# BFA clinical incidence per month
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2024,
                       age_grpfilter = '0-100')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2024,
                       age_grpfilter = '0-5')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2024,
                       age_grpfilter = '5-15')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'BFA',
                       yearfilter = 2024,
                       age_grpfilter = '15-100')
# GMB clinical incidence per month
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2024,
                       age_grpfilter = '0-100')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2024,
                       age_grpfilter = '0-5')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2024,
                       age_grpfilter = '5-15')
plot_clin_inci_bymonth(monthly_agg,
                       site = 'GMB',
                       yearfilter = 2024,
                       age_grpfilter = '15-100')


# Monthly prevalence ----
# BFA
prev_bfa <- ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_baseline_lower, ymax = pcr_prevalence_0_100_baseline_upper),
              fill = 'green', alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100_baseline), color = 'green') +
  geom_point(aes(x = date, y = pcr_prevalence_0_100_baseline), color = 'darkgreen')  + 
  
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = date, y = pcr_prevalence_0_100, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()+ 
  labs(title = 'BFA')
ggsave('plots/prev_monthly_BFA.png', prev_bfa, height = 12, width = 20)
# GMB
prev_gmb <- ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_baseline_lower, ymax = pcr_prevalence_0_100_baseline_upper),
              fill = 'green', alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100_baseline), color = 'green') +
  geom_point(aes(x = date, y = pcr_prevalence_0_100_baseline), color = 'darkgreen')  + 
  
  geom_ribbon(aes(x = date, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = date, y = pcr_prevalence_0_100, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal() + 
  labs(title = 'GMB')
ggsave('plots/prev_monthly_GMB.png', prev_gmb, height = 12, width = 20)



# Cases averted ----
# BFA
ca_bfa <- ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = cases_averted_lower, ymax = cases_averted_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = cases_averted, color = scenario)) +
  geom_point(aes(x = date, y = cases_averted, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/casesaverted_monthly_BFA.png', ca_bfa, height = 12, width = 20)
# GMB
ca_gmb <- ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = cases_averted_lower, ymax = cases_averted_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = cases_averted, color = scenario)) +
  geom_point(aes(x = date, y = cases_averted, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/casesaverted_monthly_GMB.png', ca_gmb, height = 12, width = 20)

# Prevalence difference  ----
# BFA
prevdiffbfa <- ggplot(monthly_agg %>% filter(site_name == 'BFA' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = prev_0_100_diff_lower, ymax = prev_0_100_diff_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = prev_0_100_diff, color = scenario)) +
  geom_point(aes(x = date, y = prev_0_100_diff, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/prev_0_100_diff_monthly_BFA.png', prevdiffbfa, height = 12, width = 20)
# GMB
prevdiffgmb <- ggplot(monthly_agg %>% filter(site_name == 'GMB' & year <= 2025 & age_grp == '0-100')) +
  geom_ribbon(aes(x = date, ymin = prev_0_100_diff_lower, ymax = prev_0_100_diff_upper, 
                  fill = scenario), alpha = 0.3) + 
  geom_line(aes(x = date, y = prev_0_100_diff, color = scenario)) +
  geom_point(aes(x = date, y = prev_0_100_diff, color = scenario))  + 
  scale_x_date(breaks = '6 months', labels = scales::label_date_short()) +
  facet_wrap(~scaling, scales = 'free') + theme_minimal()
ggsave('plots/prev_0_100_diff_monthly_GMB.png', prevdiffgmb, height = 12, width = 20)




# Annual plots ----
prevannbfa <- ggplot(annual_agg %>% filter(site_name == 'BFA' & age_grp == '0-100')) +
  geom_ribbon(aes(x = year, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper,
                  fill = scenario), 
              alpha = 0.3) + 
  geom_line(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) + 
  facet_wrap(~scaling) + theme_minimal()
ggsave('plots/prev_annual_BFA.png', prevannbfa, height = 12, width = 20)
prevanngmb <- ggplot(annual_agg %>% filter(site_name == 'GMB' & age_grp == '0-100')) +
  geom_ribbon(aes(x = year, ymin = pcr_prevalence_0_100_lower, ymax = pcr_prevalence_0_100_upper,
                  fill = scenario), 
              alpha = 0.3) + 
  geom_line(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) +
  geom_point(aes(x = year, y = pcr_prevalence_0_100, color = scenario)) + 
  facet_wrap(~scaling)+ theme_minimal()
ggsave('plots/prev_annual_GMB.png', prevanngmb, height = 12, width = 20)


# for calibration 
# monthly_agg %>% ungroup() %>%
#   filter(year == 2024 & age_grp == '0-100' & month >=5 & month <= 11 ) %>% 
#   group_by(site_name, scenario, ado_scaling, adult_scaling) %>%
#   summarise(across(c(clinical_monthly, clinical_monthly_lower, clinical_monthly_upper, 
#                      pcr_prevalence_0_100, pcr_prevalence_0_100_lower, pcr_prevalence_0_100_upper),
#                                       list(mean = ~mean(.x, na.rm = TRUE),
#                                            median = ~quantile(.x, 0.5, na.rm = TRUE)),
#                                       .names = "{.col}_{.fn}") ) %>% t()



