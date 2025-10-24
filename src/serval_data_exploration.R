# Get the age-specific coverage 
coverage_GMB <- read_dta("R:/Kelly/massR21_serval/shared/serval_data/SERVAL cov GM collapsed 231025 shared with Lucy_ae.dta") %>%
  mutate(country = 'GMB')
coverage_BFA <- read_dta("R:/Kelly/massR21_serval/shared/serval_data/SERVAL BF vaccine coverage collapsed final 2share with Lucy_23102025_ae1.dta") %>%
  mutate(country = 'BFA',
         StudyID = as.character(StudyID),
         AgeinYears = age_primovac)
coverage <- bind_rows(coverage_GMB, coverage_BFA) %>%
  mutate(age_group = case_when(
      AgeinYears < 5 ~ 'Under 5',
      AgeinYears >= 5 & AgeinYears < 15 ~ '5-14',
      AgeinYears >= 15 ~ '15+'
  ))

coverage %>% janitor::tabyl(totvacc, country, age_group)


# Survey data 
survey_GMB <- read_dta("R:/Kelly/massR21_serval/shared/serval_data/TG_Survey_2024_270525.dta")
survey_BFA <- read_dta("R:/Kelly/massR21_serval/shared/serval_data/BF_Survey_2024_030825.dta")

# passive case detection data 
pcd_GMB <- read_dta("R:/Kelly/massR21_serval/shared/serval_data/TG_PCD_2024_12062025.dta")
pcd_BFA <- read_dta("R:/Kelly/massR21_serval/shared/serval_data/BF_PCD_2024_19082025.dta")
