# extract site data -- written by Lucy 

if (!dir.exists('inputs')) {dir.create("inputs")}

this_iso3c <- 'BFA'
site_data<- site::fetch_site(iso3c = this_iso3c)
this_name_1 <-'Centre'
this_country <- 'Burkina Faso'
this_ur <- 'rural'


site <- site::subset_site(
  site = site_data,
  site_filter = data.frame(
    country= this_country,
    iso3c = this_iso3c,
    name_1 = this_name_1,
    urban_rural =  this_ur
  )
)

intv_long <- pivot_longer(site$interventions |> dplyr::select(year,tx_cov:pmc_cov), cols = tx_cov:pmc_cov)

# ggplot(intv_long) +
#   geom_line(aes(x=year, y=value, color=name)) +
#   theme_bw()

## modify some interventions so they are more constant, true for the trial area etc.
site$interventions$r21_cov <-0

saveRDS(site, 'inputs/bfa_serval_site.rds')


#####
# Gambia
this_iso3c <- 'GMB'
site_data<- site::fetch_site(iso3c = this_iso3c)
this_name_1 <-'Upper River'
this_country <- 'Gambia'
this_ur <- 'rural'


site <- site::subset_site(
  site = site_data,
  site_filter = data.frame(
    country= this_country,
    iso3c = this_iso3c,
    name_1 = this_name_1,
    urban_rural =  this_ur
  )
)

intv_long <- pivot_longer(site$interventions |> dplyr::select(year,tx_cov:pmc_cov), cols = tx_cov:pmc_cov)

# ggplot(intv_long) +
#   geom_line(aes(x=year, y=value, color=name)) +
#   theme_bw()

## modify some interventions so they are more constant, true for the trial area etc.
site$interventions$r21_cov <-0
site$interventions$irs_cov <-0

saveRDS(site, 'inputs/gmb_serval_site.rds')

# 
# #### First pass - constant intervention coverage:
# site<- readRDS('inputs/gmb_serval_site.rds')
# site$interventions |>
#   filter(year>=2022) |>
#   summarise(tx_cov = mean(tx_cov),
#             prop_act = mean(prop_act))
# 
# site$seasonality$seasonality_parameters
# 
# #### First pass - constant intervention coverage:
# site<- readRDS('inputs/bfa_serval_site.rds')
# site$interventions |>
#   filter(year>=2022) |>
#   summarise(tx_cov = mean(tx_cov),
#             prop_act = mean(prop_act))
# 
# site$seasonality$seasonality_parameters
