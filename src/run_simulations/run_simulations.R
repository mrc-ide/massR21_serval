# Orderly task to run the simulations for each country
library(orderly2)
library(postie)
library(malariasimulation)
library(dplyr)

orderlyparams <- orderly_parameters(
  country = NULL,
  scenario = NULL,
  description = NULL
)

orderly_dependency(name = 'create_inputs',
                   "latest()",
                   c("run_parameters.rds",
                     "gmb_site_info.rds",
                     "bfa_site_info.rds"))

orderly_resource("run_sim.R")
source('run_sim.R')
orderly_shared_resource("parameterize_site.R")
source("parameterize_site.R")

run_parameters <- readRDS('run_parameters.rds')

if(orderlyparams$country == 'BFA'){
  site_data <- readRDS('bfa_site_info.rds')
} else if(orderlyparams$country == 'GMB'){
  site_data <- readRDS('gmb_site_info.rds')
  
}


# Run simulation
  
# send runs to cluster 
parameter_drawvec <- seq(0)
scenariovec <- orderlyparams$scenario#c('mass','mass+MDA','none')
adult_scalingvec <- if(orderlyparams$country == 'BFA' & orderlyparams$scenario != 'none') {
  c(0.05, 0.1, 0.15) 
  } else if(orderlyparams$country == 'GMB' & orderlyparams$scenario != 'none') {
    c(0.7, 0.8, 0.9)
  } else 1 
ado_scalingvec <- if(orderlyparams$country == 'BFA' & orderlyparams$scenario != 'none') {
  c(0.05, 0.1, 0.15) 
} else if(orderlyparams$country == 'GMB' & orderlyparams$scenario != 'none') {
  c(0.7, 0.8, 0.9)
} else 1
u5_scalingvec <- 1
combo <- list()
for (s in scenariovec) {
  for (p in parameter_drawvec) {
    for (adult in adult_scalingvec) {
      for (ado in ado_scalingvec) {
        for(child in u5_scalingvec) {
          combo[[length(combo) + 1]] <- list(scenario = s, 
                                             parameter_draw = p,
                                             adult_scaling = adult, 
                                             ado_scaling = ado,
                                             u5_scaling = child)
        }
      }
    }
  }
}
saveRDS(combo, 'combo_parameters.rds')

cluster_cores <- Sys.getenv("CCP_NUMCPUS")
message('number of cores: ', cluster_cores)

if (cluster_cores == "") {
  message("running in serial (on a laptop?)")
  
  results2 <- lapply(combo, 
                     function(c) {
                       run_sim( 
                         site_data = site_data, # site inforamtion from site file, with calibrateD EIR
                         site_name = orderlyparams$country, # GMB or BFA
                         run_parameters = run_parameters, # small df of pop, burnin, etc.
                         parameter_draw = c$parameter_draw, # 0-50
                         scenario = orderlyparams$scenario,
                         adult_scaling = c$adult_scaling, 
                         ado_scaling = c$ado_scaling,
                         u5_scaling = c$u5_scaling)
                     })
  
} else {

  message(sprintf("running in parallel on %s (on the cluster?)", cluster_cores))
  cl <- parallel::makeCluster(as.integer(cluster_cores),
                              outfile ="")
  invisible(parallel::clusterCall(cl, ".libPaths", .libPaths()))
  parallel::clusterCall(cl, function() {
    message('running')
    library(odin2)
    library(ggplot2)
    library(dust2)
    library(tidyverse)
    library(orderly2)
    library(malariasimulation)
    library(malariaEquilibrium)
    library(retry)
    library(postie)

    source("run_sim.R")
    source("parameterize_site.R")

    TRUE
  })
  parallel::clusterExport(cl, c("combo","site_data","run_parameters", "orderlyparams"))

  results2 <- parallel::clusterApply(cl,
                                     combo,
                                     function(c) {
                                       run_sim( 
                                         site_data = site_data, # site inforamtion from site file, with calibrateD EIR
                                         site_name = orderlyparams$country, # GMB or BFA
                                         run_parameters = run_parameters, # small df of pop, burnin, etc.
                                         parameter_draw = c$parameter_draw, # 0-50
                                         scenario = orderlyparams$scenario,
                                         adult_scaling = c$adult_scaling, 
                                         ado_scaling = c$ado_scaling,
                                         u5_scaling = c$u5_scaling)
                                     }
  )
  parallel::stopCluster(cl)

}

saveRDS(results2, 'model_outputs.rds')