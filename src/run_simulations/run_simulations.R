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
parameter_draw <- seq(0,5)#seq(0,50)
scenario <- orderlyparams$scenario#c('mass','mass+MDA','none')
combo <- list()
for (s in scenario) {
  for (p in parameter_draw) {
    combo[[length(combo) + 1]] <- list(scenario = s, parameter_draw = p)
  }
}

cluster_cores <- Sys.getenv("CCP_NUMCPUS")
message('number of cores: ', cluster_cores)

if (cluster_cores == "") {
  message("running in serial (on a laptop?)")
  
  results2 <- lapply(combo, 
                     function(combo) {
                       run_sim( 
                         site_data = site_data, # site inforamtion from site file, with calibrateD EIR
                         site_name = orderlyparams$country, # GMB or BFA
                         run_parameters, # small df of pop, burnin, etc.
                         combo$parameter_draw, # 0-50
                         orderlyparams$scenario)
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
                                     function(combo) {
                                       run_sim(
                                         site_data = site_data, # site inforamtion from site file, with calibrateD EIR
                                         site_name = orderlyparams$country, # GMB or BFA
                                         run_parameters, # small df of pop, burnin, etc.
                                         combo$parameter_draw, # 0-50
                                         orderlyparams$scenario)
                                     }
  )
  parallel::stopCluster(cl)

}

saveRDS(results2, 'model_outputs.rds')