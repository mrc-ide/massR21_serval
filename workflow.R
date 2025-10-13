# Workflow script 
library(orderly2)
library(hipercow)

orderly2::orderly_run('create_inputs')

# Calibration
bfa_runs3 <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'BFA',
                                                                     scenario = 'none',
                                                                     description = 'test runs')),
                             resources = hipercow_resources(cores = 6))
task_log_show(bfa_runs3)

gmb_runs3 <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'GMB',
                                                                     scenario = 'none',
                                                                     description = 'test runs')),
                             resources = hipercow_resources(cores = 6))
task_log_show(gmb_runs3)


# SCenarios
ncores = 30
bfa_runs <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                    parameters = list(country = 'BFA',
                                                                      scenario = 'mass',
                                                                      description = 'BFA test range of scaling factors')),
                              resources = hipercow_resources(cores = ncores))
task_log_show(bfa_runs)

gmb_runs <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                    parameters = list(country = 'GMB',
                                                                      scenario = 'mass',
                                                                      description = 'GMB test range of scaling factors')),
                              resources = hipercow_resources(cores = ncores))
task_log_show(gmb_runs)

bfa_runs_both <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'BFA',
                                                                     scenario = 'mass+MDA',
                                                                     description = 'BFA test range of scaling factors')),
                             resources = hipercow_resources(cores = ncores))
task_log_show(bfa_runs_both)

gmb_runs_both <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'GMB',
                                                                     scenario = 'mass+MDA',
                                                                     description = 'GMB test range of scaling factors')),
                             resources = hipercow_resources(cores = ncores))
task_log_show(gmb_runs_both)


# Processing
orderly2::orderly_run(name = 'postprocess')
