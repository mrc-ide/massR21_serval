# Workflow script 
library(orderly2)
library(hipercow)

hipercow::task_create_expr(orderly2::orderly_run('create_inputs'))
task_log_show('68998c0424bd9d312acc94e2dd0d08d9')
# # Calibration
# bfa_runs3 <- task_create_expr(orderly2::orderly_run('run_simulations',
#                                                    parameters = list(country = 'BFA',
#                                                                      scenario = 'none',
#                                                                      description = 'test runs')),
#                              resources = hipercow_resources(cores = 6))
# task_log_show(bfa_runs3)
# 
# gmb_runs3 <- task_create_expr(orderly2::orderly_run('run_simulations',
#                                                    parameters = list(country = 'GMB',
#                                                                      scenario = 'none',
#                                                                      description = 'test runs')),
#                              resources = hipercow_resources(cores = 6))
# task_log_show(gmb_runs3)


# Scenarios
ncores = 32
# Mass
bfa_runs_lim <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                    parameters = list(country = 'BFA',
                                                                      scenario = 'mass',
                                                                      description = 'BFA test range of scaling factors, avg coverage, limited scaling')),
                              resources = hipercow_resources(cores = ncores))
task_log_show(bfa_runs)
task_log_show(bfa_runs_lim)

gmb_runs_lim <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                    parameters = list(country = 'GMB',
                                                                      scenario = 'mass',
                                                                      description = 'GMB test range of scaling factors, avg coverage, limited scaling')),
                              resources = hipercow_resources(cores = ncores))
task_log_show(gmb_runs)
task_log_show(gmb_runs_lim)
# Mass+MDA
bfa_runs_both_lim <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'BFA',
                                                                     scenario = 'mass+MDA',
                                                                     description = 'BFA test range of scaling factors, avg coverage, limited scaling')),
                             resources = hipercow_resources(cores = ncores))
task_log_show(bfa_runs_both)
task_log_show(bfa_runs_both_lim)
gmb_runs_both_lim <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'GMB',
                                                                     scenario = 'mass+MDA',
                                                                     description = 'GMB test range of scaling factors, avg coverage, limited scaling')),
                             resources = hipercow_resources(cores = ncores))
task_log_show(gmb_runs_both)
task_log_show(gmb_runs_both_lim)
# No vaccination 
bfa_none <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'BFA',
                                                                     scenario = 'none',
                                                                     description = 'BFA no vax')),
                             resources = hipercow_resources(cores = ncores))
task_log_show(bfa_none)

gmb_none <- task_create_expr(orderly2::orderly_run('run_simulations',
                                                   parameters = list(country = 'GMB',
                                                                     scenario = 'none',
                                                                     description = 'GMB no vax')),
                             resources = hipercow_resources(cores = ncores))
task_log_show(gmb_none)



# Processing
task_process <- hipercow::task_create_expr(orderly2::orderly_run(name = 'postprocess'))
task_log_show(task_process)
