# Workflow script 
library(orderly2)
library(hipercow)

orderly2::orderly_run('create_inputs')

bfa_runs2 <- task_create_expr(orderly2::orderly_run('run_simulations',
                                       parameters = list(country = 'BFA',
                                                         scenario = 'none',
                                                         description = 'test runs')),
                             resources = hipercow_resources(cores = 6))
task_log_show(bfa_runs)
task_log_show(bfa_runs2)
