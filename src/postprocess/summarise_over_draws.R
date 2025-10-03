#' @param ... is list of time vars to group by (month, year, or year)
summarise_over_draws <- function(df, 
         ...){
  # Capture ... as a list and convert to character
  time_vars <- unlist(list(...))
  
  # Define the fixed grouping variables
  fixed_vars <- c("age_lower", "age_upper", "site_name", "scenario")
  
  # Combine all grouping variables
  all_group_vars <- c(time_vars, fixed_vars)
  
  df %>%
    dplyr::group_by(across(dplyr::all_of(all_group_vars))) %>%
    dplyr::summarise(across(c(contains('clinical'), clinical:dalys, pcr_prevalence_0_5:pcr_prevalence_0_100), 
                     list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                          median = ~quantile(.x, 0.5, na.rm = TRUE),
                          upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                     .names = "{.col}_{.fn}") ) %>%
    # rename those variables with _median to be just the variable name 
    dplyr::rename_with(.fn = \(x)sub("_median","", x)) %>%
    dplyr::mutate(age_grp = paste0(age_lower, '-', age_upper))
  
  
}
