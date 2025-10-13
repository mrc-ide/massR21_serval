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

summarise_over_draws_fast <- function(df, ...) {
  time_vars <-  unlist(list(...))
  fixed_vars <- c("age_lower", "age_upper", "site_name", "scenario")
  all_group_vars <- c(time_vars, fixed_vars)
  
  # Convert to data.table
  dt <- as.data.table(df)
  
  # Select columns to summarize
  value_cols <- names(dt)[grepl('clinical|diff|averted|^(clinical|cases|deaths|dalys|pcr_prevalence)', names(dt))]
  value_cols <- setdiff(value_cols, all_group_vars)
  
  # Calculate quantiles using data.table 
  result <- dt[, {
    lower_vals <- lapply(.SD, function(x) quantile(x, 0.025, na.rm = TRUE))
    median_vals <- lapply(.SD, function(x) quantile(x, 0.5, na.rm = TRUE))
    upper_vals <- lapply(.SD, function(x) quantile(x, 0.975, na.rm = TRUE))
    
    # Create named list
    out_list <- c(
      setNames(lower_vals, paste0(names(lower_vals), "_lower")),
      setNames(median_vals, names(median_vals)),  # median without suffix
      setNames(upper_vals, paste0(names(upper_vals), "_upper"))
    )
    
    as.list(out_list)
  }, by = all_group_vars, .SDcols = value_cols]
  result[, age_grp := paste0(age_lower, '-', age_upper)]
  
  return(as.data.frame(result))
}
