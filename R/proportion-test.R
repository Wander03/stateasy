#' Calculates summary statistics for a sample of binary categorical data
#'
#' @param data_vec A vector of TRUE/FALSE or 0/1 data representing the sample
#' @param hypo_p Optionally, specify a hypothesized value for the proportion
#'
#' @return A dataframe with summary statistics 
#'
#' @importFrom dplyr rename
#'
#' @export
get_summary_statistics <- function(data_vec, hypo_p = -1) {
  
  n = length(data_vec)
  n_yes = sum(data_vec)
  n_no = n - n_yes
  p_hat = n_yes / n
  
  if (hypo_p == -1) {
    SE_p_hat = sqrt(p_hat * (1 - p_hat) / n)
  }
  
  else {
    SE_p_hat = sqrt(hypo_p * (1 - hypo_p) / n)
  }
  
  summary_df <- data.frame(
    p_hat,
    n_yes,
    n_no,
    n,
    SE_p_hat
  ) %>% 
    rename(
      "Sample Proportion" = p_hat,
      "# Positive" = n_yes,
      "# Negative" = n_no,
      "Sample Size" = n,
      "Standard Error" = SE_p_hat)
  
  return(summary_df)
}

