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
get_summary_statistics <- function(data_vec, hypo_p) {
  
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







#' Check the assumptions of the one sample proportion test
#'
#' @param data_vec A vector of TRUE/FALSE or 0/1 data representing the sample
#'
#' @return No return value, but prints information about the conditions
#'
#' @export
check_assumptions <- function(data_vec) {
  
  print("Condition 1: Was the sample randomly collected from the population of interest?")
  print(
    str_c(
      "Condition 2: Is the sample size (",
      length(data_vec),
      ") less than 10% of the population size?"
    )
  )
  print(
    str_c(
      "Condition 3: Are both the number of positives (",
      sum(data_vec),
      ") and the number of negatives (",
      length(data_vec) - sum(data_vec),
      ") both greater than or equal to 10?"
    )
  )
  
}


#' Create a plot of the sampling distribution of the sample proportion
#'
#' @param p_hat The sample proportion
#' @param SE_p_hat The standard error of the sample proportion
#' @param hypo_p The hypothesized value of the proportion
#' @param hypo_direction The direction of the alternative hypothesis
#'
#' @return A plot of the sampling distribution
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#'
#' @export
plot_sampling_distribution <- function(p_hat, SE_p_hat, hypo_p, hypo_direction) {
  
  if (hypo_p != -1 & hypo_direction != "") {
    
    p_vec = seq(0, 1, 0.01)
    norm_density = dnorm(p_vec, mean = hypo_p, sd = SE_p_hat)
    
    sampling_dist = data.frame(p_vec, norm_density) %>% 
      mutate(
        shaded_right = case_when(
          p_vec >= p_hat & hypo_direction == ">" ~ TRUE,
          p_vec >= hypo_p + abs(p_hat - hypo_p) & hypo_direction == "!=" ~ TRUE,
          TRUE ~ FALSE
        ),
        shaded_left = case_when(
          p_vec <= p_hat & hypo_direction == "<" ~ TRUE,
          p_vec <= hypo_p - abs(p_hat - hypo_p) & hypo_direction == "!=" ~ TRUE,
          TRUE ~ FALSE
        ),
        
        shaded = case_when(
          p_vec >= p_hat & hypo_direction == ">" ~ TRUE,
          p_vec <= p_hat & hypo_direction == "<" ~ TRUE,
          (p_vec >= hypo_p + abs(p_hat - hypo_p) | p_vec <= hypo_p - abs(p_hat - hypo_p)) & hypo_direction == "!=" ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>% 
      ggplot(
        aes(
          x = p_vec,
          y = norm_density
        )
      ) + 
      geom_line() +
      geom_vline(xintercept = p_hat) +
      geom_hline(yintercept = 0) +
      geom_area(
        aes(
          x = if_else(shaded_right, p_vec, NA)
        ),
        fill = "red"
      ) +
      geom_area(
        aes(
          x = if_else(shaded_left, p_vec, NA)
        ),
        fill = "red"
      ) +
      xlim(0, 1)
    
    suppressWarnings(print(sampling_dist))
    
  }
  
}





#' Calculate a confidence interval for a specified level of confidence
#'
#' @param data_vec A vector of TRUE/FALSE or 0/1 data representing the sample
#' @param conf_level A number between 0 and 1 specifying the confidence level
#'
#' @return A dataframe with confidence level, lower bound, and upper bound
#'
#' @export
get_conf_int <- function(data_vec, conf_level) {
  
  p_hat = mean(data_vec)
  n = length(data_vec)
  z_crit = qnorm((1 - conf_level) / 2)
  
  lb = p_hat + z_crit * sqrt(p_hat * (1 - p_hat) / n)
  ub = p_hat - z_crit * sqrt(p_hat * (1 - p_hat) / n)
  
  return(
    data.frame(
      conf_level = conf_level,
      lower_bound = round(lb, 3),
      upper_bound = round(ub, 3)
    )
  )
}


#' Calculate the result of a one-sample z test for a population proportion
#'
#' @param p_hat The sample proportion
#' @param SE_p_hat The standard error of the sample proportion
#' @param hypo_p The hypothesized value of the proportion
#' @param hypo_direction The direction of the alternative hypothesis
#'
#' @return A dataframe containing the test statistic and p-value
#'
#' @export
get_test_result <- function(p_hat, SE_p_hat, hypo_p, hypo_direction) {
  
  z_value = (p_hat - hypo_p) / SE_p_hat
  
  if (hypo_direction == ">") {
    p_value = pnorm(z_value, lower.tail = FALSE)
  }
  
  else if (hypo_direction == "<") {
    p_value = pnorm(z_value)
  }
  
  else if (hypo_direction == "!=") {
    p_value = pnorm(abs(z_value), lower.tail = FALSE) * 2
  }
  
  test_df <- data.frame(
    p_hat = p_hat,
    SE_p_hat = SE_p_hat,
    z_value = z_value,
    p_value = p_value
  )
  
  return(test_df)
}