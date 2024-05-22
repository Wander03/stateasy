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
#' @return A data frame with information about the three conditions for the one-sample z-test
#'
#' @export
check_assumptions <- function(data_vec) {
  
  condition_vec = c(
    "Random", 
    "Independence", 
    "Normality")
  
  check_vec = c(
    "Was the sample randomly collected from the population of interest?",
    str_c(
      "Is the sample size (",
      length(data_vec),
      ") less than 10% of the population size?"
    ),
    str_c(
      "Are both the number of positives (",
      sum(data_vec),
      ") and the number of negatives (",
      length(data_vec) - sum(data_vec),
      ") both greater than or equal to 10?"
    )
  )
  
  return(
    data.frame(
      condition = condition_vec,
      description = check_vec
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
  
  # Define range of p for plot
  p_vec = seq(0, 1, 0.01)
  
  # Calculate normal density according to hypothesized mean and SE
  norm_density = dnorm(p_vec, mean = hypo_p, sd = SE_p_hat)
  
  # Create sampling distribution plot
  sampling_dist = data.frame(p_vec, norm_density) %>% 
    
    # Create shaded regions based on hypothesized directions and values
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
      )
    ) %>% 
    
    # Create plot
    ggplot(
      aes(
        x = p_vec,
        y = norm_density
      )
    ) + 
    
    # Trace sampling distribution curve
    geom_line() +
    
    # Add vertical line at observed sample proportion
    geom_vline(xintercept = p_hat) +
    
    # Add x-axis line
    geom_hline(yintercept = 0) +
    
    # Add region for right shaded region
    geom_area(
      aes(
        x = if_else(shaded_right, p_vec, NA)
      ),
      fill = "red"
    ) +
    
    # Add region for left shaded region
    geom_area(
      aes(
        x = if_else(shaded_left, p_vec, NA)
      ),
      fill = "red"
    ) +
    
    # Limit x to possible proportion values (0, 1)
    xlim(0, 1) +
    
    # Limit y to between 0 and top of density curve
    scale_y_continuous(
      limits = c(0, max(norm_density) * 1.1), 
      expand = c(0, 0)
    ) +
    
    # Add axis labels and titles
    labs(
      title = "Sampling Distribution for One-Sample z-test",
      subtitle = "Shaded region corresponds to p-value",
      x = "Sample Proportion",
      y = ""
    ) +
    
    # Add simple theme
    theme_bw()
  
  # Suppress warnings because p-value shading creates "missing" values in plot
  suppressWarnings(print(sampling_dist))
  
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
  
  # Calculate sample proportion and sample size
  p_hat = mean(data_vec)
  n = length(data_vec)
  
  # Calculate critical value accoridng to confidence level
  z_crit = qnorm((1 - conf_level) / 2)
  
  # Calculate lower and upper bound
  lb = p_hat + z_crit * sqrt(p_hat * (1 - p_hat) / n)
  ub = p_hat - z_crit * sqrt(p_hat * (1 - p_hat) / n)
  
  # Return confidence interval information
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
#' @param data_vec A vector of TRUE/FALSE or 0/1 data representing the sample 
#' @param hypo_p The hypothesized value of the proportion
#' @param hypo_direction The direction of the alternative hypothesis
#'
#' @return A data frame containing the test statistic and p-value
#'
#' @export
get_test_result <- function(data_vec, hypo_p, hypo_direction) {
  
  # Calculate sample size and proportion
  n = length(data_vec)
  p_hat = mean(data_vec)
  
  # If there  is no hypothesis test
  if (hypo_direction == "") {
    
    # Calculate standard error according to sample proportion
    SE_p_hat = sqrt(p_hat * (1 - p_hat) / n)
    
    # Test summaries are null
    z_value = NA
    p_value = NA
  }
  
  # If there is a hypothesis test
  else {
    
    # Calculate SE and z accoridng to hypothesized proportion
    SE_p_hat = sqrt(hypo_p * (1 - hypo_p) / n)
    z_value = (p_hat - hypo_p) / SE_p_hat
    
    # If greater than, use upper tail
    if (hypo_direction == ">") {
      p_value = pnorm(z_value, lower.tail = FALSE)
    }
    
    # If less than, use lower tail
    else if (hypo_direction == "<") {
      p_value = pnorm(z_value)
    }
    
    # If not equal to, use both tails
    else if (hypo_direction == "!=") {
      p_value = pnorm(abs(z_value), lower.tail = FALSE) * 2
    }
  }
  
  
  # Put into summary dataframe
  test_df <- data.frame(
    p_hat = p_hat,
    n = n,
    SE_p_hat = SE_p_hat,
    z_value = z_value,
    p_value = p_value
  )
  
  # Return dataframe
  return(test_df)
}







#' Calculate the result of a one-sample z test for a population proportion
#'
#' @param data_vec A vector of TRUE/FALSE or 0/1 data representing the sample
#' @param hypo_p The hypothesized value of the proportion
#' @param hypo_direction The direction of the alternative hypothesis
#' @param conf_level A number between 0 and 1 specifying the confidence level
#'
#' @return Several test results, confidence interval, plot of sampling distribution
#'
#' @export
z_test <- function(data_vec, hypo_p = -1, hypo_direction = "", conf_level = 0.95) {
  
  # Stop if the specified hypothesized value is not between 0 and 1
  if (hypo_p != -1 & (hypo_p <= 0 | hypo_p >= 1)) {
    stop("Hypothesized value must be between 0 and 1 or left blank.")
  }
  
  # Stop if the specified hypothesized direction is not <, >, or !=
  if (!valid_direction(hypo_direction)) {
    stop("Hypothesized direction must be <, >, !=, or left blank.")
  }
  
  # Stop if the data type of the passed data vector is wrong
  if (!valid_data_vec(data_vec)) {
    stop("Data vector must be logical (TRUE/FALSE) or binary numeric (1/0).")
  }
  
  # Stop if user specified only one of the hypothesized direction and value
  if ((hypo_p == -1 & hypo_direction != "") | (hypo_direction == "" & hypo_p != -1)) {
    stop("You must specify both the hypothesized proportion and the hypothesized direction or specify neither, but not one.")
  }
  
  # Stop if the confidence level is not between 0 and 1
  if (conf_level <= 0 | conf_level >= 1) {
    stop("Confidence level must be between 0 and 1.")
  }
  
  # Check the assumptions of the one-sample z test
  print(check_assumptions(data_vec), row.names = FALSE)
  
  # Get testing information from data vector and hypothesis test information
  test_df = get_test_result(data_vec, hypo_p, hypo_direction)
  print(test_df, row.names = FALSE)
  
  # If there is a hypothesis test requested
  if (hypo_p != -1 & hypo_direction != "") {
    
    # Extract p_hat and SE for sampling distribution plot
    p_hat = test_df$p_hat
    SE_p_hat = test_df$SE_p_hat
    
    # Plot sampling distribution
    plot_sampling_distribution(p_hat, SE_p_hat, hypo_p, hypo_direction)
  }
  
  # Get confidence interval information and print
  conf_df = get_conf_int(data_vec, conf_level)
  print(conf_df, row.names = FALSE)
  
}