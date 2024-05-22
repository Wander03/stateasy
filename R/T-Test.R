#' Announces the results of a one-sample t-test for a given dataset and column.
#'
#' This function performs a one-sample t-test to determine if the mean of the 
#' specified column in the dataframe is equal to a hypothesized value. It also 
#' checks the normality of the data, identifies outliers, and plots the sampling 
#' distribution.
#'
#' @param data A dataframe containing the data.
#' @param x A string representing the name of the column to be tested.
#' @param hypo_mean The hypothesized population mean under the null hypothesis (default is 0).
#' @param alternative The direction of the alternative hypothesis ("two.sided", "greater", "less"). Default is "two.sided".
#' @param conf_level The confidence level for the confidence interval (default is 0.95).
#'
#' @return A printout of the t-test result,
#' confidence interval, any outliers, and relevant plots.
#'
#' @importFrom stringr str_detect str_to_title
#' @importFrom english as.english
#'
#' @export
run_t_test <- function(data, x, hypo_mean = 0, alternative = "two.sided", conf_level = .95) {
  
  if(!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe.")
  }
  
  if(x %in% names(data) == FALSE) {
    stop("Column '", x, "' not found in the dataframe.")
  }
  
  # Display the QQ plot
  display_qqplot(data, x)
  
  # Check normality assumption
  normality <- check_normality(data[[x]])
  
  # CLT may not apply if the sample size is less than 30
  if (length(data[[x]]) < 30) {
    
    print("drilled")
    # If normality assumption is not met, display warning message
    if(normality$normality$normality_assumption == "Not met") {
      cat("Warning: Possible issues with normality, proceed with caution")
      cat("\n")
      cat("\n")
    }
    
  }
  
  outliers <- check_outliers(data[[x]])
  
  if(nrow(outliers != 0)) {
    cat("Listed below are some outliers in the data, consider inspecting these values and why they are present:")
    cat("\n")
    print(outliers, row.names = FALSE)
    cat("\n")
  }
  
  sample_mean <- mean(data[[x]])
  sample_sd <- sd(data[[x]])
  n <- length(data[[x]])
  
  print(get_t_test_result(sample_mean, sample_sd, n, hypo_mean, alternative), row.names = FALSE)
  cat("\n")
  print(get_conf_int_t(data[[x]], conf_level), row.names = FALSE)
  
  plot_sampling_distribution(sample_mean, sample_sd, n, hypo_mean, alternative)
}


#' Check normality assumption
#'
#' This function checks the normality assumption for a one-sample t-test using 
#' the Shapiro-Wilk test.
#' 
#' @param x A numeric vector of data.
#' @param alpha The significance level for the Shapiro-Wilk test (default is 0.05).
#' 
#' @return A list containing the test statistic, p-value, and the result of the 
#' normality assumption check.
#' 
#' @export
check_normality <- function(x, alpha = 0.05) {
  if(!is.numeric(x)) {
    stop("Input 'data' must be numeric.")
  }
  
  # Check for normality (Shapiro-Wilk test)
  normality_test <- shapiro.test(x)
  
  # Create a list to store the results
  assumption_results <- list(
    normality = list(
      test_statistic = normality_test$statistic,
      p_value = normality_test$p.value,
      normality_assumption = ifelse(normality_test$p.value > alpha, "Met", "Not met")
    )
  )
  
  return(assumption_results)
}


#' Display a QQ plot for checking the normality assumption
#'
#' This function generates a QQ plot to visually assess the normality of the data 
#' in the specified column of the dataframe.
#'
#' @param data A dataframe containing the data.
#' @param x The name of the column to be checked (must be numeric).
#'
#' @return A QQ plot.
#'
#' @export
display_qqplot <- function(data, x) {
  qqnorm(data[[x]])
  qqline(data[[x]])
}


#' Check for outliers in the data
#'
#' This function identifies outliers in a numeric vector using the interquartile 
#' range (IQR) method.
#'
#' @param data A numeric vector of data.
#'
#' @return A dataframe containing the indices and values of the outliers.
#'
#' @export
check_outliers <- function(data) {
  if (!is.numeric(data)) {
    stop("Input 'data' must be numeric.")
  }
  
  # Calculate the first and third quartiles
  q1 <- quantile(data, 0.25)
  q3 <- quantile(data, 0.75)
  
  # Calculate the interquartile range (IQR)
  iqr <- q3 - q1
  
  # Define the lower and upper bounds for outliers
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Identify outliers and their indices
  outlier_indices <- which(data < lower_bound | data > upper_bound)
  outlier_values <- data[outlier_indices]
  
  # Return a data frame containing indices and outlier values
  outliers <- data.frame(Index = outlier_indices, Outlier = outlier_values)
  
  return(outliers)
}


#' Plot the sampling distribution for a one-sample t-test
#'
#' This function creates a plot of the sampling distribution for a one-sample 
#' t-test based on the sample mean, sample standard deviation, sample size, 
#' hypothesized mean, and direction of the alternative hypothesis.
#'
#' @param sample_mean The sample mean.
#' @param sample_sd The sample standard deviation.
#' @param n The sample size.
#' @param hypo_mean The hypothesized population mean.
#' @param hypo_direction The direction of the alternative hypothesis ("greater", "less", "two.sided").
#'
#' @return A plot of the sampling distribution.
#'
#' @export
plot_sampling_distribution <- function(sample_mean, sample_sd, n, hypo_mean, hypo_direction) {
  
  # Calculate the standard error and t-statistic
  se <- sample_sd / sqrt(n)
  t_stat <- (sample_mean - hypo_mean) / se
  
  # Create the t distribution
  df <- n - 1
  t_vec <- seq(hypo_mean - 4 * se, hypo_mean + 4 * se, length.out = 1000)
  t_density <- dt((t_vec - hypo_mean) / se, df = df)
  
  # Create a data frame for plotting
  plot_data <- data.frame(t_vec, t_density) %>%
    mutate(
      shaded_right = case_when(
        t_vec >= sample_mean & hypo_direction == "greater" ~ TRUE,
        t_vec >= hypo_mean + abs(sample_mean - hypo_mean) & hypo_direction == "two.sided" ~ TRUE,
        TRUE ~ FALSE
      ),
      shaded_left = case_when(
        t_vec <= sample_mean & hypo_direction == "less" ~ TRUE,
        t_vec <= hypo_mean - abs(sample_mean - hypo_mean) & hypo_direction == "two.sided" ~ TRUE,
        TRUE ~ FALSE
      ),
      shaded = case_when(
        t_vec >= sample_mean & hypo_direction == "greater" ~ TRUE,
        t_vec <= sample_mean & hypo_direction == "less" ~ TRUE,
        (t_vec >= hypo_mean + abs(sample_mean - hypo_mean) | t_vec <= hypo_mean - abs(sample_mean - hypo_mean)) & hypo_direction == "two.sided" ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # Suppress warnings while plotting
  suppressWarnings({
    p <- ggplot(plot_data, aes(x = t_vec, y = t_density)) + 
      geom_line() +
      geom_vline(xintercept = sample_mean) +
      geom_hline(yintercept = 0) +
      geom_area(
        aes(
          x = if_else(shaded_right, t_vec, as.numeric(NA))
        ),
        fill = "red",
        alpha = 0.5
      ) +
      geom_area(
        aes(
          x = if_else(shaded_left, t_vec, as.numeric(NA))
        ),
        fill = "red",
        alpha = 0.5
      ) +
      labs(
        title = "Sampling Distribution for One-Sample t-Test",
        x = "Sample Mean",
        y = "Density"
      ) +
      xlim(hypo_mean - 4 * se, hypo_mean + 4 * se) +
      theme_bw()
    
    print(p)
  })
}


#' Calculate a confidence interval for a one-sample t-test
#'
#' This function calculates a confidence interval for the population mean of 
#' the specified data vector using a one-sample t-test.
#'
#' @param data_vec A numeric vector representing the sample data.
#' @param conf_level A number between 0 and 1 specifying the confidence level 
#' for the interval (default is 0.95).
#'
#' @return A dataframe with columns for the confidence level, lower bound, and 
#' upper bound of the confidence interval.
#'
#' @export
get_conf_int_t <- function(data_vec, conf_level) {
  
  # Sample mean and standard deviation
  sample_mean <- mean(data_vec)  
  sample_sd <- sd(data_vec)      
  
  # Sample size
  n <- length(data_vec)          
  
  # Significance level
  alpha <- 1 - conf_level        
  
  # Critical value from t-distribution
  t_crit <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)  
  
  # Standard Error of the sample mean
  se <- sample_sd / sqrt(n)
  
  # Calculate lower and upper bounds of the confidence interval
  lb <- sample_mean - t_crit * se
  ub <- sample_mean + t_crit * se
  
  # Create dataframe for the confidence interval
  conf_interval <- data.frame(
    conf_level = conf_level,
    lower_bound = round(lb, 3),
    upper_bound = round(ub, 3)
  )
  
  return(conf_interval)
}


#' Calculate the result of a one-sample t-test for a population mean
#'
#' This function computes the result of a one-sample t-test for a population mean 
#' using the sample mean, sample standard deviation, sample size, hypothesized 
#' mean, and direction of the alternative hypothesis.
#'
#' @param sample_mean The sample mean.
#' @param sample_sd The sample standard deviation.
#' @param n The sample size.
#' @param hypo_mean The hypothesized value of the mean under the null hypothesis.
#' @param hypo_direction The direction of the alternative hypothesis ("<", ">", "!=").
#'
#' @return A dataframe containing the test statistic (t-value) and p-value.
#'
#' @export
get_t_test_result <- function(sample_mean, sample_sd, n, hypo_mean, hypo_direction = "two.sided") {
  
  # Calculate the standard error
  SE_mean <- sample_sd / sqrt(n)
  
  # Calculate the t-statistic
  t_value <- (sample_mean - hypo_mean) / SE_mean
  
  # Degrees of freedom
  df <- n - 1
  
  # Calculate the p-value based on the direction of the alternative hypothesis
  if (hypo_direction == "greater") {
    p_value <- pt(t_value, df, lower.tail = FALSE)
  } else if (hypo_direction == "less") {
    p_value <- pt(t_value, df)
  } else if (hypo_direction == "two.sided") {
    p_value <- 2 * pt(abs(t_value), df, lower.tail = FALSE)
  } else {
    stop("Invalid hypo_direction. Use 'two.sided', 'greater', or 'less'.")
  }
  
  # Create dataframe for the test result
  test_result <- data.frame(
    sample_mean = sample_mean,
    sample_sd = sample_sd,
    n = n,
    hypo_mean = hypo_mean,
    t_value = round(t_value, 3),
    p_value = round(p_value, 3)
  )
  
  return(test_result)
  
}

