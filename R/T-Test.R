#' Announces the number of candygrams for a person.
#'
#' @param person The candygram recipient
#' @param number How many grams they got
#' @param extra_message A string giving extra commentary.
#'
#' @return A candy gram announcement
#'
#' @importFrom stringr str_detect str_to_title
#' @importFrom english as.english
#'
#' @export
run_t_test <- function(data, x, mu = 0, alternative) {
  if(!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe.")
  }
  
  if(x %in% names(data) == FALSE) {
    stop("Column '", x, "' not found in the dataframe.")
  }
  
  # Display the qqplot
  display_qqplot(data, x)
  
  # Check assumptions
  assumptions <- check_assumptions(data[[x]])
  
  # CLT may not apply if the sample size is less than 30
  if (length(data[[x]] < 30)) {
  
    # If normallity assumption is not met, display warning message
    if(assumptions$normality$normality_assumption == "Not met") {
      print("Warning: Possible issues with normallity, proceed with caution")
    }
  
  }
  
  # Run the t-test
  t_test_result <- t.test(data[[x]], mu = mu)
  return(t_test_result)
  
}


#' Check normallity assumption
#'
#' This function checks the assumptions of a one-sample t-test.
#' 
#' @param data A vector of numeric data.
#' @param alpha The significance level for the tests (default is 0.05).
#' 
#' @return A list containing the results of the assumption checks.
#' 
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' assumptions <- check_assumptions(data)
#' print(assumptions)
#' 
#' @export
check_assumptions <- function(data, alpha = 0.05) {
  if(!is.numeric(data)) {
    stop("Input 'data' must be numeric.")
  }
  
  # Check for normality (Shapiro-Wilk test)
  normality_test <- shapiro.test(data)
  
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


#' Creates a qqplot to check the normallity assumption
#'
#' @param data The dataframe
#' @param x Variable of interest (must be numeric)
#'
#' @return A normallity qqplot
#'
#' @export
display_qqplot <- function(data, x) {
  
  qqnorm(data[[x]])
  qqline(data[[x]])
  
}


