#' Performs simple linear regression
#'
#' @param data A dataframe containing the data.
#' @param x Predictor variable
#' @param y Response variable
#' @param intercept True if the intercept should be used, False otherwise
#'
#' @return Common summary results and assumption checks for simple linear regression
#'
#' @importFrom dplyr pull
#' @importFrom broom augment
#'
#' @export
perform_slr <- function(data, x, y, intercept = TRUE) {
  
  # Check inputs
  if(!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe.")
  }

  if(deparse(substitute(x)) %in% names(data) == FALSE) {
    stop("Column '", deparse(substitute(x)), "' not found in the dataframe.")
  }
  
  if(deparse(substitute(y)) %in% names(data) == FALSE) {
    stop("Column '", deparse(substitute(y)), "' not found in the dataframe.")
  }
  
  # Extract predictor and response
  predictor <- data %>% dplyr::pull({{x}})
  response <- data %>% dplyr::pull({{y}})  
  
  # Check that inputs are numeric
  stopifnot(is.numeric(predictor) & is.numeric(response))
  
  # Create model
  formula <- as.formula(paste(deparse(substitute(y)), "~", deparse(substitute(x))))
  if (!intercept) {
    formula <- update(formula, . ~ . - 1)
  }
  model <- stats::lm(formula, data = data)
  model.diag.metrics <- broom::augment(model)
  
  # Print summary of coefficients
  cat("Summary of coefficients:")
  print(summary(model)$coefficients, digits = 3)
  
  # Create and display scatter plot with regression line
  plot <- create_scatter_plot(data, {{x}}, {{y}}, model, model.diag.metrics, intercept)
  print(plot)
  
  # Test assumptions
  test_assumptions(model, model.diag.metrics, intercept)
  
  return(model)
  
}

#' Creates scatter plot for simple linear regression
#'
#' @param df A dataframe containing the data.
#' @param x Predictor variable
#' @param y Response variable
#' @param model lm object of slr model
#' @param model.diag.metrics common diagnostic metrics for lm model
#' @param intercept True if the intercept should be used, False otherwise
#'
#' @return Scatter plot for provided lm model
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_segment theme_bw labs
#' @importFrom withr with_options
#' @importFrom plotly ggplotly
#'
#' @export
create_scatter_plot <- function(df, x, y, model, model.diag.metrics, intercept) {
  
  std_residuals <- model.diag.metrics$.std.resid
  leverage <- model.diag.metrics$.hat
  cooks_d <- model.diag.metrics$.cooksd
  
  std_residuals_threshold <- 3
  leverage_threshold <- 2 * length(coef(model)) / nrow(model.diag.metrics)
  cooks_d_threshold <- qf(.5, length(coef(model)), nrow(model.diag.metrics) - length(coef(model)))
  
  formula <- if (intercept) {
    y ~ x
  } else {
    y ~ x - 1
  }
  
  p <- ggplot2::ggplot(data = model.diag.metrics, mapping = ggplot2::aes({{x}}, {{y}})) +
    ggplot2::geom_smooth(method = "lm", formula = formula, se = FALSE, color = "lightblue") +
    ggplot2::geom_segment(ggplot2::aes(xend = {{x}}, yend = .fitted), color = "red", size = 0.3) +
    ggplot2::geom_point(ggplot2::aes(
      text = 
    ),
    color = "black"
    ) +
    ggplot2::theme_bw()
  
  withr::with_options(
    list(digits = 3),
    plotly::ggplotly(p, tooltip = c("x", "y", ".fitted"), width = 800, height = 600)
  )
  
}

#' Checks assumptions for simple linear regression
#'
#' @param model lm object of slr model
#' @param model.diag.metrics common diagnostic metrics for lm model
#' @param intercept True if the intercept should be used, False otherwise
#'
#' @return Plots of assumptions for provided lm model
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_segment stat_qq stat_qq_line geom_hline geom_vline theme_bw labs
#' @importFrom gridExtra grid.arrange
#' @importFrom stats lm shapiro.test cor
#'
#' @export
test_assumptions <- function(model, model.diag.metrics, intercept) {
  
  # Extract residuals, fitted values, leverage, standardized residuals and cooks distance
  residuals <- model$residuals
  fitted <- model$fitted.values
  leverage <- model.diag.metrics$.hat
  std_residuals <- model.diag.metrics$.std.resid
  cooks_d <- model.diag.metrics$.cooksd
  
  violated_indices <- list()
  
  # Linearity
  plot1 <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess",  formula = y ~ x, se = FALSE, color = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted"
    )
  
  linearity_test <- stats::lm(residuals ~ fitted)
  if (summary(linearity_test)$coefficients[2, 4] < 0.05) {
    violated_indices[["linearity"]] <- which(summary(linearity_test)$coefficients[, 4] < 0.05)
  }
  
  # Normality
  plot2 <- ggplot2::ggplot(data = NULL, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Normal Q-Q Plot"
    )
  
  shapiro_test <- stats::shapiro.test(residuals)
  if (shapiro_test$p.value < 0.05) {
    violated_indices[["normality"]] <- TRUE
  }
  
  # Homoscedasticity
  plot3 <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = fitted, y = abs(residuals))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Fitted values",
      y = "Absolute Residuals",
      title = "Scale-Location Plot"
    )
  
  if (stats::cor(fitted, abs(residuals)) > 0.5) {
    violated_indices[["homoscedasticity"]] <- TRUE
  }
  
  # Leverage
  leverage_threshold <- 2 * length(coef(model)) / nrow(model.diag.metrics)
  
  plot4 <- ggplot2::ggplot(data = model.diag.metrics, ggplot2::aes(x = .hat, y = .std.resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = -3, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = 2 * length(coef(model)) / nrow(model.diag.metrics), linetype = "dashed", color = "blue") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "lightblue") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Leverage",
      y = "Standardized Residuals",
      title = "Residuals vs Leverage"
    )
  
  high_leverage_points <- which(leverage > leverage_threshold)
  if (length(high_leverage_points) > 0) {
    violated_indices[["leverage"]] <- c(which(leverage > leverage_threshold))
  }
  
  # Outliers
  std_residuals_threshold <- 3
  outliers <- which(abs(std_residuals) > std_residuals_threshold)
  if (length(outliers) > 0) {
    violated_indices[["outliers"]] <- c(which(abs(std_residuals) > std_residuals_threshold))
  }
  
  # Cooks Dist.
  cooks_d_threshold <- qf(.5, length(coef(model)), nrow(model.diag.metrics) - length(coef(model)))
  
  plot5 <- ggplot2::ggplot(data = model.diag.metrics, ggplot2::aes_string(x = ".hat", y = "cooks_d")) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = cooks_d_threshold, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "lightblue") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Leverage",
      y = "Cook's Distance",
      title = "Cook's Distance vs Leverage"
    )
  
  if (any(cooks_d > cooks_d_threshold)) {
    violated_indices[["cooks_distance"]] <- which(cooks_d > cooks_d_threshold)
  }
  
  # Print indices of observations where violations occur
  if (length(violated_indices) > 0) {
    cat("Violations occurred in the following observations:\n")
    for (violation_type in names(violated_indices)) {
      cat(paste(violation_type, ": ", toString(violated_indices[[violation_type]]), "\n", sep = ""))
    }
  } else {
    cat("No violations detected.\n")
  }
  
  # Display plots
  gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 2)
  
}
# 
# library(dplyr)
# library(ggplot2)
# library(knitr)
# library(rlang)
# library(gridExtra)
# library(broom)
# library(plotly)
# library(withr)
# 
# a <- rnorm(10)
# b <- a + rnorm(10)
# # 
# df <- data.frame(a, b)
# df <- rename(df, c('var1' = a, 'var2' = b))
# # 
# # df <- rbind(df, data.frame('var1' = 1, 'var2' = 100))
# # 
# perform_slr(df, var1, var2, TRUE)








