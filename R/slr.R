#' Performs simple linear regression
#'
#' @param df Data frame containing predictor and response variables
#' @param x Predictor variable
#' @param y Response variable
#' @param intercept True if the intercept should be used, False otherwise
#'
#' @return Common summary results from simple linear regression
#'
#' @importFrom dplyr select filter pull
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_bw labs
#' @importFrom knitr kable
#' @importFrom gridExtra grid.arrange
#' @importFrom broom augment
#' @importFrom stats lm shapiro.test cor
#'
#' @export
perform_slr <- function(df, x, y, intercept = TRUE) {

  # Extract predictor and response
  predictor <- df %>% pull({{x}})
  response <- df %>% pull({{y}})  

  # Check that inputs are numeric
  stopifnot(is.numeric(predictor) & is.numeric(response))
  
  # Create model
  formula <- as.formula(paste(deparse(substitute(y)), "~", deparse(substitute(x))))
  if (!intercept) {
    formula <- update(formula, . ~ . - 1)
  }
  model <- lm(formula, data = df)
  model.diag.metrics <- augment(model)

  # Print summary of coefficients
  cat("Summary of coefficients:")
  print(kable(summary(model)$coefficients, digits = 4))
  
  # Create and display scatter plot with regression line
  plot <- create_scatter_plot(df, {{x}}, {{y}}, model, model.diag.metrics, intercept)
  print(plot)

  # Test assumptions
  test_assumptions(model, model.diag.metrics, intercept)

}


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

  model.diag.metrics <- model.diag.metrics %>%
    mutate(label = case_when(
      .std.resid > std_residuals_threshold &
        .hat > leverage_threshold &
        .cooksd > cooks_d_threshold ~ 
        "red",
      .std.resid > std_residuals_threshold &
        .hat > leverage_threshold ~
        "orange",
      .cooksd > cooks_d_threshold ~
        "green",
      TRUE ~ "black"
        )
      )

  p <- ggplot(data = model.diag.metrics, mapping = aes({{x}}, {{y}})) +
    geom_smooth(method = "lm", formula = formula, se = FALSE, color = "lightblue") +
    geom_segment(aes(xend = {{x}}, yend = .fitted), color = "red", size = 0.3) +
    # geom_point(color = "black") +
    theme_bw() +
    geom_point(aes(color = label), size = 3, show.legend = TRUE)
    
  # p <- p + geom_text(data = subset(model.diag.metrics, cooks_d > cooks_d_threshold),
  #                    aes(x = {{x}}, y = {{y}}, label = row.names(model.diag.metrics)[cooks_d > cooks_d_threshold]), 
  #                    color = "black", size = 3, hjust=-1, vjust=0)
  
  # ggplotly(p, tooltip = c("x", "y", ".fitted"), width = 800, height = 600)
  
}


test_assumptions <- function(model, model.diag.metrics, intercept) {
  
  # Extract residuals, fitted values, leverage, standardized residuals and cooks distance
  residuals <- model$residuals
  fitted <- model$fitted.values
  leverage <- model.diag.metrics$.hat
  std_residuals <- model.diag.metrics$.std.resid
  cooks_d <- model.diag.metrics$.cooksd
  
  violated_indices <- list()

  # Linearity
  plot1 <- ggplot(data = NULL, aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_smooth(method = "loess",  formula = y ~ x, se = FALSE, color = "red") +
    theme_bw() +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted"
    )
  
  linearity_test <- lm(residuals ~ fitted)
  if (summary(linearity_test)$coefficients[2, 4] < 0.05) {
    violated_indices[["linearity"]] <- which(summary(linearity_test)$coefficients[, 4] < 0.05)
  }
  
  # Normality
  plot2 <- ggplot(data = NULL, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    theme_bw() +
    labs(
      title = "Normal Q-Q Plot"
    )
  
  shapiro_test <- shapiro.test(residuals)
  if (shapiro_test$p.value < 0.05) {
    violated_indices[["normality"]] <- TRUE
  }
  
  # Homoscedasticity
  plot3 <- ggplot(data = NULL, aes(x = fitted, y = abs(residuals))) +
    geom_point() +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "red") +
    theme_bw() +
    labs(
      x = "Fitted values",
      y = "Absolute Residuals",
      title = "Scale-Location Plot"
    )

  if (cor(fitted, abs(residuals)) > 0.5) {
    violated_indices[["homoscedasticity"]] <- TRUE
  }
  
  # Leverage
  leverage_threshold <- 2 * length(coef(model)) / nrow(model.diag.metrics)

  plot4 <- ggplot(data = model.diag.metrics, aes(x = .hat, y = .std.resid)) +
    geom_point() +
    geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
    geom_hline(yintercept = -3, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 2 * length(coef(model)) / nrow(model.diag.metrics), linetype = "dashed", color = "blue") +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "lightblue") +
    theme_bw() +
    labs(
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
  
  plot5 <- ggplot(data = model.diag.metrics, aes_string(x = ".hat", y = "cooks_d")) +
    geom_point() +
    geom_hline(yintercept = cooks_d_threshold, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "lightblue") +
    theme_bw() +
    labs(
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
  grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 2)
  
}

library(dplyr)
library(ggplot2)
library(knitr)
library(rlang)
library(gridExtra)
library(broom)
library(plotly)

a <- rnorm(10)
b <- a + rnorm(10)

df <- data.frame(a, b)
df <- rename(df, c('var1' = a, 'var2' = b))

# df <- rbind(df, data.frame('var1' = -10, 'var2' = 10))

# perform_slr(df, var1, var2, TRUE)

perform_slr(alcoholtobacco, Tobacco, Alcohol, TRUE)








