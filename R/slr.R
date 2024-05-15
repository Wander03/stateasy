#' Performs simple linear regression
#'
#' @param df Data frame containing predictor and response variables
#' @param x Predictor variable
#' @param y Response variable
#' @param intercept True if the intercept should be used, False otherwise
#'
#' @return Common summary results from simple linear regression
#'
#' @importFrom dplyr select filter 
#' @importFrom ggplot2 ggplot
#' @importFrom knitr kable
#'
#' @export
perform_slr <- function(df, x, y, intercept = T) {

  predictor <- df %>% pull({{x}})
  response <- df %>% pull({{y}})  
  
  predictor_name <- print(deparse(substitute(x)))
  response_name <- print(deparse(substitute(y)))

  stopifnot(is.numeric(predictor) & is.numeric(response))
  
  model <- lm(response ~ predictor, data = df)
  
  print(kable(summary(m)$coef))
  
  create_scatter_plot(df, {{x}}, {{y}}, predictor_name, response_name)
  
  
}

create_scatter_plot <- function(df, x, y, x_name, y_name) {

  ggplot(df, mapping = aes({{x}}, {{y}})) +
    geom_smooth(method = "lm", se = F, color = "red") +
    theme_bw() +
    labs(title = paste(paste("Simple Linear Regression for", y_name, "="), expression(paste(beta, "x")))
    )
  
}

library(dplyr)
library(ggplot2)
library(knitr)
library(rlang)

a <- rnorm(10)
b <- rnorm(10)^3

df <- data.frame(a, b)
df <- rename(df, c('var1' = a, 'var2' = b))

m <- perform_slr(df, var1, var2)
m







