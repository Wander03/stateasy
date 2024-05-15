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

  stopifnot(is.numeric(predictor) & is.numeric(response))
  
  predictor_name <- print(deparse(substitute(x)))
  response_name <- print(deparse(substitute(y)))
  
  model <- lm(response ~ predictor, data = df)
  print(kable(summary(model)$coef))
  
  create_scatter_plot(df, predictor, response, predictor_name, response_name)
  test_assumptions(model)
  
}

create_scatter_plot <- function(df, x, y, x_name, y_name) {

  ggplot(df, mapping = aes(x, y)) +
    geom_point(color = "lightblue3") +
    geom_smooth(method = "lm", formula = y ~ x, se = F, color = "red") +
    theme_bw() +
    labs(
      x = x_name,
      y = y_name
    )
  
}

test_assumptions <- function(m) {
  
  plot(m)
  
}

library(dplyr)
library(ggplot2)
library(knitr)
library(rlang)

a <- rnorm(10)
b <- a + 20 + runif(10)

df <- data.frame(a, b)
df <- rename(df, c('var1' = a, 'var2' = b))

perform_slr(df, var1, var2)








