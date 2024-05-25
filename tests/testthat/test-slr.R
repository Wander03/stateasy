set.seed(123)
test_data <- data.frame(x = rnorm(100), y = 2 * rnorm(100) + 1)


# Test perform_slr function
test_that("perform_slr works correctly with default settings", {
  model <- perform_slr(test_data, x, y)
  expect_s3_class(model, "lm")
  expect_equal(length(coef(model)), 2)
})

test_that("perform_slr works correctly without intercept", {
  model <- perform_slr(test_data, x, y, intercept = FALSE)
  expect_s3_class(model, "lm")
  expect_equal(length(coef(model)), 1)
})

test_that("perform_slr handles non-dataframe input", {
  expect_error(perform_slr(list(x = 1:10, y = 1:10), x, y))
})

test_that("perform_slr handles non-existing columns", {
  expect_error(perform_slr(test_data, non_existing_column, y))
  expect_error(perform_slr(test_data, x, non_existing_column))
})

test_that("perform_slr handles non-numeric columns", {
  test_data_non_numeric <- data.frame(
    x = letters[1:10],
    y = rnorm(10)
  )
  test_data_non_numeric_2 <- data.frame(
    x = rnorm(10),
    y = letters[1:10]
  )
  expect_error(perform_slr(test_data_non_numeric, x, y))
  expect_error(perform_slr(test_data_non_numeric_2, x, y))
})

# Test create_scatter_plot function
test_that("create_scatter_plot works correctly", {
  model <- lm(y ~ x, data = test_data)
  model_diag_metrics <- broom::augment(model)
  plot <- create_scatter_plot(test_data, x, y, model, model_diag_metrics, intercept = TRUE)
  expect_s3_class(plot, "plotly")
})

# Test test_assumptions function
test_that("test_assumptions detects violations", {
  test_data_violations <- data.frame(
    x = c(rnorm(98), 20, 20),
    y = c(rnorm(98), 50, -50)
  )
  model <- lm(y ~ x, data = test_data_violations)
  model_diag_metrics <- broom::augment(model)
  expect_output(test_assumptions(model, model_diag_metrics, intercept = TRUE), "Violations occurred in the following observations")
})

test_that("test_assumptions detects normality violations", {
  test_data_normality_violation <- data.frame(
    x = rnorm(100),
    y = c(rnorm(95), 5, 6, 7, 8, 9)
  )
  model <- lm(y ~ x, data = test_data_normality_violation)
  model_diag_metrics <- broom::augment(model)
  expect_output(test_assumptions(model, model_diag_metrics, intercept = TRUE), "normality")
})

test_that("test_assumptions detects homoscedasticity violations", {
  x <- seq(1, 10, length.out = 100)
  y <- 2 * x + rnorm(100, sd = x)
  test_data_homoscedasticity_violation <- data.frame(
    x = x,
    y = y
  )
  model <- lm(y ~ x, data = test_data_homoscedasticity_violation)
  model_diag_metrics <- broom::augment(model)
  expect_output(test_assumptions(model, model_diag_metrics, intercept = TRUE), "homoscedasticity")
})

test_that("test_assumptions detects leverage points", {
  test_data_leverage_violation <- data.frame(
    x = c(rnorm(95), 20, 21, 22, 23, 24),
    y = rnorm(100)
  )
  model <- lm(y ~ x, data = test_data_leverage_violation)
  model_diag_metrics <- broom::augment(model)
  expect_output(test_assumptions(model, model_diag_metrics, intercept = TRUE), "leverage")
})

test_that("test_assumptions detects outliers", {
  test_data_outliers_violation <- data.frame(
    x = rnorm(100),
    y = c(rnorm(95), 10, -10, 12, -12, 15)
  )
  model <- lm(y ~ x, data = test_data_outliers_violation)
  model_diag_metrics <- broom::augment(model)
  expect_output(test_assumptions(model, model_diag_metrics, intercept = TRUE), "outliers")
})

test_that("test_assumptions detects high Cook's distance", {
  x <- seq(1, 10, length.out = 100)
  y <- 2 * x + rnorm(100, mean = 0, sd = 1)
  y[100] <- 1000
  test_data_cooksd_violation <- data.frame(
    x = x,
    y = y
  )
  model <- lm(y ~ x, data = test_data_cooksd_violation)
  model_diag_metrics <- broom::augment(model)
  expect_output(test_assumptions(model, model_diag_metrics, intercept = TRUE), "cooks_distance")
})