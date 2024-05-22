data("iris")

string_dataframe <- data.frame(string = c("a", "b", "c", "d"))

#-------------------------------------------------------------------------------

# Testing check_normality function

test_that("check_normality gives error if a numeric vector is not passed", {
  
  correct_result <- "Input 'data' must be numeric."
  
  # Test if the function throws an error with the expected message
  expect_error(check_normality(string_dataframe$string), correct_result)
})

test_that("check_normality returns correct value", {
  
  # Expected test statistic value
  correct_result <- 0.97609027
  
  # Call the function with a numeric vector
  test <- check_normality(iris$Sepal.Length)
  
  # Extract the test statistic from the result and convert to numeric
  my_result <- as.numeric(test$normality$test_statistic)
  
  # Compare the actual result with the expected result
  expect_equal(my_result, correct_result, tolerance = 1e-8)
})

#-------------------------------------------------------------------------------

# Testing check_outliers function

test_that("check_outliers gives error if a numeric vector is not passed", {
  
  correct_result <- "Input 'data' must be numeric."
  
  # Test if the function throws an error with the expected message
  expect_error(check_normality(string_dataframe$string), correct_result)
})

test_that("check_outliers returns an empty data frame when there are no outliers", {
  
  # Expected result: an empty data frame with columns "Index" and "Outlier"
  correct_result <- data.frame(Index = integer(0), Outlier = numeric(0))
  
  # Call the function with a numeric vector
  test <- check_outliers(iris$Sepal.Length)
  
  # Compare the actual result with the expected result
  expect_equal(test, correct_result)
})

test_that("check_outliers returns the correct outliers", {
  
  # Expected result: a data frame with specific indices and outliers
  correct_result <- data.frame(Index = c(16, 33, 34, 61), Outlier = c(4.4, 4.1, 4.2, 2.0))
  
  # Call the function with a numeric vector
  test <- check_outliers(iris$Sepal.Width)
  
  # Compare the actual result with the expected result
  expect_equal(test, correct_result)
})

#-------------------------------------------------------------------------------

test_that("get_conf_int_t returns the correct confidence interval", {
  
  # Expected result
  correct_result <- data.frame(conf_level = 0.95, lower_bound = 5.71, upper_bound = 5.977)
  
  # Call the function with a numeric vector and confidence level
  test <- get_conf_int_t(iris$Sepal.Length, 0.95)
  
  # Compare the actual result with the expected result
  expect_equal(test, correct_result)
})

test_that("get_conf_int_t handles single element vector", {
  
  data_vec <- c(5)
  
  correct_result <- data.frame(conf_level = 0.95, lower_bound = NaN, upper_bound = NaN)
  
  test <- get_conf_int_t(data_vec, 0.95)
  
  expect_equal(test, correct_result)
})

#-------------------------------------------------------------------------------

# Test get_t_test_result  function

# Calculate sample statistics for iris$Sepal.Length
sample_mean <- mean(iris$Sepal.Length)
sample_sd <- sd(iris$Sepal.Length)
n <- length(iris$Sepal.Length)
hypo_mean <- 5.5  # Example hypothesis mean

# Testing get_t_test_result function
test_that("get_t_test_result returns correct t-value and p-value for two.sided hypothesis", {
  correct_result <- data.frame(
    sample_mean = sample_mean, 
    sample_sd = sample_sd, 
    n = n, 
    hypo_mean = hypo_mean,
    t_value = round((sample_mean - hypo_mean) / (sample_sd / sqrt(n)), 3), 
    p_value = round(2 * pt(abs((sample_mean - hypo_mean) / (sample_sd / sqrt(n))), n - 1, lower.tail = FALSE), 3)
  )
  
  test <- get_t_test_result(sample_mean, sample_sd, n, hypo_mean, "two.sided")
  
  expect_equal(test, correct_result)
})

test_that("get_t_test_result returns correct t-value and p-value for greater hypothesis", {
  correct_result <- data.frame(
    sample_mean = sample_mean, 
    sample_sd = sample_sd, 
    n = n, 
    hypo_mean = hypo_mean,
    t_value = round((sample_mean - hypo_mean) / (sample_sd / sqrt(n)), 3), 
    p_value = round(pt((sample_mean - hypo_mean) / (sample_sd / sqrt(n)), n - 1, lower.tail = FALSE), 3)
  )
  
  test <- get_t_test_result(sample_mean, sample_sd, n, hypo_mean, "greater")
  
  expect_equal(test, correct_result)
})

test_that("get_t_test_result returns correct t-value and p-value for less hypothesis", {
  correct_result <- data.frame(
    sample_mean = sample_mean, 
    sample_sd = sample_sd, 
    n = n, 
    hypo_mean = hypo_mean,
    t_value = round((sample_mean - hypo_mean) / (sample_sd / sqrt(n)), 3), 
    p_value = round(pt((sample_mean - hypo_mean) / (sample_sd / sqrt(n)), n - 1), 3)
  )
  
  test <- get_t_test_result(sample_mean, sample_sd, n, hypo_mean, "less")
  
  expect_equal(test, correct_result)
})

test_that("get_t_test_result handles invalid hypo_direction", {
  expect_error(get_t_test_result(sample_mean, sample_sd, n, hypo_mean, "invalid_direction"), 
               "Invalid hypo_direction. Use 'two.sided', 'greater', or 'less'.")
})

# Define small sample data
small_sample <- iris$Sepal.Length[1:5]
small_sample_mean <- mean(small_sample)
small_sample_sd <- sd(small_sample)
small_n <- length(small_sample)
small_hypo_mean <- 4.0

test_that("get_t_test_result handles small sample size", {
  correct_result <- data.frame(
    sample_mean = small_sample_mean, 
    sample_sd = small_sample_sd, 
    n = small_n, 
    hypo_mean = small_hypo_mean,
    t_value = round((small_sample_mean - small_hypo_mean) / (small_sample_sd / sqrt(small_n)), 3), 
    p_value = round(2 * pt(abs((small_sample_mean - small_hypo_mean) / (small_sample_sd / sqrt(small_n))), small_n - 1, lower.tail = FALSE), 3)
  )
  
  test <- get_t_test_result(small_sample_mean, small_sample_sd, small_n, small_hypo_mean, "two.sided")
  
  expect_equal(test, correct_result)
})

# Define large sample data (using all iris$Sepal.Length as an example of a relatively larger sample)
large_sample_mean <- mean(iris$Sepal.Length)
large_sample_sd <- sd(iris$Sepal.Length)
large_n <- length(iris$Sepal.Length)
large_hypo_mean <- 6.0

test_that("get_t_test_result handles large sample size", {
  correct_result <- data.frame(
    sample_mean = large_sample_mean, 
    sample_sd = large_sample_sd, 
    n = large_n, 
    hypo_mean = large_hypo_mean,
    t_value = round((large_sample_mean - large_hypo_mean) / (large_sample_sd / sqrt(large_n)), 3), 
    p_value = round(2 * pt(abs((large_sample_mean - large_hypo_mean) / (large_sample_sd / sqrt(large_n))), large_n - 1, lower.tail = FALSE), 3)
  )
  
  test <- get_t_test_result(large_sample_mean, large_sample_sd, large_n, large_hypo_mean, "two.sided")
  
  expect_equal(test, correct_result)
})