test_that("Check data vector type with numeric data", {
  
  correct_result <- FALSE
  
  data_vec <- c(5, 6, 7, 8, 9)
  
  my_result <- valid_data_vec(data_vec)
  
  expect_equal(my_result, correct_result)
})


test_that("Check data vector type with boolean data", {
  
  correct_result <- TRUE
  
  data_vec <- c(TRUE, FALSE, FALSE, TRUE)
  
  my_result <- valid_data_vec(data_vec)
  
  expect_equal(my_result, correct_result)
})

test_that("Check data vector type with binary numeric data", {
  
  correct_result <- TRUE
  
  data_vec <- c(0, 1, 1, 0, 0)
  
  my_result <- valid_data_vec(data_vec)
  
  expect_equal(my_result, correct_result)
})

test_that("Hypothesis direction check: incorrect specification", {
  
  correct_result <- FALSE
  
  my_result <- valid_direction(">=")
  
  expect_equal(my_result, correct_result)
})

test_that("Hypothesis direction check: correct specification", {
  
  correct_result <- TRUE
  
  my_result <- valid_direction("!=")
  
  expect_equal(my_result, correct_result)
})


test_that("Validate that assumption checking is working properly", {
  
  correct_result <- data.frame(
    condition = c("Random", "Independence", "Normality"),
    description = c(
      "Was the sample randomly collected from the population of interest?",
      "Is the sample size (50) less than 10% of the population size?",
      "Are both the number of positives (20) and the number of negatives (30) both greater than or equal to 10?"
      )
    )
  
  data_vec = c(rep(1, 20), rep(0, 30))
  
  my_result <- check_assumptions(c(rep(1, 20), rep(0, 30)))
  
  expect_equal(my_result, correct_result)
})


test_that("Confidence Interval correctly calculated", {
  
  correct_result <- data.frame(
    conf_level = 0.95,
    lower_bound = 0.257,
    upper_bound = 0.443
  )
  
  data_vec = c(rep(1, 35), rep(0, 65))
  
  my_result <- get_conf_int(data_vec, 0.95)
  
  expect_equal(my_result, correct_result)
})


test_that("Test summary correctly calculated when no hypothesis test performed", {
  
  correct_result <- data.frame(
    p_hat = 0.75,
    n = 200,
    SE_p_hat = 0.031,
    z_value = NA_real_,
    p_value = NA_real_
  )
  
  data_vec = c(rep(1, 150), rep(0, 50))
  
  my_result <- get_test_result(data_vec, hypo_p = -1, hypo_direction = "")
  
  expect_equal(my_result, correct_result)
})


test_that("Test summary correctly calculated when no hypothesis test performed", {
  
  correct_result <- data.frame(
    p_hat = 0.75,
    n = 200,
    SE_p_hat = 0.032,
    z_value = 1.543,
    p_value = 0.061
  )
  
  data_vec = c(rep(1, 150), rep(0, 50))
  
  my_result <- get_test_result(data_vec, hypo_p = 0.7, hypo_direction = ">")
  
  expect_equal(my_result, correct_result)
})

