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

test_that("Check that hypothesis direction check is functional", {
  
  correct_result <- FALSE
  
  
  my_result <- valid_direction(">=")
  
  expect_equal(my_result, correct_result)
})




# 
# test_that("add_commentary compliments people with lots of grams", {
#   correct_result <- "You go, Glen Coco!"
#   
#   my_result <- add_commentary("Glen Coco", 4)
#   
#   expect_equal(my_result, correct_result)
# })
# 
# 
# test_that("add_commentary compliments people with lots of grams", {
#   correct_result <- "You go, Glen Coco!"
#   
#   my_result <- add_commentary("Glen Coco", 4)
#   
#   expect_equal(my_result, correct_result)
# })