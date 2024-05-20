test_that("add_commentary compliments people with lots of grams", {
  correct_result <- "You go, Glen Coco!"
  
  my_result <- add_commentary("Glen Coco", 4)
  
  expect_equal(my_result, correct_result)
})