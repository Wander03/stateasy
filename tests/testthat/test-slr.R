set.seed(123)
df <- data.frame("var1" = rnorm(50), "var2" = rnorm(50))


test_that("perform_slr input arguments test", {
  
  correct_result <- "Input 'data' must be a dataframe."
  expect_error(perform_slr("df", var1, var2), correct_result)
  
  correct_result <- "Column 'var3' not found in the dataframe."
  expect_error(perform_slr(df, var3, var2), correct_result)
  
  correct_result <- "Column 'var3' not found in the dataframe."
  expect_error(perform_slr(df, var1, var3), correct_result)
  
})

test_that("perform_slr complete run test", {
  
  correct_result <- lm(var2 ~ var1, data = df)
  expect_equal(perform_slr(df, var1, var2), correct_result)
  
})