test_that("summary.fastlm outputs the correct components", {
  set.seed(2)
  X <- matrix(rnorm(50), ncol = 2)
  y <- rnorm(25)
  model <- fast_lm(X, y)

  # Capture the output of summary
  summary_output <- capture.output(summary(model))

  # Check if the output contains expected text
  expect_true(any(grepl("Coefficients:", summary_output)))
  expect_true(any(grepl("Residual standard error:", summary_output)))
})
