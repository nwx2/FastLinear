test_that("fast_lm produces correct coefficients compared to lm()", {
  set.seed(1)
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), ncol = p)
  colnames(X) <- paste0("X", 1:p)
  beta_true <- rnorm(p + 1)
  y <- cbind(1, X) %*% beta_true + rnorm(n)

  # Fit models
  model_fast <- fast_lm(X, y)
  data <- data.frame(y = y, X)
  model_lm <- lm(y ~ ., data = data)

  # Extract coefficients
  coef_fast <- model_fast$coefficients
  coef_lm <- coef(model_lm)

  # Convert coef_fast to numeric vector
  coef_fast <- as.vector(coef_fast)

  # Assign names to coef_fast
  names(coef_fast) <- names(coef_lm)

  # Test if coefficients are equal within tolerance
  expect_equal(coef_fast, coef_lm, tolerance = 1e-6)
})
