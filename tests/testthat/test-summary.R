#' n <- 1000
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' beta_true <- rnorm(p + 1)
#' y <- cbind(1, X) %*% beta_true + rnorm(n)
#' model_fast <- fast_lm(X, y)
#' summary.fastlm(model)
#' @export
summary.fastlm <- function(object, ...) {
  cat("Coefficients:\n")
  print(object$coefficients)
  cat("\nResidual standard error:\n")
  rss <- sum(object$residuals^2)
  n <- length(object$residuals)
  p <- length(object$coefficients)
  df <- n - p
  rse <- sqrt(rss / df)
  cat(rse, "\n")
}

