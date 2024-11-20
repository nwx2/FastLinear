#' summary.fastlm
#'
#' Provides a summary for objects of class \code{fastlm}.
#'
#' @param object An object of class \code{fastlm}.
#' @param ... Additional arguments passed to or from other methods.
#' @return Prints the summary to the console.
#' @examples
#' set.seed(123)
#' n <- 1000
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' beta_true <- rnorm(p + 1)
#' y <- cbind(1, X) %*% beta_true + rnorm(n)
#' model_fast <- fast_lm(X, y)
#' summary(model_fast)
#'
#' @method summary fastlm
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

