#' fast_lm
#'
#' Fits a linear model using QR decomposition for improved computational efficiency.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A numeric vector of response variable.
#' @return An object of class \code{fastlm}, which is a list containing:
#' \item{coefficients}{Estimated regression coefficients.}
#' \item{fitted.values}{Predicted values.}
#' \item{residuals}{Residuals from the model.}
#' @examples
#' set.seed(123)
#' n <- 1000
#' p <- 10
#' X <- matrix(rnorm(n * p), ncol = p)
#' beta_true <- rnorm(p + 1)
#' y <- cbind(1, X) %*% beta_true + rnorm(n)
#' model_fast <- fast_lm(X, y)
#' @export
fast_lm <- function(X, y) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  X <- cbind(Intercept = 1, X)
  qr_decomp <- qr(X)
  coefficients <- qr.coef(qr_decomp, y)
  coefficients <- as.vector(coefficients)
  names(coefficients) <- colnames(X)

  fitted.values <- X %*% coefficients
  residuals <- y - fitted.values
  structure(
    list(
      coefficients = coefficients,
      fitted.values = fitted.values,
      residuals = residuals
    ),
    class = "fastlm"
  )
}

