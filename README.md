---
title: "README"
output: html_document
date: "2024-11-18"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## FastLinear

FastLinear is an R package that provides efficient linear regression functions using QR decomposition. It offers faster computations compared to the base R lm() function while maintaining accuracy, making it especially beneficial for large datasets with many predictors.

## Installation
You can install the FastLinear directly from GitHub:

```{r}
install.packages("devtools")
devtools::install_github("yourusername/FastLinear")
```

## Usage

```{r}
# Load the package
install.packages("FastLinear")
library(FastLinear)
install.packages("bench")

library(bench)

# Simulate data
set.seed(123)
n <- 10000  # Number of observations
p <- 10    # Number of predictors
X <- matrix(rnorm(n * p), ncol = p)
colnames(X) <- paste0("X", 1:p)
beta_true <- rnorm(p + 1)  # True coefficients including intercept
y <- cbind(1, X) %*% beta_true + rnorm(n)

# Fit the model using FastLinear
model <- fast_lm(X, y)

# View the coefficients
print(model$coefficients)

# Summarize the model
summary(model)

```
## Vignettes
For detailed examples, usage instructions, and performance comparisons with base R functions, please refer to the package vignette:
```{r}
browseVignettes("FastLinear")
```
## Benchmarking
The fast_lm function provides significant performance improvements over the base lm() function, especially with larger datasets. Benchmarks conducted using the bench package show that fast_lm can be considerably faster while producing equivalent results.
```{r}
library(bench)

# Benchmarking fast_lm vs. lm
benchmark_results <- bench::mark(
  FastLinear = fast_lm(X, y),
  lm = lm(y ~ ., data = data.frame(y = y, X)),
  iterations = 50,
  check = FALSE
)

print(benchmark_results)

```
## Correctness Verification
We can verify the correctness of fast_lm by comparing its coefficients with those from lm():
```{r}
# Fit models
model_fast <- fast_lm(X, y)
data <- data.frame(y = y, X)
model_lm <- lm(y ~ ., data = data)

# Extract coefficients
coefficients_fast <- as.numeric(model_fast$coefficients)
coefficients_lm <- coef(model_lm)

# Assign names to coefficients_fast if missing
if (is.null(names(coefficients_fast))) {
  names(coefficients_fast) <- names(coefficients_lm)
}

# Ensure both are numeric vectors
coefficients_fast <- as.numeric(coefficients_fast)
coefficients_lm <- as.numeric(coefficients_lm)

# Compare the coefficients
comparison_result <- all.equal(coefficients_fast, coefficients_lm, tolerance = 1e-6)
print(comparison_result)


```
The output TRUE indicates that the coefficients are equal within the specified tolerance, confirming the correctness of fast_lm.


