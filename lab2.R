
linmodEst <- function(x, y) {
  coef <- solve(t(x) %*% x) %*% t(x) %*% y
  ## degrees of freedom and standard deviation of residuals
  df <- nrow(x) - ncol(x)
  sigma2 <- sum((y - x %*% coef) ^ 2) / df
  ## compute sigma^2 * (x’x)^-1
  vcov <- sigma2 * solve(t(x) %*% x)
  colnames(vcov) <- rownames(vcov) <- colnames(x)
  list(
    coefficients = coef,
    vcov = vcov,
    sigma = sqrt(sigma2),
    df = df
  )
}

data("mtcars")
linmodEst(x = cbind(1, mtcars$wt, mtcars$hp), y= mtcars$mpg)

data("mtcars")
lm(mpg ~ wt + hp, data = mtcars)

library(Linreg)

data(cats, package = "MASS")
linmodEst(cbind(1, cats$Bwt), cats$Hwt)

data(mtcars)
linmodEst(x = cbind(1, mtcars$wt, mtcars$hp), y = mtcars$mpg)

#' Linear regression
#'
#' Runs an OLS regression not unlike \code{\link{lm}}
#'
#' @param y response vector (1 x n)
#' @param X covariate matrix (p x n) with no intercept
#'
#' @return A list with 4 elements: coefficients, vcov, sigma, df
#'
#' @examples
#' data(mtcars)
#' X <- as.matrix(mtcars[, c("cyl", "disp", "hp")])
#' y <- mtcars[, "mpg"]
#' linmodEst(y, X)
#'
#' @export
#'

# # If not already run when creating package with RStudio:
# # git init
# git add *
# git commit -m "First commit"
# # Replace cchoirat with your GitHub username
# git remote add origin https://github.com/cchoirat/Linreg
# git push -u origin master

devtools::build_vignettes()
devtools::install(build_vignettes = TRUE)

vignette("vlinreg")
# Or
vignette("vlinreg", package = "Linreg")

devtools::use_testthat()

data(cats, package = "MASS")
l1 <- linmodEst(cbind(1, cats$Bwt), cats$Hwt)
l2 <- lm(Hwt ~ Bwt, data = cats)

test_that("same estimated coefficients as lm function", {
  v1 <- c(round(l1$coefficients, 3))
  v2 <- round(l2$coefficients, 3); names(v2) <- NULL
  expect_equal(v1, v2)
})

devtools::test()
## Loading Linreg
## [...]

usethis::use_travis()

hello <- function() {
  s <- "Hello World!"
  class(s) <- "hi"
  return(s)
}

hello() # "hi"

print.hi <- function(...) {
  print("Surprise!")
}

hello() # "Surprise!"

linmod <- function(x, ...)
  UseMethod("linmod")

linmod.default <- function(x, y, ...) {
  x <- as.matrix(x)
  y <- as.numeric(y)
  est <- linmodEst(x, y)
  est$fitted.values <- as.vector(x %*% est$coefficients)
  est$residuals <- y - est$fitted.values
  est$call <- match.call()
  class(est) <- "linmod"
  return(est)
}

print.linmod <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

linmod.formula <- function(formula, data = list(), ...) {
  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)
  est <- linmod.default(x, y, ...)
  est$call <- match.call()
  est$formula <- formula
  return(est)
}

linmod(Hwt ~ - 1 + Bwt * Sex, data = cats)

