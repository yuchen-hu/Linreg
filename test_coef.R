data(cats, package = "MASS")
l1 <- linmodEst(cbind(1, cats$Bwt), cats$Hwt)
l2 <- lm(Hwt ~ Bwt, data = cats)

test_that("same estimated coefficients as lm function", {
  v1 <- c(round(l1$coefficients, 3))
  v2 <- round(l2$coefficients, 3)
  names(v2) <- NULL
  expect_equal(v1, v2)
})
