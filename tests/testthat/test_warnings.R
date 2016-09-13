context("warnings")

test_that("Heteroskedasticity",
          {
              library(flipRegression)
              y  <- 1:100 + .001
              x <- rnorm(100, y, y)
              ExpectWarning(Regression(y ~ x, robust.se = FALSE), "Breusch")
              ExpectNoWarning(Regression(y ~ x, robust.se = FALSE), "zzBreusch")
              ExpectNoWarning(Regression(y ~ x, robust.se = TRUE), "Breusch")

          })
