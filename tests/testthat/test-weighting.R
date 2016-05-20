context("Weighting")

dat <- data.frame(x = 1:2, y = 2:1)
sm <- sum(AdjustDataToReflectWeights(dat, 2:1))
# sm

test_that("Replicating data file with integer weights", {
    d = AdjustDataToReflectWeights(dat, 2:1)
    expect_that(nrow(d), equals(3))
    expect_that(sum(d), equals(9))
})

test_that("Creating bootrapped sample with weights", {
    # Small sample.
    d <- suppressWarnings(AdjustDataToReflectWeights(dat, (2:1) / 10))
    expect_that(nrow(d), equals(1))
    expect_that(sum(d), equals(3))
    expect_warning(AdjustDataToReflectWeights(dat, (2:1) / 10))

    # Moderate sample.
    d <- suppressWarnings(AdjustDataToReflectWeights(dat[rep(1:2, 50), ], runif(100)))
    expect_that(nrow(d), equals(50))
    expect_that(sum(d), equals(150))
    expect_warning(AdjustDataToReflectWeights(dat[rep(1:2, 50), ], runif(100)))
})


test_that("Mean is correct.", {
    # Small sample.
    expect_that(Mean(1:10), equals(5.5))
    expect_that(Mean(1:10, 1:10), equals(7))
    expect_that(Mean(c(100, 1:10, NA), c(NA, 1:10, 100)), equals(7))
    expect_that(Mean(cbind(1:10,1:10), 1:10)[2], equals(7))
})



test_that("StandardDeviation is correct.", {
    # Small sample.
    expect_that(StandardDeviation(1:10), equals(StandardDeviation(1:10, rep(1, 10))))
    expect_that(StandardDeviation(1:10, 1:10), equals(StandardDeviation(rep(1:10, 1:10))))
    expect_that(StandardDeviation(1:10, 1:10), equals(StandardDeviation(c(100, 1:10, NA), c(NA, 1:10, 100))))
    expect_that(StandardDeviation(cbind(1:10, 1), 1:10)[1], equals(StandardDeviation(rep(1:10, 1:10))))
    expect_that(Mean(c(100, 1:10, NA), c(NA, 1:10, 100)), equals(7))
    expect_that(Mean(cbind(1:10,1:10), 1:10)[2], equals(7))
})

