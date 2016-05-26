context("Missing Data")
data(bank, package = "flipData")

dat <- data.frame(a = rep((1:10)/10,2),
                  b = rep(1:10,2),
                  c = factor(rep(c(rep("A",5),rep("B",5)),2)),
                  d = ordered(rep(c(rep("A",5),rep("B",5)),2)), e = rep("dog",20), stringsAsFactors = FALSE)
for (i in 1:5)
    dat[i, i] <- NA

test_that("AnyNA",
{
    expect_that(AnyNA(dat), is_true())
    expect_that(AnyNA(dat[11:20,]), is_false())
    expect_that(AnyNA(dat[3:20, ], c ~ a + b), is_true())
    expect_that(AnyNA(dat[3:20, ], ~ a + b + c + d), is_true())
    expect_that(AnyNA(dat[3:20, ], a ~ b), is_false())
    expect_that(AnyNA(dat[3:20, ], ~ a + b), is_false())
    expect_that(AnyNA(dat[3:20, ], ~ a + b), is_false())
})


test_that("Missing options",
{
    expect_error(EstimationData(Overall ~ Overall ~  Branch, bank, missing = "Error if missing data")$estimation.data)
    expect_equal(341, suppressWarnings(nrow(EstimationData(Overall ~ Overall ~ Branch, bank, missing = "Exclude cases with missing data")$estimation.data)))
    expect_equal(823, nrow(EstimationData(Overall ~ Overall ~ Branch, bank, missing = "Use partial data")$estimation.data))
    expect_equal(823, nrow(EstimationData(Overall ~ Overall ~ Branch, bank, missing = "Use partial data (pairwise correlations)")$estimation.data))
})




#
# data(bank, package = "flipData")
# sb <- bank$ID > 100
# attr(sb, "label") <- "ID greater than 100"
# z <- flipRegression::Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = "Use partial data (pairwise correlations)", subset = bank$weight > 1, detail = FALSE)
# z
