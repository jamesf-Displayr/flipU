context("properties")

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

test_that("CopyAttributes",
          {
              z <- 1:10
              attr(z, "question") <- "Question"
              q <- 2
              expect_equal(attr(CopyAttributes(q, z), "question"), "Question")

              df <- data.frame(a = 1:10, b = z)
              df1 <- data.frame(a = 1:10, b = 1:10)
              expect_equal(attr(CopyAttributes(df1, df)$b, "question"), "Question")
          })

test_that("AllVariablesNames", {
    expect_equal(AllVariablesNames(`Cola.sav$Variables$Q2` ~ Q3), c("`Cola.sav$Variables$Q2`", "Q3"))
})
