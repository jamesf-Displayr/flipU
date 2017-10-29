context("lookups")


test_that("LookupName",{
    d <- c("A" = 1, "B" = 2, "C" = 1)
    expect_equal(LookupName(2, d), "B")
    expect_equal(suppressWarnings(LookupName(1, d)), "A")
    expect_warning(LookupName(1, d))
    expect_equal(LookupName(3, d), NULL)
    d1 <- list("A" = 1, "B" = 2, "C" = 1)
    expect_equal(LookupName(2, d1), "B")
    expect_equal(suppressWarnings(LookupName(1, d1)), "A")
    expect_warning(LookupName(1, d1))
    expect_equal(LookupName(3, d1), NULL)

})


test_that("RemoveCharacterElements", # This is more extensively tested in flipTables via the functions for removing rows and columns
          {
              expect_equal(RemoveCharacterElements(LETTERS[1:5]), LETTERS[1:5])
              expect_equal(RemoveCharacterElements(LETTERS[1:5], names.to.remove = "C"), LETTERS[1:5][-3])
          })
