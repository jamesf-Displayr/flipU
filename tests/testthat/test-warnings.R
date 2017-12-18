context("warnings")

test_that("Warnings",
{
    expect_silent(ExpectWarning(warning("we have a problem"), "problem"))
    expect_silent(ExpectNoWarning(warning("we have a problem"), "issue"))
    expect_error(ExpectNoWarning(message("this is just a message, not a warning"), "this"), NA)
})

test_that("Intercept warnings",
{
    swapAToB <- function(warn)
    {
        if (warn$message == "A")
            warning("B", call. = FALSE)
        else
            warning(warn)
    }
    expect_silent(ExpectWarning(InterceptWarnings(warning("A", call. = FALSE), swapAToB), "B"))
    expect_silent(ExpectWarning(InterceptWarnings(warning("C", call. = FALSE), swapAToB), "C"))
})
