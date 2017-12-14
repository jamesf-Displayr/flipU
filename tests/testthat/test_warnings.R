context("warnings")

test_that("Warnings",
{
    ExpectWarning(warning("we have a problem"), "problem")
    ExpectNoWarning(warning("we have a problem"), "issue")
    ExpectNoWarning(message("this is just a message, not a warning"), "this")
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
    ExpectWarning(InterceptWarnings(warning("A", call. = FALSE), swapAToB), "B")
    ExpectWarning(InterceptWarnings(warning("C", call. = FALSE), swapAToB), "C")
})
