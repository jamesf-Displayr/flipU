context("warnings")

test_that("Warnings",
{
    ExpectWarning(warning("we have a problem"), "problem")
    ExpectNoWarning(warning("we have a problem"), "issue")
    ExpectNoWarning(message("this is just a message, not a warning"), "this")
})
