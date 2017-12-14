#' \code{ExpectWarning}
#' @description Checks to see if the printing of an object causes warnings to appear.
#' @param my.function The function to look for warnings in.
#' @param string The string of text to be searched for in the warnings
#' @importFrom testthat expect_true
#' @export
ExpectWarning <- function(my.function, string)
{
    expect_true(containsWarning(my.function, string))
}

#' \code{ExpectNoWarning}
#' @description Checks to see if the printing of an object does not cause a warning to appear.
#' @param my.function The function to look for warnings in.
#' @param string The string of text to be searched for in the warnings
#' @importFrom testthat expect_false
#' @export
ExpectNoWarning <- function(my.function, string)
{
    expect_false(containsWarning(my.function, string))
}

#' @importFrom utils capture.output
#' @importFrom testthat capture_warnings
containsWarning <- function(my.function, string)
{
    my.warnings <- capture.output(capture_warnings(print(my.function)))
    my.matches <- (grep(string, my.warnings))
    return(length(my.matches) > 0)
}

#' @title InterceptWarnings
#' @description This function intercepts warning messages produced from running
#' \code{expr} and passes them to \code{warning.handler}.
#' @param expr The expression whose warnings are to be intercepted.
#' @param warning.handler The function that handles intercepted warnings.
#' @return The value of \code{expr}.
#' @export
InterceptWarnings <- function(expr, warning.handler)
{
    withCallingHandlers(expr,
                        warning = function(w) {
                            warning.handler(w)
                            invokeRestart("muffleWarning")
                        })
}
