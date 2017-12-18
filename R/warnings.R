#' \code{ExpectWarning}
#' @description Checks to see if the printing of an object causes warnings to appear.
#' @param code The code to evaluate for warnings.
#' @param string The string of text to be searched for in the warnings
#' @export
ExpectWarning <- function(code, string)
{
    stopifnot(containsWarning(code, string))
}

#' \code{ExpectNoWarning}
#' @description Checks to see if the printing of an object does not cause a warning to appear.
#' @param code The code to evaluate for warnings.
#' @param string The string of text to be searched for in the warnings
#' @export
ExpectNoWarning <- function(code, string)
{
    stopifnot(!containsWarning(code, string))
}

containsWarning <- function(code, string)
{
    my.warnings <- tryCatch(code, warning = function(e) e$message)
    any(grepl(string, my.warnings))
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
