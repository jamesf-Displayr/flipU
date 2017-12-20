#' @title ExpectWarning
#' @description Checks to see if the printing of an object causes warnings to
#' appear.
#' @param code The code to evaluate for warnings.
#' @param string The string of text to be searched for in the warnings.
#' @return Returns NULL. Throws an exception if the code is evaluated and no
#' warning is thrown or the string does not appear in the warning.
#' @examples
#' throwsAWarning <- function()
#' {
#'     warning("This is a warning")
#' }
#' ExpectWarning(throwsAWarning(), "a warning")
#' @export
ExpectWarning <- function(code, string)
{
    stopifnot(containsWarning(code, string))
}

#' @title ExpectNoWarning
#' @description Checks to see if the printing of an object does not cause a
#' warning to appear.
#' @param code The code to evaluate for warnings.
#' @param string The string of text to be searched for in the warnings
#' @return Returns NULL. Throws an exception if the code is evaluated and
#' the string appears in the warning.
#' @examples
#' throwsAWarning <- function()
#' {
#'     warning("This is a warning")
#' }
#' ExpectNoWarning(throwsAWarning(), "different warning")
#' @export
ExpectNoWarning <- function(code, string)
{
    stopifnot(!containsWarning(code, string))
}

containsWarning <- function(code, string)
{
    my.warnings <- NULL
    withCallingHandlers(code, warning = function(e){
        my.warnings <<- c(my.warnings, e$message)
        invokeRestart("muffleWarning")
        })
    any(grepl(string, my.warnings))
}

containsWarning2 <- function(code, string)
{
    my.warnings <- NULL
    withCallingHandlers(code, warning = function(e){
e$message
        })
    any(grepl(string, my.warnings))
}


#' @title InterceptWarnings
#' @description This function intercepts warning messages produced from running
#' \code{expr} and passes them to \code{warning.handler}.
#' @param expr The expression whose warnings are to be intercepted.
#' @param warning.handler The function that handles intercepted warnings.
#' @return The value of \code{expr}.
#' @examples
#' throwsAWarning <- function()
#' {
#'     warning("This is a warning")
#' }
#' addExclamationMark <- function(warn)
#' {
#'     warning(warn$message, "!")
#' }
#' ## Intercepts the warning from throwsAWarning and rethrows it with an
#' ## exclamation mark.
#' InterceptWarnings(throwsAWarning(), addExclamationMark)
#' @export
InterceptWarnings <- function(expr, warning.handler)
{
    withCallingHandlers(expr,
                        warning = function(w) {
                            warning.handler(w)
                            invokeRestart("muffleWarning")
                        })
}
