#' Check that all items in a variable are integers
#'
#' @param x The variable to check.
#'
#' @export
AllIntegers <- function(x)
{
    all(x %% 1 == 0)
}
