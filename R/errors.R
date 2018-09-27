#' @title CheckVariableLengths
#' @param variable.list A list of variables from a form.dropBox control with
#'     multiple inputs.
#' @param list.name The name used to refer to the variable list in the error
#'     message.
#' @return Nothing is returned, but an error is thrown if the variables have
#'     differing lengths.
#' @export
CheckVariableLengths <- function(variable.list, list.name)
{
    if (length(unique(sapply(variable.list, length))) > 1)
        stop("The ", list.name , " have differing lengths. Please ensure ",
             "that they are from the same data set or have the same length.")
}
