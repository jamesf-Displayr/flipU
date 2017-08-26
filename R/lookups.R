#' LookupName
#'
#' @param value The value to be look uped in a \code{dictionary}.
#' @param dictionary A \code{\link{vector}} of values.
#' @param details Returns the name of the element in a vector corresponding to the \code{value}.
#' Where the vector is un-named, its index is
#' returned. Where the value appears multiple times a warning is provided. Returns a \code{NULL} if
#' the value is not found.
#' @export
LookupName <- function(value, dictionary)
{
    if (is.list(dictionary))
        return(sapply(dictionary, function(x) LookupName(value = value, x)))
    if (is.null(names(dictionary)))
        names(dictionary) <- 1:length(dictionary)
    matches <- dictionary %in% value
    if (all(!matches))
        return(NULL)
    nms <- names(dictionary)[matches]
    if (length(nms) > 1)
        warning(paste0("''", value, "'appears in the dictionnary multiple times. The first has been used."))
    nms[1]
}
