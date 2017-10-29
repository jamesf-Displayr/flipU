#' \code{ConvertCommaSeparatedStringToVector}
#'
#' Converts a string containing commas into a vector, trimming whistepaces along the way.
#' @param string A \code{\link{character}} to be converted.
#' @param split A \code{\link{character}} vector containing regular expressions to be used in splitting. Where multiple entries are in the vector they are recycled along the vector of \code{string} (i.e., they are not all used as delimiters).
#' @return A \code{vector} of \code{character}s.
#' @export
ConvertCommaSeparatedStringToVector <- function(string, split = ",")
{
    comma.delimited <- unlist(strsplit(string, split))
    return(TrimWhitespace(comma.delimited))
}


#' \code{TrimLeadingWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the beginning of a string.
#' @param x A \code{\link{character}} that may contain whitecontaining text to be modified.
#' @return A \code{character}.
#' @export
TrimLeadingWhitespace <- function (x) {
    if (is.list(x))
        x <- unlist(x)
    if (length(x) > 1)
        return(as.vector(unlist(sapply(x, TrimLeadingWhitespace))))
    result <- gsub("^\\s+", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}

#' \code{TrimTrailingWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the end of a string.
#' @param x A \code{\link{character}} that may contain whitecontaining text to be modified.
#' @return A \code{character}.
#' @export
TrimTrailingWhitespace <- function (x) {
    if (is.list(x))
        x <- unlist(x)
    if (length(x) > 1)
        return(as.vector(unlist(sapply(x, TrimTrailingWhitespace))))
    result <- gsub("\\s+$", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}


#' \code{TrimWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the beginning or end of a string.
#' @param x A \code{\link{character}} that may contain whitecontaining text to be modified.
#' @return A \code{character}.
#' @export
TrimWhitespace <- function (x){
    if (is.list(x))
        x <- unlist(x)
    if (length(x) > 1)
        return(as.vector(unlist(sapply(x, TrimWhitespace))))
    result <- gsub("^\\s+|\\s+$", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}





#' RemoveCharacterElements
#'
#' Removes elements from a \code{\link{character}} vector, ignoring case.
#' @param names The \code{\link{vector}} of \code{\link{character}}..
#' @param names.to.remove A character vector or delimited string containing the
#' row labels to remove.
#' @param split Character delimiter to split \code{row.names.to.remove}
#' and \code{col.names.to.remove} on. Default is to split on either of
#' \code{","} or \code{";"}. Assumed to be a regular expression; see
#' \code{\link{strsplit}}.
#' @details Trailing spaces are removed and lower/upper case is ignored.
#' @return A vector containing the names that have not been removed.
#' @export
RemoveCharacterElements <- function(names, names.to.remove = c("NET", "Total", "SUM"), split = "[;,]")
{
    if (is.null(names) || is.null(names.to.remove) || split == "")
        return(names)
    tmpname <- tolower(trimws(names))
    tmpstring <- ConvertCommaSeparatedStringToVector(names.to.remove, split)
    tmpstring <- tolower(tmpstring)
    names[!tmpname %in% tmpstring]
}
