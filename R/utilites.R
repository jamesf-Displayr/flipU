#' \code{ConvertCommaSeparatedStringToVector}
#'
#' Converts a string containing commas into a vector, trimming whitepaces along
#' the way.
#' @param string A \code{\link{character}} to be converted.
#' @param split A \code{\link{character}} vector containing regular expressions
#'   to be used in splitting. Where multiple entries are in the vector they are
#'   recycled along the vector of \code{string} (i.e., they are not all used as
#'   delimiters).
#' @param text.qualifier A character that is placed at the start and end of
#'   text (after the start split and before the end split), so that it is never
#'   split even if it contains the split character.
#' @return A \code{vector} of \code{character}s.
#' @export
ConvertCommaSeparatedStringToVector <- function(string, split = ",",
                                                text.qualifier = NULL)
{
    split.text <- unlist(strsplit(string, split))

    result <- character(0)
    if (!is.null(text.qualifier) && length(split.text) > 0)
    {
        matches <- gregexpr(split, string)
        delim <- sapply(matches[[1]], function (x) {
            substr(string, x, x)
        })

        n.parts <- length(split.text)
        open.quote.index <- NA
        for (i in 1:n.parts)
        {
            t <- TrimWhitespace(split.text[i])
            nc <- nchar(t)
            if (is.na(open.quote.index))
            {
                if (substr(t, 1, 1) == "\"")
                {
                    if (nc > 1 && substr(t, nc, nc) == "\"")
                        result <- c(result, substr(t, 2, nc - 1))
                    else
                        open.quote.index <- i
                }
                else
                    result <- c(result, t)
            }
            else if (substr(t, nc, nc) == "\"")
            {
                composite <- paste0(split.text[open.quote.index:i],
                                    c(delim[open.quote.index:(i - 1)], ""),
                                    collapse = "")
                composite <- TrimWhitespace(composite)
                composite <- substr(composite, 2, nchar(composite) - 1)
                result <- c(result, composite)
                open.quote.index <- NA
            }
        }
        if (!is.na(open.quote.index)) # quotes never closed
            result <- c(result, split.text[open.quote.index:n.parts])
    }
    else
        result <- split.text

    return(TrimWhitespace(result))
}

#' \code{ParseTextList}
#'
#' Parse a string of the form "pet: cat, dog, rat", i.e., list name followed
#' by a colon and a comma-separated list of elements.
#'
#' @param string A \code{\link{character}} to be converted.
#' @return A list containing the name and elements.
#' @export
ParseTextList <- function(string)
{
    error.msg <- paste0("Input \"", string, "\" is not in the correct format.")

    matches <- gregexpr("^\\s*\".*?\"\\s*:", string, perl = TRUE)
    if (matches[[1]] != -1)
    {
        match.length <- attr(matches[[1]], "match.length")
        name <- TrimWhitespace(substr(string, 1, match.length - 1))
        name <- substr(name, 2, nchar(name) - 1) # remove double quotes
        element.string <- substr(string, match.length + 1, nchar(string))
    }
    else
    {
        matches <- gregexpr("^.*?:", string)
        if (matches[[1]] != -1)
        {
            match.length <- attr(matches[[1]], "match.length")
            name <- TrimWhitespace(substr(string, 1, match.length - 1))
            element.string <- substr(string, match.length + 1, nchar(string))
        }
        else
            stop(error.msg)
    }

    if (nchar(name) == 0)
        stop(error.msg)

    if (nchar(element.string) > 0)
        elements <- ConvertCommaSeparatedStringToVector(element.string)
    else
        stop(error.msg)

    list(name = name, elements = elements)
}

#' \code{UniquePlaceholders}
#'
#' Generates a vector of unique alphanumeric characters, which can be used as
#' placeholders in text manipulation.
#' @param n.placeholders The length of the vector.
#' @param n.characters The number of characters in each entry.
#' @return A vector of unique alphanumeric characters.
#' @export
UniquePlaceholders <- function(n.placeholders, n.characters = 64)
{
    result <- character(0)
    while (length(result) < n.placeholders)
    {
        candidate <- paste0(sample(c(LETTERS, letters, 0:9), 64, TRUE),
                            collapse = "")
        if (!(candidate %in% result)) # unlikely false but check anyway
            result <- c(result, candidate)
    }
    result
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

    x <- gsub("\\xA0", " ", x)
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

    x <- gsub("\\xA0", " ", x)
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

    # Convert non-breaking (hexcode: A0) space to normal space
    # In Windows, this automatically matches white space (\s) but not in Ubuntu
    x <- gsub("\\xA0", " ", x)
    result <- gsub("^\\s+|\\s+$", "", x)
    if (is.null(names(x)))
        names(result) <- NULL
    result
}



