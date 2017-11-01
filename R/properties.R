#' Test if all items in a variable are integers
#'
#' @param x A vector.
#' @return logical.
#' @export
AllIntegers <- function(x)
{
    all(x == floor(x))
}

#' Find the names of the variables in a formula
#'
#' Handles \code{.} on right hand side of formula and \code{$} within backticks in
#' variable names.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}} from which to extract variable
#' names if \code{.} is used in the formula.
#' @return A character vector of variable names appearing in \code{formula}.
#' @export
#' @importFrom stats terms
#' @examples
#' dat <- data.frame("dat$Var$y" = 1, x = 2, "`dat$Var$z`" = 3,
#'                                check.names = FALSE)
#' AllVariablesNames(`dat$Var$y` ~ ., data = dat)
#' AllVariablesNames(`dat$Var$y` ~ `dat$Var$z`, data = dat)
#' AllVariablesNames(`dat$Var$y` ~ `dat$Var$z`*x)
AllVariablesNames <- function(formula, data = NULL)
{
    ## 1) backticks in col. names get backslash escapes
    ## 2) response needs to be obtained separately
    terms <- stats::terms(formula, data = data)
    tl <- attr(terms, "term.labels")

    ## Literal backticks present in data names
    ## are escaped with '\\' in tl, so need to substitute them out.
    out <- gsub("\\\\`", "", tl)

    ## tl contains interactions, need to split them into
    ## main effects/variables and keep unique ones.
    ## Complex regex is so we don't split on a colon inside backticks
    ## i.e. a non-syntactic variable name that contains a colon
    out <- unique(unlist(strsplit(out, ":(?=([^`]*`[^`]*`)*[^`]*$)",
                                  perl = TRUE)))

    ## Need unique() below because of strange behaviour where
    ## backtick'd response sometimes appears in tl when dot on RHS
    unique(c(OutcomeName(formula), out))
}

#' Copy attributes from one object to another
#'
#' Copies the "label", "name", "question" and "questiontype" attributes
#' for each for variable in a \code{\link{data.frame}}.
#' @param data.without.attributes an object to receive attributes from, such as
#' a data.frame, list, or matrix
#' @param data.with.attributes an object to copy attributes from
#' @param attr.to.not.copy character vector of attribute names appearing in
#' \code{data.with.attributes} that should not be copied
#' @return A copy of \code{data.without.attributes} with all the attributes
#' of \code{data.with.attributes}.
#' @details In the case when both arguments are \code{data.frame}s, any attributes
#' in the columns of \code{data.with.attributes} will also be copied to
#' \code{data.without.attributes} excluding \code{class} and \code{levels}
#'
#' In the case when the inputs are data.frames (lists), names are used when
#' copying attributes in each component.  Nothing will be copied for the case
#' of lists with \code{NULL} names attribute
#' @export
CopyAttributes <- function(data.without.attributes, data.with.attributes,
                           attr.to.not.copy = c("dimnames", "names", "row.names",
                                                "dim", "class", "levels"))
{
    ## for data.frame recursion when arg1 has columns arg2 does not,
    ## atts.to.copy will be NULL and data.without.attributes is returned
    atts.to.copy <- names(attributes(data.with.attributes))
    atts.to.copy <- atts.to.copy[!atts.to.copy %in% attr.to.not.copy]
    for (a in atts.to.copy)
        attr(data.without.attributes, a) <- attr(data.with.attributes, a)

    if (is.list(data.without.attributes))
        for (n in names(data.without.attributes))
            data.without.attributes[[n]] <- CopyAttributes(data.without.attributes[[n]],
                                                           data.with.attributes[[n]])

    data.without.attributes
}


#' \code{CopyAttributes}
#' @description Copies the "label", "name", "question" and "questiontype" attributes
#' for each for variable in a \code{\link{data.frame}}.
#' @param data.without.attributes A \code{\link{data.frame}}.
#' @param data.with.attributes A \code{\link{data.frame}}.
#' @return A \code{\link{data.frame}}.
#' @noRd
copyAttributesOld <- function(data.without.attributes, data.with.attributes)
{
    if (is.list(data.without.attributes))
    {
        for (i in seq_along(data.without.attributes))
            data.without.attributes[[i]] <- CopyAttributes(data.without.attributes[[i]], data.with.attributes[[i]])
        return(data.without.attributes)
    }
    # Attention: the vector of attribute names below should be kept up to date
    # to match the attributes being assigned to Q variables.
    for (a in c("name", "label", "question", "questiontype"))
        attr(data.without.attributes, a) <- attr(data.with.attributes, a)
    data.without.attributes
}

#' Find the name of the outcome variable from a formula
#'
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}} containing the variables in the formula.
#' Currently, ignored.
#' @return Character string giving the response variable name, or \code{NULL} if
#' no response is present in \code{formula}.
#' @export
OutcomeName <- function(formula, data = NULL)
{
    if (HasOutcome(formula))
        return(as.character(formula)[2])
    return(NULL)
}


#' Check if a formula contains an outcome variable
#'
#' @param formula A \code{\link{formula}}.
#' @return logical
#' @export
HasOutcome <- function(formula)
{
    length(formula) == 3
}

#' A print function for error checking
#'
#' Prints its name and a \code{\link{summary}}.
#' @param x Something to be printed.
#' @export
PrintDetails <- function(x)
{
    cat(paste0(deparse(substitute(x)), " n:", length(x), " valid:", sum(!is.na(x)), " missing:",sum(is.na(x)), "\n"))
    print(summary(x))
    cat("\n")
}


#' Test whether a vector contains any negative values
#'
#' @param x A vector.
#' @return logical.
#' @export
AnyNegative <- function(x)
{
    min(c(x, NA), na.rm = TRUE) < 0
}

#' Check if data, or, a model description, counts or represents counts
#'
#' @param x A variable or text string describing a family (e.g., "Poisson").
#' @return logical.
#' @export
IsCount <- function(x) {
    if(is.factor(x))
        return(FALSE)
    else if (is.logical(x))
        return (FALSE)
    if(!is.numeric(x)) {
        if (!is.character(x))
            x <- x$type
        return(x == "Poisson" | x == "Quasi-Poisson" | x == "NBD")
    }
    x <- x[!is.na(x)]
    if (length(x) == 0)
        stop("No data.")
    u = unique(x)
    if (min(u, na.rm = TRUE) < 0)
        return(FALSE)
    sum(as.integer(u) != u, na.rm = TRUE) == 0}


#' Return the outcome variable from a model
#'
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}} from which to extract the variable.
#' @return A vector of data.
#' @export
OutcomeVariable <- function(formula, data)
{
    data[[OutcomeName(formula, data)]]
}

#' Check that a subset contains data
#'
#' @param subset The filter used to filter data in a model.
#' @return true if the subset contains information
#' @export
HasSubset <- function(subset)
{
    !is.null(subset) & length(subset) != 1
}


#' Check if there are any NAs in a data frame
#'
#' @param data A \code{\link{data.frame}}.
#' @param formula A \code{\link{formula}}. Where supplied, only variables in the formula are checked.
#' @return logical.
#' @export
AnyNA <- function(data, formula = NULL)
{
    if (!is.null(formula))
    {
        data <- data[, AllVariablesNames(formula, data)]
    }
    any(is.na(data))
}
