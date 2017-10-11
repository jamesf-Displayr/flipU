#' \code{AllIntegers}
#' @description  that all items in a variable are integers.
#' @param x A vector.
#' @return logical.
#' @export
AllIntegers <- function(x)
{
    all(x %% 1 == 0)
}

#' \code{AllVariablesNames}
#' @description Find the names of the variables (including those in dataframes) in a formula.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @export
AllVariablesNames <- function(formula, data = NULL)
{
    if (length(formula) == 3) {
        if (formula[3] == ".()") {
            if (is.null(data))
                stop("If predictor variables are specified by '.' then data must be given to extract names.")
            dep <- all.vars(formula)[1]
            indep <- colnames(data)
            indep <- indep[indep != dep]
            return(c(dep, indep))
        }
    }

    .randomStr <- function(n.characters = 16)
    {
        paste0(sample(c(letters, LETTERS), n.characters, replace = TRUE), collapse = "")
    }

    dollar.placeholder <- .randomStr()
    replaced.text <- list()
    replaced.text[[dollar.placeholder]] <- "$"

    formula.str <- paste0(deparse(formula), collapse = "")
    new.str <- ""
    inside.backticks <- FALSE
    backtick.start <- NA
    # We need to replace parts of the formula with placeholders in order to use all.vars()
    for (i in 1:nchar(formula.str))
    {
        ch <- substr(formula.str, i, i)
        if (ch == "$" && !inside.backticks)
            new.str <- paste0(new.str, dollar.placeholder)
        else if (ch == "`")
        {
            if (inside.backticks)
            {
                placeholder <- .randomStr()
                replaced.text[[placeholder]] <- substr(formula.str, backtick.start, i)
                new.str <- paste0(new.str, placeholder)
            }
            else
                backtick.start <- i
            inside.backticks <- !inside.backticks
        }
        else if (!inside.backticks)
            new.str <- paste0(new.str, ch)
    }

    var.names <- all.vars(formula(new.str))

    # Replace the placeholders in the variable names
    sapply(var.names, function(x) {
        for (nm in names(replaced.text))
            x <- gsub(nm, replaced.text[[nm]], x, fixed = TRUE)
        x
    }, USE.NAMES = FALSE)
}

#' Copy attributes from one object to
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

#' \code{OutcomeName}
#' @description Find the name of the outcome variable.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @return character.
#' @export
OutcomeName <- function(formula, data = NULL)
{
    if (HasOutcome(formula))
        return(AllVariablesNames(formula, data = data)[1])
    return(NULL)
}


#' \code{HasOutcome}
#' @description Checking if the formula contains an outcome i.e., dependent variable).
#' @param formula A \code{\link{formula}}.
#' @return logical
#' @export
HasOutcome <- function(formula)
{
    length(formula) == 3
}

#' \code{PrintDetails}
#' @description A print function for error checking.
#' Prints its name and a \code{\link{summary}}.
#' @param x Something to be printed.
#' @export
PrintDetails <- function(x)
{
    cat(paste0(deparse(substitute(x)), " n:", length(x), " valid:", sum(!is.na(x)), " missing:",sum(is.na(x)), "\n"))
    print(summary(x))
    cat("\n")
}

#' #' \code{UnclassIfNecessary}
#' #' @description Unclasses a variable if it is a factor. Otherwise, returns x.
#' #' @param x A vector.
#' #' @return A vector
#' #' @export
#' UnclassIfNecessary <- function(x)
#' {
#'     if(is.factor(x))
#'         return(unclass(x));
#'     return(x);
#' }

#' \code{AnyNegative}
#' @description The values contain a negative value.
#' @param x A vector.
#' @return logical.
#' @export
AnyNegative <- function(x)
{
    min(c(x, NA), na.rm = TRUE) < 0
}

#' \code{IsCount}
#' @description Checks of data, or, a model description, counts or represents counts.
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


#' \code{OutcomeVariable}
#' @description Returns the outcome variable from a model.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @return A vector of data.
#' @export
OutcomeVariable <- function(formula, data)
{
    data[[OutcomeName(formula, data)]]
}

#' \code{HasSubset}
#' @description Checks that the subset contains data.
#' @param subset The filter used to filter data in a model.
#' @return true if the subset contains information
#' @export
HasSubset <- function(subset)
{
    !is.null(subset) & length(subset) != 1
}


#' \code{AnyNA}
#' @description Checks to see if there are any NAs in a data frame.
#' @param data A \code{\link{data.frame}}.
#' @param formula A \code{\link{formula}}. Where supplied, only variables in the formula are checked.
#' @return logical.
#' @export
AnyNA <- function(data, formula = NULL)
{
    if (!is.null(formula))
    {
        data <- data[, AllVariablesNames(formula)]
    }
    any(is.na(data))
}


