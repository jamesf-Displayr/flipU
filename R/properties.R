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
#' @export
AllVariablesNames <- function(formula)
{
    dollar.placeholder <- "wPpJPcPZGeUTPe2j"
    backtick.placeholder <- "k54UQuVJqPDA32Oe"
    space.placeholder <- "IeJkGhbMRbBLmLpK"
    formula.str <- deparse(formula)
    new.str <- ""
    inside.backticks <- FALSE
    for (i in 1:nchar(formula.str))
    {
        ch <- substr(formula.str, i, i)
        if (ch == "$")
            replacement <- dollar.placeholder
        else if (ch == "`")
        {
            inside.backticks <- !inside.backticks
            replacement <- backtick.placeholder
        }
        else if (ch == " " && inside.backticks)
            replacement <- space.placeholder
        else
            replacement <- ch
        new.str <- paste0(new.str, replacement)
    }
    var.names <- all.vars(formula(new.str))
    sapply(var.names, function(x) {
        s <- gsub(dollar.placeholder, "$", x, fixed = TRUE)
        s <- gsub(backtick.placeholder, "`", s, fixed = TRUE)
        gsub(space.placeholder, " ", s, fixed = TRUE)
    }, USE.NAMES = FALSE)
}

#' \code{CopyAttributes}
#' @description Copies the "label", "name", and "qestion" attributse for each for variable in a \code{\link{data.frame}}.
#' @param data.without.attributes A \code{\link{data.frame}}.
#' @param data.with.attributes A \code{\link{data.frame}}.
#' @return A \code{\link{data.frame}}.
#' @export
CopyAttributes <- function(data.without.attributes, data.with.attributes)
{
    if (is.list(data.without.attributes))
    {
        for (i in seq_along(data.without.attributes))
            data.without.attributes[[i]] <- CopyAttributes(data.without.attributes[[i]], data.with.attributes[[i]])
        return(data.without.attributes)
    }
    for (a in c("name", "label", "question"))
        attr(data.without.attributes, a) <- attr(data.with.attributes, a)
    data.without.attributes
}


#' \code{OutcomeName}
#' @description Find the name of the outcome variable.
#' @param formula A \code{\link{formula}}.
#' @return character.
#' @export
OutcomeName <- function(formula)
{
    if (HasOutcome(formula))
        return(AllVariablesNames(formula)[1])
    return(NULL)
}


#' \code{HasOutcome}
#' @description Checking if the formula contains an outcome (varib)i.e., dependent variable).
#' @param formula A \code{\link{formula}}.
#' @return logical
#' @export
HasOutcome <- function(formula)
{
    attr(stats::terms(formula), "response") != 0
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
    data[[OutcomeName(formula)]]
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
#' @description Checks to see if there are any NAs in a data frame.0.
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


