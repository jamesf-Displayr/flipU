#' \code{GetTidyTwoDimensionalArray}
#' @description Checks that an array is two dimensional and tidies if appropriate (assuming it is a Q table).
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @export
GetTidyTwoDimensionalArray <- function(x, row.names.to.remove = NULL, column.names.to.remove = NULL)
{
    dim.x <- dim(x)
    dim.names <- dimnames(x)
    if (length(dim.x) != 2)
    {
        if (length(dim.x) == 3 & !is.null(dim.names))
        {
            x <- x[ , ,1]
            warning(paste0("Correspondence analysis has been performed on the first statistic in the table (",
                           dim.names[[3]][1], ")."))
            if (is.character(x[1,1]))
                x <- matrix(as.numeric(x), nrow(x), dimnames = dimnames(x))
        }
        else
        {
            stop("This analysis requires a two-dimensional table (i.e., a table with one set of row headings, one set of columns headings, and one statistic in each cell.")
        }
    }
    if (is.null(dim.names))
    {
        dimnames(x) <- list(Rows = 1:nrow(x), Columns = 1:ncol(x))
    }
    else
    {
        x <- RemoveRowsAndOrColumns(x, row.names.to.remove, column.names.to.remove)
    }
    x
}

#' \code{RemoveRowsAndOrColumns}
#' @description Removes rows or columns from the table.
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @export
RemoveRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"))
{
    x[!rownames(x) %in% row.names.to.remove, !colnames(x) %in% column.names.to.remove, drop = FALSE]
}

#' \code{FactorsToIndicators}
#' @description Convert a factor variable to a matrix whose columns are binary variables
#' representing one of the levels from the factor variable.
#' @param variable The factor variable to convert.
#' @param variable.name The name of the input variable.
#' @export
FactorToIndicators <- function(variable, variable.name = deparse(substitute(variable)))
{
    result <- stats::model.matrix( ~ variable - 1)
    colnames(result) <- paste0(variable.name, ":", levels(variable))
    result
}

#' \code{OrderedToNumeric}
#' @description Convert an ordered factor to a numeric vector.
#' @param x An ordered factor.
#' @export
OrderedToNumeric <- function(x)
{
    if (is.ordered(x))
    {
        return(unclass(x))
    }
    return(stats::model.matrix( ~ x - 1))
}

#' \code{FactorToNumeric}
#' @description Convert a factor variable to a numeric vector (when the factor is ordered),
#' or a matrix of indicator variables (when the factor is not ordered).
#' @param x A factor or ordered factor.
#' @param variable.name The name of the variable.
#' @export
FactorToNumeric <- function(x, variable.name = deparse(substitute(x)))
{
    if (is.ordered(x))
    {
        return(OrderedToNumeric(x))
    }
    indicators <- FactorToIndicators(x, variable.name)
    if (nrow(indicators) < length(x))
    {
        new.indicators <- matrix(NA, length(x), ncol(indicators))
        row.names <- as.numeric(dimnames(indicators)[[1]])
        colnames(new.indicators) <- colnames(indicators)
        new.indicators[row.names, ] <- indicators
        return(new.indicators)
    }
    return(indicators)
}


#' \code{ListToDataFrame}
#' @description Coerce a list of numeric or factor variables into a data frame.
#' @param list.of.variables A list containing the variables to combine. The
#'   elements of the list should be of class numeric, factor, or ordered factor.
#' @param coerce.to.numeric A boolean value specifying whether or not factor
#'   variables should be coerced to numeric.
#' @export
ListToDataFrame <- function(list.of.variables, coerce.to.numeric = FALSE)
{
    result <- NULL
    for (counter in seq(along = list.of.variables))
    {
        variable <- list.of.variables[[counter]]
        variable.name <- names(list.of.variables)[counter]

        if (is.null(variable.name) || variable.name == "")
        {
            variable.name <- counter
        }

        if (is.character(variable))
        {
            stop("Variable '", variable.name,
                "' is a Text variable. It needs to be converted to numeric data if to be used in cluster analysis.")
        }

        if (is.data.frame(variable))
        {
            transformed.variable <- ListToDataFrame(variable, coerce.to.numeric)
            colnames(transformed.variable) <- paste0(variable.name, ":", colnames(transformed.variable))
        }
        else
        {
            if (coerce.to.numeric && is.factor(variable))
            {
                transformed.variable <- FactorToNumeric(variable, variable.name)#,                 variable.name = nms[counter])
            }
            else
            {
                transformed.variable <- variable
            }
        }

        if (is.null(result))
        {
            result <- as.data.frame(transformed.variable)
        }
        else
        {
            result <- cbind(result, as.data.frame(transformed.variable))
        }

        if (is.null(ncol(transformed.variable)))
        {
            colnames(result)[ncol(result)] <- variable.name
        }
    }

    return (result)
}
