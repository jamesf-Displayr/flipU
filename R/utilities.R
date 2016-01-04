#'  \code{IfThen}
#' @description If else statment as a single line.
#' @param condition A logical expression.
#' @param TRUE.result The object to be returned if \code{condition} is \code{TRUE}.
#' @param FALSE.result The object to be returned if \code{condition} is \code{FALSE}.
#' @export
IfThen <- function(condition, TRUE.result, FALSE.result)
{
    if(condition)
        return(TRUE.result);
    return(FALSE.result);
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
    x[!rownames(x) %in% row.names.to.remove, !colnames(x) %in% column.names.to.remove ]
}

#' \code{FactorsToIndicators}
#' @description Convert a factor variable to a matrix whose columns are binary variables
#' representing one of the levels from the factor variable.
#' @param variable The factor variable to convert.
#' @param variable.name The name of the input variable.
#' @export
FactorToIndicators <- function(variable, variable.name = deparse(substitute(variable)))
{
    result <- model.matrix( ~ variable - 1)
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
    return(model.matrix( ~ x - 1))
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
#' @param list.of.variables A list containing the variables to combine. The elements of the list should be of class
#' numeric, factor, or ordered factor.
#' @param coerce.to.numeric A boolean value specifying whether or not factor variables should be coerced to numeric.
#' @export
ListToDataFrame <- function(list.of.variables, coerce.to.numeric = FALSE)
{
    result <- NULL
    nms <- names(list.of.variables)
    counter <- 0
    for (variable in list.of.variables)
    {
        counter <- counter + 1
        variable.name <- nms[counter]
        if(is.character(variable))
        {
            stop(paste0("Variable ", counter, " is a Text variable. It needs to be converted to numeric data if to be used in cluster analysis."))
        }
        if (is.data.frame(variable))
        {
            transformed.variable <- ListToDataFrame(variable, coerce.to.numeric)
            colnames(transformed.variable ) <- paste0(variable.name, ":", colnames(transformed.variable ))
        } else {
            if (coerce.to.numeric && is.factor(variable))
            {
                transformed.variable <- FactorToNumeric(variable, variable.name)#,                 variable.name = nms[counter])
            } else {
                    transformed.variable <- variable
            }
        }
        if (is.null(result))
        {
            result <- as.data.frame(transformed.variable)
        } else {
            result <- cbind(result, as.data.frame(transformed.variable))
        }

        if (is.null(ncol(transformed.variable)))
        {
            colnames(result)[ncol(result)] <- variable.name
        }
    }
    return(result)
}
