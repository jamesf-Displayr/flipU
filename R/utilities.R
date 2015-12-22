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
RemoveRowsAndOrColumns = function(x,
        row.names.to.remove = c("NET", "Total", "SUM"),
        column.names.to.remove = c("NET", "Total", "SUM"))
{
    x[!rownames(x) %in% row.names.to.remove, !colnames(x) %in% column.names.to.remove ]
}

