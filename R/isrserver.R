#' @title IsRServer
#' @description This function indicates if it is being run on an R server.
#' @return TRUE if running on an R server. False otherwise.
#' @export
IsRServer <- function()
{
    node.name <- Sys.info()[["nodename"]]
    node.name == "reusdev" ||
        grepl("^reustest.*", node.name) ||
        grepl("^reusprod.*", node.name)
}

#' @title IsTestRServer
#' @description This function indicates if it is being run on the test R
#' server.
#' @return TRUE if running on the test R server. False otherwise.
#' @export
IsTestRServer <- function()
{
    node.name <- Sys.info()[["nodename"]]
    grepl("^reustest.*", node.name)
}
