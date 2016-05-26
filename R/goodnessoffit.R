#'
#' #' @export
#' print.flipGOF = function(x, digits = max(3L, getOption("digits") - 3L), ...)
#' {
#'   cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
#'       "\n\n", sep = "")
#'   cat(paste0(c(x$description,"\n", collapse = "")))
#'   invisible(x)
#' }
#'
#' #' @describeIn GoodnessOfFit  Goodness-of-fit for a smacof object with square data (i.e., dissimilarities/distance)
#' #' @export
#' GoodnessOfFit.smacof = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
#'   stress =  object$stress
#'   names(stress) = "STRESS1"
#'   description = list("Variance Explained: ",
#'                      formatC(100 *  (1 - stress), digits = digits),
#'                      "%\n(100 - STRESS1 * 100)")
#'   GoodnessOfFitInternal(stress, description, object$call)
#' }
#'
#' #' @describeIn GoodnessOfFit  Goodness-of-fit for a smacofR object with rectangular data (e.g., preferences)
#' #' @export
#' GoodnessOfFit.smacofR = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
#'   stress =  object$stress
#'   names(stress) = "STRESS2"
#'   description = list("Variance Explained: ",
#'                      formatC(100 - stress * 100, digits = digits),
#'                      "%\n(100 - STRESS2 * 100)")
#'   GoodnessOfFitInternal(stress, description, object$call)
#' }
#'
#' #
#'
