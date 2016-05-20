#' Adjust data to reflect weights
#'
#' Creates a new \code{\link{data.frame}} to reflect weights.
#'
#' @param data A \code{\link{data.frame}}.
#' @param weights The sampling or replication weights.
#' @param seed The seed used in random number generation.
#' @details In situations where an algorithm does not accomodate weights, this
#' function modifies the \code{\link{data.frame}} by either: (A) stretching it
#' out, where the the weights are integers, or (B) resampling to create a new
#' bootstrapped \code{\link{data.frame}}, where the \code{weights} are
#' proportional to the probability of selection. When creating the bootstrap
#' sample, the sample size is whichever is greatest of the rounded sum and 1.
#'
#' @export

AdjustDataToReflectWeights <- function(data, weights, seed = 123)
{   # Inspired by Zelig, 13-11-15.
    set.seed(seed)
    n <- nrow(data)
    if (AllIntegers(weights))
    {   # Reproducing cases according to the values of the weights.
        replicants <- rep(seq_len(n), weights)
    }
    else
    {   # Creating bootstrapped data file by resampling.
        warning("Weights have been applied, but the algorithm you have selected ",
            "is only able to use integer valued weights. ",
            "A bootstrapped version of the dataset was constructed using the ",
            "weights as sample probabilities.")
        sum.weights <- max(round(sum(weights)), 1)
        replicants <- sample.int(n, size = sum.weights,
            replace = TRUE, prob = weights / sum.weights)
    }

    return(data[replicants, ])
}




# #' Q23 and Weights from Phone.sav
# #'
# #' 25 variables from a 5-point scale. Extra missing data has been added at random.
# #' This makes up about 20% of the values. This is to test PCA and Factor analysis.
# #'
# #' @format A list containing:
# #' \describe{
# #'   \item{data.set}{25 variables from q23}
# #'   \item{weight}{A vector of weights}
# #' }
# "pcaPhoneTestData"



# Compute a matrix containing the pairwise weighted correlations between each column in
# 'data'.
weightedPartialCovarianceMatrix <- function(data, weight, correlation = FALSE)
{
    .weightedPartialCovariance <- function(numeric1, numeric2, input.weight, correlation = FALSE)
    {
        Sumxy <- 0.0
        SumY <- 0.0
        SumX <- 0.0
        SsX <- 0.0
        SsY <- 0.0


        # Vector Version

        Wx <- input.weight * numeric1
        Wy <- input.weight * numeric2
        Wxy <- Wx * numeric2
        Wxx <- Wx * numeric1
        Wyy <- Wy * numeric2

        complete.indicator <- !is.na(Wxy) & input.weight > 0

        SumX <- sum(Wx[complete.indicator])
        SumY <- sum(Wy[complete.indicator])
        Sumxy <- sum(Wxy[complete.indicator])
        SsX <- sum(Wxx[complete.indicator])
        SsY <- sum(Wyy[complete.indicator])

        # Calculate the Population for the
        # weights for the pairwise complete observations
        NWeighted <- sum(input.weight[complete.indicator])

        # Covariance
        Value <- Sumxy - SumX * SumY / NWeighted
        if (correlation)
        {
            SdX <- sqrt(SsX - SumX * SumX / NWeighted)
            SdY <- sqrt(SsY - SumY * SumY / NWeighted)
            denominator <- SdX * SdY
            return(Value / denominator)
        } else {
            return(Value / (NWeighted-1))
        }
    }

    num.cols <- ncol(data)
    output.matrix <- matrix(0, nrow = num.cols, ncol = num.cols,
        dimnames = list(colnames(data), colnames(data)))
    for (row in 1L:num.cols)
    {
        for (col in row:num.cols)
        {
            output.matrix[row, col] <- .weightedPartialCovariance(numeric1 = data[, row],
                numeric2 = data[, col],
                input.weight = weight,
                correlation = correlation)
            output.matrix[col, row] <- output.matrix[row, col]
        }
    }
    return(output.matrix)
}


#' \code{CovarianceAndCorrelationMatrix}
#'
#' @description Generate a covariance or correlation matrix from weighted or
#'   unweighted data using either the set of complete observations, or using
#'   pairwise-complete observations.
#'
#' @param data A data frame containing the input data.
#' @param weights A numeric vector containing the value of the weight for each
#'   row of \code{data}. If weights is NULL then this function is just a wrapper
#'   for the base functions \code{cov} and \code{cor}.
#' @param pairwise A logical value. If \code{TRUE} the correlations or
#'   covariances will be computed using the complete data for each pair of
#'   variables from \code{data}. If \code{FALSE} then cases with missing data
#'   will be excluded from the computation.
#' @param use.correlation A logical value specifying whether a correlation or
#'   covariance matrix should be returned.
#' @examples
#' my.data <- cbind(c(-0.9, 0.05, 0.1, 0.8), c(1, NaN, 0, -0.9))
#' my.weight <- c(1.2, 0.8, 0.8, 1.2)
#' CovarianceAndCorrelationMatrix(my.data, weights = my.weight, pairwise = TRUE)
#' @export
CovarianceAndCorrelationMatrix <- function(data,
    weights = NULL,
    pairwise = FALSE,
    use.correlation = TRUE)
{
    # Create the input correlation or convariance matrix
    if (is.null(weights))
    {
        # Unweighted options
        if (pairwise)
        {
            use.string <- "pairwise.complete.obs"
        }
        else
        {
            use.string <- "complete.obs"
        }

        if (use.correlation)
        {
            input.matrix <- cor(data, use = use.string)
        }
        else
        {
            input.matrix <- cov(data, use = use.string)
        }
    }
    else
    {
        # Handles all cases
        input.matrix <- weightedPartialCovarianceMatrix(data,
            weight = weights,
            correlation = use.correlation)
    }
    return(input.matrix)
}



#' Variance
#'
#' Computes the weighted variance of one or more variables
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @param method The way that the weights are addressed in the computation. Currently only "SPSS" is supported.
#' @export

Variance <- function(x, weights = NULL, method = "SPSS")
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (is.null(weights))
        return(apply(x, 2, FUN = var, na.rm = TRUE))
    if (method != "SPSS")
        stop("Only SPSS supported in this function.")
    Ws <- matrix(weights, nrow(x), ncol(x))
    Ws[is.na(x)] <- NA
    sum.W <- apply(Ws, 2, sum, na.rm = TRUE)
    # xw <- sweep(x, 1, weights, "*")
    xbar <- Mean(x, weights)
    xxw <- sweep(x * x, 1, weights, "*")
    sum.xxw <- apply(xxw, 2, sum, na.rm = TRUE)
    s2 <- (sum.xxw - sum.W * xbar * xbar) / (sum.W - 1)
    s2
}


#' StandardDeviation
#'
#' Computes the weighted variance of one or more variables
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @param method The way that the weights are addressed in the computation. Currently only "SPSS" is supported.
#' @export

StandardDeviation <- function(x, weights = NULL, method = "SPSS")
{
    sqrt(Variance(x, weights, method))
}


#' Mean
#'
#' Computes the weighted mean of one or more variables. Missing values are automatically excluded.
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @export

Mean <- function(x, weights = NULL, method = "SPSS")
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (is.null(weights))
        return(apply(x, 2, FUN = mean, na.rm = TRUE))
    Ws <- matrix(weights, nrow(x), ncol(x))
    Ws[is.na(x)] <- NA
    sum.W <- apply(Ws, 2, sum, na.rm = TRUE)
    xw <- sweep(x, 1, weights, "*")
    apply(xw, 2, sum, na.rm = TRUE) / sum.W
}
