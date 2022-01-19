#' Get names of geosketch functions
#'
#' @return A list of names of objects exposed in the geosketch module
#' @author Charlotte Soneson
#'
#' @examples
#' getGeosketchNames()
#'
#' @export
#'
#' @importFrom reticulate import
#' @importFrom basilisk basiliskStart basiliskRun basiliskStop
getGeosketchNames <- function() {
    cl <- basiliskStart(geosketchenv)
    geosketch.names <- basiliskRun(cl, function() {
        X <- reticulate::import("geosketch")
        names(X)
    })
    basiliskStop(cl)
    geosketch.names
}

#' Run geosketch to subsample a matrix
#'
#' Perform geometric sketching with the \code{geosketch} python package.
#'
#' The first time this function is run, it will create a conda environment
#' containing the \code{geosketch} package.
#' This is done via the \code{basilisk} R/Bioconductor package - see the
#' documentation for that package for troubleshooting.
#'
#' @param mat m x n matrix. Samples (the dimension along which to subsample)
#'     should be in the rows, features in the columns.
#' @param N Numeric scalar, the number of samples to retain.
#' @param replace Logical scalar, whether to sample with replacement.
#' @param k Numeric scalar or \code{"auto"}, specifying the number of covering.
#'     If \code{k = "auto"} (the default), it is set to \code{sqrt(nrow(mat))}
#'     for \code{replace = TRUE} and to \code{N} for \code{replace = FALSE}.
#' @param alpha Numeric scalar defining the acceptable interval around \code{k}.
#'     Binary search halts when it obtains between \code{k * (1 - alpha)} and
#'     \code{k * (1 + alpha)} covering boxes.
#' @param seed Numeric scalar or \code{NULL} (default). If not \code{NULL}, it
#'     will be converted to integer and passed to numpy to seed the random
#'     number generator.
#' @param max_iter Numeric scalar giving the maximum iterations at which to
#'     terminate binary search in rare cases of non-monotonicity of covering
#'     boxes.
#' @param one_indexed Logical scalar, whether to return one-indexed indices.
#' @param verbose Locigal scalar, whether to print logging output while running.
#'
#' @examples
#' x <- matrix(rnorm(500), nrow = 100)
#' geosketch(mat = x, N = 10, seed = 42)
#'
#' @references
#' Hie et al (2019): Geometric sketching compactly summarizes the
#' single-cell transcriptomic landscape. Cell Systems 8, 483–493.
#'
#' @author Charlotte Soneson, Michael Stadler
#'
#' @return A numeric vector with indices to retain.
#'
#' @export
#'
#' @importFrom DelayedArray is_sparse
geosketch <- function(mat, N, replace = FALSE, k = "auto",
                      alpha = 0.1, seed = NULL, max_iter = 200,
                      one_indexed = TRUE, verbose = FALSE) {
    ## --------------------------------------------------------------------- ##
    ## Check input arguments
    ## --------------------------------------------------------------------- ##
    if (DelayedArray::is_sparse(mat)) {
        mat <- as.matrix(mat)
    }
    .assertVector(x = mat, type = "matrix")
    .assertScalar(x = replace, type = "logical")
    .assertScalar(x = N, type = "numeric",
                  rngIncl = c(1, ifelse(replace, Inf, nrow(mat))))
    N <- as.integer(N)
    if (!identical(k, "auto")) {
        .assertScalar(x = k, type = "numeric", rngIncl = c(1, Inf))
        k <- as.integer(k)
    }
    .assertScalar(x = alpha, type = "numeric", rngIncl = c(0, 1))
    if (!is.null(seed)) {
        .assertScalar(x = seed, type = "numeric")
        seed <- as.integer(seed)
    }
    .assertScalar(x = max_iter, type = "numeric", rngIncl = c(1, Inf))
    max_iter <- as.integer(max_iter)
    .assertScalar(x = one_indexed, type = "logical")
    .assertScalar(x = verbose, type = "logical")

    ## --------------------------------------------------------------------- ##
    ## Run geosketch
    ## --------------------------------------------------------------------- ##
    idx <- basiliskRun(env = geosketchenv, fun = .run_geosketch,
                       mat = mat, N = N, replace = replace, k = k,
                       alpha = alpha, seed = seed, max_iter = max_iter,
                       one_indexed = one_indexed, verbose = verbose)
    idx
}

# Internal function to run geosketch
.run_geosketch <- function(mat, N, replace, k, alpha, seed, max_iter,
                           one_indexed, verbose) {
    gsk <- reticulate::import("geosketch")
    sketch_index <- gsk$gs(X = mat, N = N, k = k, seed = seed,
                           replace = replace, alpha = alpha,
                           max_iter = max_iter, one_indexed = one_indexed,
                           verbose = verbose)
    unlist(sketch_index)
}
