#' Get names of scSampler functions
#'
#' @return A list of names of objects exposed in the scSampler module
#' @author Charlotte Soneson
#'
#' @examples
#' getScSamplerNames()
#'
#' @export
#'
#' @importFrom reticulate import
#' @importFrom basilisk basiliskStart basiliskRun basiliskStop
getScSamplerNames <- function() {
    cl <- basiliskStart(universalenv)
    scsampler.names <- basiliskRun(cl, function() {
        X <- reticulate::import("scsampler")
        names(X)
    })
    basiliskStop(cl)
    scsampler.names
}

#' Run scSampler to subsample a matrix
#'
#' Perform subsampling with the \code{scSampler} python package.
#'
#' The first time this function is run, it will create a conda environment
#' containing the \code{scSampler} package.
#' This is done via the \code{basilisk} R/Bioconductor package - see the
#' documentation for that package for troubleshooting.
#'
#' @param mat m x n matrix. Samples (the dimension along which to subsample)
#'     should be in the rows, features in the columns.
#' @param N Numeric scalar, the number of samples to retain.
#' @param random_split Numeric scalar, the number of parts to randomly
#'     split the data into before subsampling within each part. A larger
#'     value will speed up computations, but give less optimal results.
#' @param seed Numeric scalar, passed to \code{scsampler} to seed the random
#'     number generator.
#'
#' @examples
#' x <- matrix(rnorm(500), nrow = 100)
#' scsampler(mat = x, N = 10)
#'
#' @references
#' Song et al (2022): scSampler: fast diversity-preserving subsampling of
#' large-scale single-cell transcriptomic data.
#' bioRxiv doi:10.1101/2022.01.15.476407
#'
#' @author Charlotte Soneson, Michael Stadler
#'
#' @return A numeric vector with indices to retain.
#'
#' @export
#'
#' @importFrom DelayedArray is_sparse
scsampler <- function(mat, N, random_split = 1, seed = 0) {
    ## --------------------------------------------------------------------- ##
    ## Check input arguments
    ## --------------------------------------------------------------------- ##
    if (DelayedArray::is_sparse(mat)) {
        mat <- as.matrix(mat)
    }
    .assertVector(x = mat, type = "matrix")
    .assertScalar(x = N, type = "numeric",
                  rngIncl = c(1, nrow(mat)))
    N <- as.integer(N)
    .assertScalar(x = random_split, type = "numeric",
                  rngIncl = c(1, nrow(mat)))
    random_split <- as.integer(random_split)
    .assertScalar(x = seed, type = "numeric")
    seed <- as.integer(seed)

    ## --------------------------------------------------------------------- ##
    ## Run scSampler
    ## --------------------------------------------------------------------- ##
    idx <- basiliskRun(env = universalenv, fun = .run_scsampler,
                       mat = mat, n_obs = N, random_state = seed,
                       random_split = random_split)
    idx
}

# Internal function to run scSampler
.run_scsampler <- function(mat, n_obs, random_state, random_split) {
    scs <- reticulate::import("scsampler")
    sketch_index <- scs$scsampler(mat, n_obs = n_obs,
                                  random_state = random_state,
                                  random_split = random_split)
    as.numeric(unlist(sketch_index[[2]]) + 1)
}
