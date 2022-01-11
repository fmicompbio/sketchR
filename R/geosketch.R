#' @importFrom DelayedArray is_sparse
.make_np_friendly <- function(x) {
    if (is_sparse(x)) {
        as(x, "dgCMatrix")
    } else {
        as.matrix(x)
    }
}

#' Get names of geosketch functions
#'
#' @return A list of names of objects exposed in the geosketch module
#' @author Charlotte Soneson
#'
#' @examples
#' getGeosketchNames()
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
#' @param mat m x n matrix. Samples (the dimension along which to subsample)
#'     should be in the rows, features in the columns.
#' @param N Numeric scalar, the number of samples to retain.
#' @param replace Logical scalar, whether to sample with replacement.
#' @param one_indexed Logical scalar, whether to return one-indexed indices.
#'
#' @export
#'
#' @author Charlotte Soneson
#'
#' @return A numeric vector with indices to retain.
#'
geosketch <- function(mat, N, replace = FALSE, one_indexed = TRUE) {
    N <- as.integer(N)
    idx <- basiliskRun(env = geosketchenv, fun = .run_geosketch,
                       mat = mat, N = N, replace = replace,
                       one_indexed = one_indexed)
    idx
}

# Internal function to run geosketch
.run_geosketch <- function(mat, N, replace, one_indexed) {
    gsk <- import("geosketch")
    mat <- .make_np_friendly(mat)
    sketch_index <- gsk$gs(mat, N, replace = replace,
                           one_indexed = one_indexed)
    unlist(sketch_index)
}
