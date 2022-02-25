#' Create diagnostic plot of Hausdorff distances
#'
#' Create diagnostic plot showing the Hausdorff distance between a sketch
#' and the full data set, for varying sketch sizes.
#'
#' @param mat m x n matrix. Samples (the dimension along which to subsample)
#'     should be in the rows, features in the columns.
#' @param Nvec Numeric vector of sketch sizes.
#' @param Nrep Numeric scalar indicating the number of sketches to draw
#'     for each sketch size.
#' @param q Numeric scalar in [0,1], indicating the fraction of largest
#'     minimum distances to discard when calculating the robust Hausdorff
#'     distance. Setting q=0 gives the classical Hausdorff distance.
#'     The default is 1e-4, as suggested by Hie et al (2019).
#' @param seed Numeric scalar or \code{NULL} for initialization of the
#'     random number generator.
#' @param methods Character vector, indicating which method(s) to include
#'     in the plot. Should be a subset of c("geosketch", "scsampler",
#'     "uniform"), where "uniform" randomly samples from input features
#'     with uniform probabilities.
#' @param extraArgs Named list providing extra arguments to the respective
#'     methods (beyond the matrix and the sketch size). The names of the list
#'     should be the method names (currently, "geosketch" or "scsampler"),
#'     and each list element should be a named list of argument values. See
#'     the examples for an illustration of how to use this argument. Note that
#'     the \code{seed} argument, if provided to any of the methods,
#'     will be ignored in favor of the global \code{seed} argument
#'     (since the former would imply providing the same seed for each
#'     repeated run of the sketching).
#'
#' @author Charlotte Soneson, Michael Stadler
#'
#' @export
#'
#' @references
#' Hie et al (2019): Geometric sketching compactly summarizes the
#' single-cell transcriptomic landscape. Cell Systems 8, 483–493.
#'
#' Song et al (2022): scSampler: fast diversity-preserving subsampling of
#' large-scale single-cell transcriptomic data.
#' bioRxiv doi:10.1101/2022.01.15.476407
#'
#' Huttenlocher et al (1993): Comparing images using the Hausdorff
#' distance. IEEE Transactions on Pattern Analysis and Machine
#' Intelligence 15(9), 850-863.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' ## Generate example data matrix
#' mat <- matrix(rnorm(1000), nrow = 100)
#'
#' ## Generate diagnostic Hausdorff distance plot
#' ## (including all available methods)
#' hdp <- hausdorffDistPlot(mat, Nvec = c(10, 25, 50))
#'
#' ## Provide additional arguments for geosketch
#' hdp <- hausdorffDistPlot(mat, Nvec = c(10, 25, 50), Nrep = 2,
#'                          extraArgs = list(geosketch = list(max_iter = 100)))
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line
#'     theme_bw scale_x_continuous labs theme element_text
#' @importFrom dplyr %>% group_by summarize
#' @importFrom stats runif
#'
hausdorffDistPlot <- function(mat, Nvec, Nrep = 5, q = 1e-4,
                              seed = NULL,
                              methods = c("geosketch", "scsampler", "uniform"),
                              extraArgs = list()) {
    ## --------------------------------------------------------------------- ##
    ## Check input arguments
    ## --------------------------------------------------------------------- ##
    .assertVector(x = mat, type = "matrix")
    .assertVector(x = Nvec, type = "numeric", rngIncl = c(1, nrow(mat)))
    Nvec <- as.integer(Nvec)
    .assertScalar(x = Nrep, type = "numeric", rngIncl = c(1, Inf))
    .assertScalar(x = q, type = "numeric", rngIncl = c(0, 1))
    if (!is.null(seed)) {
        .assertScalar(x = seed, type = "numeric")
        seed <- as.integer(seed)
    }
    .assertVector(x = methods, type = "character",
                  validValues = c("geosketch", "scsampler", "uniform"))
    .assertVector(x = extraArgs, type = "list")
    .assertVector(x = names(extraArgs), type = "character",
                  allowNULL = TRUE, validValues = c("geosketch", "scsampler"))
    for (nm in names(extraArgs)) {
        .assertVector(x = extraArgs[[nm]], type = "list")
    }

    ## --------------------------------------------------------------------- ##
    ## Remove any extra arguments that are already specified explicitly
    ## --------------------------------------------------------------------- ##
    extraArgs <- lapply(extraArgs, function(ea) {
        ea[!(names(ea) %in% c("mat", "N", "seed"))]
    })

    ## --------------------------------------------------------------------- ##
    ## Set the seed to initialize the random number state
    ## --------------------------------------------------------------------- ##
    if (!is.null(seed)) {
        set.seed(seed)
    }

    ## --------------------------------------------------------------------- ##
    ## Calculate Hausdorff distances
    ## --------------------------------------------------------------------- ##
    hausd <- do.call(rbind, lapply(Nvec, function(N) {
        do.call(rbind, lapply(seq_len(Nrep), function(i) {
            ## For each sketch, draw a new random seed
            newseed <- as.integer(1e7 * stats::runif(1))
            do.call(rbind, lapply(methods, function(m) {
                if (m == "geosketch") {
                    idx <- do.call(geosketch,
                                   c(list(mat = mat, N = N, seed = newseed),
                                     extraArgs[[m]]))
                } else if (m == "scsampler") {
                    idx <- do.call(scsampler,
                                   c(list(mat = mat, N = N, seed = newseed),
                                     extraArgs[[m]]))
                } else if (m == "uniform") {
                    idx <- sample.int(n = nrow(mat), size = N)
                }
                hdd <- .calcRobustDirectedHausdorffDist(
                    mat, mat[idx, , drop = FALSE], q = q
                )
                data.frame(method = m,
                           N = N,
                           frac = N/nrow(mat),
                           HausdorffDist = hdd)
            }))
        }))
    }))

    ## --------------------------------------------------------------------- ##
    ## Plot
    ## --------------------------------------------------------------------- ##
    ## For each method and sketch size, calculate mean and SE of the
    ## Hausdorff distances
    hausdPlot <- hausd %>%
        dplyr::group_by(.data$method, .data$frac) %>%
        dplyr::summarize(
            mean = mean(.data$HausdorffDist),
            se = stats::sd(.data$HausdorffDist) /
                sqrt(length(.data$HausdorffDist)),
            low = .data$mean - .data$se,
            high = .data$mean + .data$se,
            .groups = "drop"
        )
    gg <- ggplot2::ggplot(hausdPlot,
                          ggplot2::aes(x = .data$frac, y = .data$mean,
                                       group = .data$method)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low,
                                          ymax = .data$high),
                             alpha = 0.2) +
        ggplot2::geom_point(ggplot2::aes(color = .data$method)) +
        ggplot2::geom_line(ggplot2::aes(color = .data$method)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(labels = scales::percent) +
        ggplot2::labs(x = "Sketch size (% of full dataset size)",
                      y = "Mean +/- SE of Hausdorff distance") +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                       axis.title = ggplot2::element_text(size = 14))

    gg
}

#' @keywords internal
#' @noRd
#' @importFrom Biobase matchpt
.calcRobustDirectedHausdorffDist <- function(full, sketch, q = 1e-4) {
    ## Note that this function only calculates the "directed"
    ## Hausdorff distance, from the full data set to the sketch.
    ## To get a symmetric Hausdorff distance one would also
    ## calculate the distance from the sketch to the full
    ## data set; however, this will always be zero since the
    ## sketch is a subset of the full data set. Thus, the
    ## function should not be used as a general replacement for
    ## functions like pracma::hausdorff_dist that calculates the
    ## full Hausdorff distance.

    NN <- nrow(full)

    ## For each point in the full dataset, find nearest neighbor
    ## in the sketch, as well as the distance
    dists <- Biobase::matchpt(full, sketch)

    ## Find K'th largest value
    K <- ceiling((1 - q) * NN)
    if (K == 0) K <- 1
    sort(dists$distance, partial = K)[K]
}
