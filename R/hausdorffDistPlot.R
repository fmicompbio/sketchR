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
#' @param doPlot Logical scalar, indicating whether or not to generate the
#'     plot.
#' @param... Additional arguments provided to \code{geosketch}.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @references
#' Hie et al (2019): Geometric sketching compactly summarizes the
#' single-cell transcriptomic landscape. Cell Systems 8, 483–493.
#'
#' Huttenlocher et al (1993): Comparing images using the Hausdorff
#' distance. IEEE Transactions on Pattern Analysis and Machine
#' Intelligence 15(9), 850-863.
#'
#' @return
#' Invisibly, a \code{data.frame} with three columns: the size of the sketch
#' as the number of samples (N), the size of the sketch as a fraction
#' of the original data set (frac), and the robust Hausdorff distance
#' (HausdorffDist).
#'
#' @examples
#' mat <- matrix(rnorm(1000), nrow = 100)
#' hdp <- hausdorffDistPlot(mat, Nvec = c(10, 25, 50))
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line
#'     theme_bw scale_x_continuous labs theme element_text
#' @importFrom dplyr %>% group_by summarize
#'
hausdorffDistPlot <- function(mat, Nvec, Nrep = 5, q = 1e-4,
                              doPlot = TRUE, ...) {
    .assertVector(x = mat, type = "matrix")
    .assertVector(x = Nvec, type = "numeric", rngIncl = c(1, nrow(mat)))
    Nvec <- round(Nvec)
    .assertScalar(x = Nrep, type = "numeric", rngIncl = c(1, Inf))
    .assertScalar(x = q, type = "numeric", rngIncl = c(0, 1))
    .assertScalar(x = doPlot, type = "logical")

    hausd <- do.call(rbind, lapply(Nvec, function(N) {
        do.call(rbind, lapply(seq_len(Nrep), function(i) {
            idx <- geosketch(mat = mat, N = N, ...)
            hdd <- .calcRobustDirectedHausdorffDist(
                mat, mat[idx, , drop = FALSE], q = q
            )
            data.frame(N = N,
                       frac = N/nrow(mat),
                       HausdorffDist = hdd)
        }))
    }))

    if (doPlot) {
        hausdPlot <- hausd %>%
            dplyr::group_by(.data$frac) %>%
            dplyr::summarize(
                mean = mean(.data$HausdorffDist),
                se = stats::sd(.data$HausdorffDist) /
                    sqrt(length(.data$HausdorffDist)),
                low = .data$mean - .data$se,
                high = .data$mean + .data$se
            )
        print(
            ggplot2::ggplot(hausdPlot,
                            ggplot2::aes(x = .data$frac, y = .data$mean)) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low,
                                                  ymax = .data$high),
                                     alpha = 0.2) +
                ggplot2::geom_point() + ggplot2::geom_line() +
                ggplot2::theme_bw() +
                ggplot2::scale_x_continuous(labels = scales::percent) +
                ggplot2::labs(x = "Sketch size (% of full dataset size)",
                              y = "Mean +/- SE of Hausdorff distance") +
                ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                               axis.title = ggplot2::element_text(size = 14))
        )
    }

    invisible(hausd)
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