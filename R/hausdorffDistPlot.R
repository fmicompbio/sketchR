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
#' @param q Numeric scalar in [0,1], indicating the fraction of points to 
#'     discard when calculating the robust Hausdorff distance. Setting 
#'     q=0 gives the classical Hausdorff distance. The default is 1e-4, 
#'     as suggested by Hie et al (2019).
#' @param doPlot Logical scalar, indicating whether or not to generate the 
#'     plot. 
#' @param... Additional arguments provided to \code{geosketch}.
#' 
#' @author Charlotte Soneson
#' 
#' @export
#' 
#' @return 
#' A \code{data.frame} with three columns: the size of the sketch
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
hausdorffDistPlot <- function(mat, Nvec, Nrep = 5, q = 1e-4, 
                              doPlot = TRUE, ...) {
    .assertVector(mat, type = "matrix")
    .assertVector(Nvec, type = "numeric", rngIncl = c(1, nrow(mat)))
    .assertScalar(Nrep, type = "numeric", rngIncl = c(1, Inf))
    .assertScalar(q, type = "numeric", rngIncl = c(0, 1))
    
    hausd <- do.call(rbind, lapply(Nvec, function(N) {
        do.call(rbind, lapply(seq_len(Nrep), function(i) {
            idx <- geosketch(mat = mat, N = N, ...)
            hdd <- .calcRobustHausdorffDist(mat, mat[idx, , drop = FALSE], q = q)
            data.frame(N = N, 
                       frac = N/nrow(mat),
                       HausdorffDist = hdd)
        }))
    }))
    
    if (doPlot) {
        hausdPlot <- hausd %>%
            dplyr::group_by(frac) %>%
            dplyr::summarize(mean = mean(HausdorffDist),
                             se = sd(HausdorffDist)/sqrt(length(HausdorffDist)),
                             low = mean - se,
                             high = mean + se)
        ggplot2::ggplot(hausdPlot, ggplot2::aes(x = frac, y = mean)) + 
            ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high), alpha = 0.2) +
            ggplot2::geom_point() + ggplot2::geom_line() + 
            ggplot2::theme_bw() + 
            ggplot2::scale_x_continuous(labels = scales::percent) + 
            ggplot2::labs(x = "Sketch size (% of full dataset size)",
                          y = "Mean +/- SE of Hausdorff distance") + 
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14))
    }
    
    hausd
}

#' @keywords internal
#' @noRd
#' @importFrom Biobase matchpt
.calcRobustHausdorffDist <- function(full, sketch, q = 1e-4) {
    NN <- nrow(full)
    dists <- Biobase::matchpt(full, sketch)
    K <- ceiling((1 - q) * NN)
    if (K == 0) K <- 1
    ## Find k'th largest value
    sort(dists$distance, partial = K)[K]
}
