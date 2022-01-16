#' Compare the compositions of a data set and a subset
#' 
#' Plot the composition of a data set (e.g., the number of cells from 
#' each cell type) and contrast it with the corresponding composition 
#' of a subset. 
#' 
#' @param df A \code{data.frame} or \code{DFrame} object.
#' @param idx A numeric vector representing the row indexes of \code{df} 
#'     corresponding to the subset of interest. Can also be a named list 
#'     of index vectors if multiple subsets are of interest.
#' @param column A character scalar corresponding to a column of 
#'     \code{df} and representing the variable for which the composition 
#'     should be calculated.
#' \param showPercentages Logical scalar, indicating whether relative 
#'     frequencies of each category should be shown in the plot.
#' \param fontSizePercentages Numerical scalar, indicating the font size 
#'     of the relative frequencies, if \code{showPercentages} is \code{TRUE}.
#' \param doPlot Logical scalar, indicating whether the plot should be drawn. 
#' 
#' @return Invisibly, a \code{data.frame} with absolute and relative 
#' frequencies of each category in each set of observations. 
#' 
#' @author Charlotte Soneson
#' 
#' @export
#' 
#' @examples 
#' df <- data.frame(celltype = sample(LETTERS[1:5], 1000, 
#'                                    prob = c(0.1, 0.2, 0.5, 0.05, 0.15)))
#' idx <- sample(seq_len(1000), 200)
#' compareCompositionPlot(df, idx, "celltype")
#' 
#' @importFrom ggplot2 ggplot aes geom_bar theme_bw theme facet_wrap 
#'     element_text geom_text
#' @importFrom dplyr mutate group_by ungroup %>% select
#' @importFrom rlang .data
#' @importFrom scales percent
#'      
compareCompositionPlot <- function(df, idx, column, showPercentages = TRUE, 
                                   fontSizePercentages = 4, doPlot = TRUE) {
    ## --------------------------------------------------------------------- ##
    ## Check arguments
    ## --------------------------------------------------------------------- ##
    if (is.list(idx)) {
        .assertVector(x = names(idx), type = "character", allowNULL = FALSE)
        for (nm in names(idx)) {
            .assertVector(x = idx[[nm]], type = "numeric")
        }
    } else {
        .assertVector(x = idx, type = "numeric")
    }
    .assertScalar(column, type = "character")
    if (!(column %in% names(df))) {
        stop("'column' is not in names(df)")
    }
    .assertScalar(x = showPercentages, type = "logical")
    .assertScalar(x = fontSizePercentages, type = "numeric")
    .assertScalar(x = doPlot, type = "logical")
    
    ## --------------------------------------------------------------------- ##
    ## Create data.frame for plotting
    ## --------------------------------------------------------------------- ##
    dfplot <- as.data.frame(table(df[[column]])) %>%
        dplyr::mutate(Var1 = as.character(Var1)) %>%
        setNames(c(column, "Frequency")) %>%
        dplyr::mutate(group = "Full dataset")
    
    if (is.list(idx)) {
        for (nm in names(idx)) {
            dfplot <- rbind(
                dfplot,
                as.data.frame(table(df[[column]][idx[[nm]]])) %>%
                    dplyr::mutate(Var1 = as.character(Var1)) %>%
                    setNames(c(column, "Frequency")) %>%
                    dplyr::mutate(group = nm)
            )
        }
        dfplot$group <- factor(dfplot$group, levels = c("Full dataset", names(idx)))
    } else {
        dfplot <- rbind(
            dfplot,
            as.data.frame(table(df[[column]][idx])) %>%
                dplyr::mutate(Var1 = as.character(Var1)) %>%
                setNames(c(column, "Frequency")) %>%
                dplyr::mutate(group = "Subset")
        )
    }
    dfplot <- dfplot %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(ypos = max(Frequency)/2,
                      RelFrequency = Frequency/sum(Frequency)) %>%
        dplyr::ungroup()
    
    NN <- nrow(df)
    gg <- ggplot2::ggplot(dfplot, ggplot2::aes(x = .data[[column]], y = Frequency,
                             fill = .data[[column]],
                             label = scales::percent(RelFrequency,
                                                     accuracy = 0.1))) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::theme_bw() + 
        ggplot2::facet_wrap(~ group, ncol = 1, scales = "free_y") + 
        ggplot2::theme(
            legend.position = "none",
            axis.title = ggplot2::element_text(size = 14),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.text.x = ggplot2::element_text(size = 12, angle = 90, 
                                                hjust = 1, vjust = 0.5)
        )
    
    if (showPercentages) {
        gg <- gg + 
            ggplot2::geom_text(
                size = fontSizePercentages, color = "black",
                angle = 0, aes(y = ypos))
    }
    
    if (doPlot) {
        print(gg)
    }
    
    invisible(dfplot %>% dplyr::select(-ypos))
}

