test_that("compareCompositionPlot works", {
    set.seed(123)
    df <- data.frame(celltype = sample(LETTERS[1:3], 100, replace = TRUE,
                                       prob = c(0.1, 0.3, 0.6)))
    idx <- 1:20
    expect_error(compareCompositionPlot(df = "error", idx = idx,
                                        column = "celltype"),
                 "subscript out of bounds")
    expect_error(compareCompositionPlot(df = df, idx = "error",
                                        column = "celltype"),
                 "of class 'numeric'")
    expect_error(compareCompositionPlot(df = df, idx = list("one" = "error"),
                                        column = "celltype"),
                 "of class 'numeric'")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = 1),
                 "of class 'character'")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "missing"),
                 "must be one of")
    expect_error(compareCompositionPlot(df = df, idx = list(idx, idx),
                                        column = "celltype"),
                 "must not be NULL")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "celltype",
                                        showPercentages = "error"),
                 "of class 'logical'")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "celltype",
                                        showPercentages = c(TRUE, FALSE)),
                 "length 1")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "celltype",
                                        fontSizePercentages = "error"),
                 "of class 'numeric'")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "celltype",
                                        fontSizePercentages = c(4, 5)),
                 "length 1")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "celltype",
                                        doPlot = "error"),
                 "of class 'logical'")
    expect_error(compareCompositionPlot(df = df, idx = idx, column = "celltype",
                                        doPlot = c(TRUE, FALSE)),
                 "length 1")

    ccp <- compareCompositionPlot(df = df, idx = idx, column = "celltype")
    expect_equal(ccp$Frequency[ccp$celltype == "A" & ccp$group == "Full dataset"],
                 sum(df$celltype == "A"))
    expect_equal(ccp$Frequency[ccp$celltype == "B" & ccp$group == "Full dataset"],
                 sum(df$celltype == "B"))
    expect_equal(ccp$Frequency[ccp$celltype == "C" & ccp$group == "Full dataset"],
                 sum(df$celltype == "C"))
    expect_equal(ccp$Frequency[ccp$celltype == "A" & ccp$group == "Subset"],
                 sum(df$celltype[idx] == "A"))
    expect_equal(ccp$Frequency[ccp$celltype == "B" & ccp$group == "Subset"],
                 sum(df$celltype[idx] == "B"))
    expect_equal(ccp$Frequency[ccp$celltype == "C" & ccp$group == "Subset"],
                 sum(df$celltype[idx] == "C"))

    expect_equal(ccp$RelFrequency[ccp$celltype == "A" & ccp$group == "Full dataset"],
                 sum(df$celltype == "A")/nrow(df))
    expect_equal(ccp$RelFrequency[ccp$celltype == "A" & ccp$group == "Subset"],
                 sum(df$celltype[idx] == "A")/length(idx))

    ccpl <- compareCompositionPlot(df = df,
                                   idx = list(first = idx, second = idx),
                                   column = "celltype")
    expect_equal(ccpl$Frequency[ccpl$celltype == "A" & ccpl$group == "first"],
                 ccpl$Frequency[ccpl$celltype == "A" & ccpl$group == "second"])
    expect_equal(ccpl$Frequency[ccpl$celltype == "A" & ccpl$group == "first"],
                 ccp$Frequency[ccp$celltype == "A" & ccp$group == "Subset"])
})
