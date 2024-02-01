test_that("Hausdorff distance calculation works", {
    m1 <- matrix(1:15, nrow = 5)
    m2 <- matrix(stats::rnorm(12), nrow = 4)
    all_dists_m1_to_m2 <- sqrt(cbind(diag(m1 %*% t(m1))) %*% rbind(rep(1, 4)) +
        cbind(rep(1, 5)) %*% rbind(diag(m2 %*% t(m2))) - 2 * m1 %*% t(m2))
    min_dists_m1_to_m2 <- apply(all_dists_m1_to_m2, 1, min)
    expect_equal(.calcRobustDirectedHausdorffDist(full = m1, sketch = m2, q = 0),
                 max(min_dists_m1_to_m2))
    expect_equal(.calcRobustDirectedHausdorffDist(full = m1, sketch = m2, q = 0.5),
                 sort(min_dists_m1_to_m2)[3])
    expect_equal(.calcRobustDirectedHausdorffDist(full = m1, sketch = m2, q = 1),
                 min(min_dists_m1_to_m2))
})

test_that("Hausdorff diagnostic plot works", {
    m1 <- matrix(stats::rnorm(150), nrow = 15)
    expect_error(hausdorffDistPlot(mat = "error"), "of class 'matrix'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = "error"),
                 "of class 'numeric'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = "error"),
                 "of class 'numeric'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   q = "error"),
                 "of class 'numeric'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   methods = "error"),
                 "All values in 'methods'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   methods = c("geosketch", "scsampler"),
                                   extraArgs = 1),
                 "of class 'list'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   methods = c("geosketch", "scsampler"),
                                   extraArgs = list(seed = 1)),
                 "All values in 'namesextraArgs'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   methods = c("geosketch", "scsampler"),
                                   extraArgs = list(geosketch = 1)),
                 "of class 'list'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   methods = c("geosketch", "scsampler"),
                                   extraArgs = list(geosketch = c(seed = 1))),
                 "of class 'list'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5,
                                   methods = c("geosketch", "scsampler"),
                                   extraArgs = list(geosketch = list(seed = 1),
                                                    scsampler = c(seed = 1))),
                 "of class 'list'")

    set.seed(1)
    df1 <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2)
    expect_s3_class(df1, "ggplot")
    df1 <- df1$data
    expect_s3_class(df1, "data.frame")
    expect_named(df1, c("method", "frac", "mean", "se", "low", "high"))
    expect_equal(nrow(df1), 6)

    set.seed(1)
    df2 <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2)
    expect_s3_class(df2, "ggplot")
    df2 <- df2$data
    expect_equal(df1, df2)

    ## Check that we get reproducible results also if we only use a
    ## subset of the methods
    set.seed(1)
    df2b <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2,
                              methods = c("geosketch", "uniform"))
    expect_s3_class(df2b, "ggplot")
    df2b <- df2b$data
    expect_equal(df1[df1$method %in% c("geosketch", "uniform"), ], df2b,
                 ignore_attr = TRUE)

    set.seed(42)
    df3 <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2)
    expect_s3_class(df3, "ggplot")
    df3 <- df3$data
    expect_equal(df1$frac, df3$frac)
    expect_equal(df1$method, df3$method)
    expect_false(all(df1$mean == df3$mean))
    expect_false(all(df1$se == df3$se))

    ## Ignore any seed argument provided to the individual methods
    set.seed(1)
    df4 <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2,
                             extraArgs = list(geosketch = list(seed = 123),
                                              scsampler = list(seed = 456)))
    expect_s3_class(df4, "ggplot")
    df4 <- df4$data
    expect_equal(df1, df4)
})
