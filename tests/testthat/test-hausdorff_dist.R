test_that("Hausdorff distance calculation works", {
    m1 <- matrix(1:15, nrow = 5)
    m2 <- matrix(rnorm(12), nrow = 4)
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
    m1 <- matrix(rnorm(150), nrow = 15)
    expect_error(hausdorffDistPlot(mat = "error"), "of class 'matrix'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = "error"), "of class 'numeric'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = "error"),
                 "of class 'numeric'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5, q = "error"),
                 "of class 'numeric'")
    expect_error(hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 5, doPlot = "error"),
                 "of class 'logical'")
    
    df <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2)
    expect_s3_class(df, "data.frame")
    expect_named(df, c("N", "frac", "HausdorffDist"))
    expect_equal(nrow(df), 4)
})
