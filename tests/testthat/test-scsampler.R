set.seed(1)
xd <- matrix(stats::rnorm(5000), nrow = 1000)
xs <- methods::as(xd, "dgCMatrix")
n <- 10

test_that("scsampler works", {
    skip_on_os(os = "mac", arch = "aarch64")

    ## ------------------------------------------------------------------------- ##
    ## Mis-specified arguments
    ## ------------------------------------------------------------------------- ##
    ## scsampler
    expect_error(scsampler(mat = "error"), "of class 'matrix'")
    expect_error(scsampler(mat = xd), '"N" is missing')
    expect_error(scsampler(N = n), '"mat" is missing')
    expect_error(scsampler(mat = xd, N = "error"), "class 'numeric'")
    expect_error(scsampler(mat = xd, N = n, random_split = "error"), "class 'numeric'")
    expect_error(scsampler(mat = xd, N = n, seed = "error"), "class 'numeric'")

    ## ------------------------------------------------------------------------- ##
    ## Checks, scsampler
    ## ------------------------------------------------------------------------- ##
    id1 <- scsampler(mat = xd, N = n, seed = 42)
    id2 <- scsampler(mat = xd, N = n, seed = 44)
    id3 <- scsampler(mat = xd, N = 2 * n, seed = 42)
    id4 <- scsampler(mat = xd, N = 2 * n, seed = 44)
    id5 <- scsampler(mat = xd, N = n, seed = 99)
    id6 <- scsampler(mat = xd, N = n, seed = 99)
    is1 <- scsampler(mat = xs, N = n, seed = 42)
    is2 <- scsampler(mat = xs, N = n, seed = 44)

    expect_type(id1, "double")
    expect_type(id2, "double")
    expect_type(id3, "double")
    expect_type(id4, "double")
    expect_type(id5, "double")
    expect_type(id6, "double")
    expect_type(is1, "double")
    expect_type(is2, "double")

    expect_length(id1, n)
    expect_length(id2, n)
    expect_length(id3, 2 * n)
    expect_length(id4, 2 * n)
    expect_length(id5, n)
    expect_length(id6, n)
    expect_length(is1, n)
    expect_length(is2, n)

    expect_equal(id1, c(480, 726, 694, 295, 204, 697, 460, 143, 492, 938))
    expect_equal(id2, c(656, 615, 915, 14, 800, 442, 633, 841, 262, 527))
    expect_equal(id3, c(480, 726, 694, 295, 204, 697, 460, 143, 492, 938,
                        219, 442, 277, 274, 265, 495, 966, 165, 636, 615))
    expect_equal(id4, c(656, 615, 915, 14, 800, 442, 633, 841, 262, 527,
                        713, 966, 651, 165, 771, 495, 606, 387, 246, 4))
    expect_equal(id5, c(348, 295, 966, 492, 345, 274, 651, 694, 533, 726))

    expect_false(all(id1 == id2))

    expect_identical(id1, is1)
    expect_identical(id2, is2)
    expect_identical(id1, id3[seq_len(n)])
    expect_identical(id2, id4[seq_len(n)])
    expect_identical(id5, id6)
})

test_that("getScSamplerNames works", {
    skip_on_os(os = "mac", arch = "aarch64")

    ## ------------------------------------------------------------------------- ##
    ## Checks, getScSamplerNames
    ## ------------------------------------------------------------------------- ##
    nms <- getScSamplerNames()

    expect_type(nms, "character")
    expect_true("scsampler" %in% nms)
})

test_that("hausdorffDistPlot works also with scsampler", {
    skip_on_os(os = "mac", arch = "aarch64")

    m1 <- matrix(stats::rnorm(150), nrow = 15)

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

    set.seed(1)
    df4 <- hausdorffDistPlot(mat = m1, Nvec = c(5, 10), Nrep = 2,
                             extraArgs = list(geosketch = list(seed = 123)))
    expect_s3_class(df4, "ggplot")
})

