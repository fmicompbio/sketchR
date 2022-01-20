set.seed(1)
xd <- matrix(stats::rnorm(5000), nrow = 1000)
xs <- methods::as(xd, "dgCMatrix")
n <- 10

test_that("scsampler works", {
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

    expect_identical(id1, is1)
    expect_identical(id2, is2)
    expect_identical(id1, id3[seq_len(n)])
    expect_identical(id5, id6)
})

test_that("getScSamplerNames works", {
    ## ------------------------------------------------------------------------- ##
    ## Checks, getScSamplerNames
    ## ------------------------------------------------------------------------- ##
    nms <- getScSamplerNames()

    expect_type(nms, "character")
    expect_true("scsampler" %in% nms)
})
