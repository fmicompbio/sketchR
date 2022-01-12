set.seed(1)
xd <- matrix(stats::rnorm(5000), nrow = 1000)
xs <- methods::as(xd, "dgCMatrix")
n <- 10

## ------------------------------------------------------------------------- ##
## Miss-specified arguments
## ------------------------------------------------------------------------- ##
expect_error(geosketch(mat = "error"), "of class 'matrix'")
expect_error(geosketch(mat = xd), '"N" is missing')
expect_error(geosketch(N = n), '"mat" is missing')
expect_error(geosketch(mat = xd, N = "error"), "class 'integer'")
expect_error(geosketch(mat = xd, N = n, replace = "error"), "class 'logical'")
expect_error(geosketch(mat = xd, N = n, k = "error"), "class 'integer'")
expect_error(geosketch(mat = xd, N = n, alpha = "error"), "class 'numeric'")
expect_error(geosketch(mat = xd, N = n, seed = "error"), "class 'integer'")
expect_error(geosketch(mat = xd, N = n, max_iter = "error"), "class 'integer'")
expect_error(geosketch(mat = xd, N = n, one_indexed = "error"), "class 'logical'")
expect_error(geosketch(mat = xd, N = n, verbose = "error"), "class 'logical'")

## ------------------------------------------------------------------------- ##
## Checks, geosketch
## ------------------------------------------------------------------------- ##
id1 <- geosketch(mat = xd, N = n, seed = 42)
id2 <- geosketch(mat = xd, N = n, seed = 44)
id3 <- geosketch(mat = xd, N = 2 * n, seed = 42)
id4 <- geosketch(mat = xd, N = 2 * n, seed = 44)
is1 <- geosketch(mat = xs, N = n, seed = 42)
is2 <- geosketch(mat = xs, N = n, seed = 44)

expect_type(id1, "double")
expect_type(id2, "double")
expect_type(id3, "double")
expect_type(id4, "double")
expect_type(is1, "double")
expect_type(is2, "double")

expect_length(id1, n)
expect_length(id2, n)
expect_length(id3, 2 * n)
expect_length(id4, 2 * n)
expect_length(is1, n)
expect_length(is2, n)

expect_identical(id1, is1)
expect_identical(id2, is2)

expect_identical(id1, c(49, 253, 353, 364, 486, 615, 651, 729, 737, 754))
expect_identical(id2, c(49, 96, 295, 433, 495, 591, 615, 651, 673, 845))
expect_identical(id3, c(39, 147, 220, 248, 253, 312, 466, 492, 503, 578, 593,
                        702, 714, 729, 749, 837, 858, 868, 920, 979))
expect_identical(id4, c(39, 63, 120, 147, 289, 312, 345, 492, 615, 642, 694,
                        714, 737, 754, 832, 841, 920, 933, 936, 979))
