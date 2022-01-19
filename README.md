
# sketchR

<!-- badges: start -->
[![R-CMD-check](https://github.com/csoneson/sketchR/workflows/R-CMD-check/badge.svg)](https://github.com/csoneson/sketchR/actions)
<!-- badges: end -->

`sketchR` provides a simple interface to the [`geosketch`](https://github.com/brianhie/geosketch) and 
[`scSampler`](https://github.com/SONGDONGYUAN1994/scsampler)
python packages, which implement subsampling algorithms described in 
[Hie et al (2019)](https://www.cell.com/cell-systems/fulltext/S2405-4712(19)30152-8) 
and [Song et al (2022)](https://www.biorxiv.org/content/10.1101/2022.01.15.476407v1),
respectively. The implementation makes use of the
[`basilisk`](https://bioconductor.org/packages/basilisk/) 
package for interaction between R and python.

## Installation

You can install the development version of sketchR from 
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("csoneson/sketchR")
```

## Example

``` r
library(sketchR)

## Create an example data matrix. Rows represent "samples" (the unit of 
## downsampling), columns represent features (e.g., principal components).
mat <- matrix(rnorm(5000), nrow = 500)

## Run geosketch. The output is a vector of indices, which you can use 
## to subset the rows of the input matrix.
idx <- geosketch(mat, N = 100)

## Run scSampler. As for geosketch, the output is a vector of indices.
idx2 <- scsampler(mat, N = 100)
```

