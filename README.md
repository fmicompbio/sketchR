
# geosketchR

<!-- badges: start -->
[![R-CMD-check](https://github.com/csoneson/geosketchR/workflows/R-CMD-check/badge.svg)](https://github.com/csoneson/geosketchR/actions)
<!-- badges: end -->

`geosketchR` provides a simple interface to the [`geosketch`](https://github.com/brianhie/geosketch) 
python package, which implements the geometric sketching algorithm described by 
[Hie et al (2019)](https://www.cell.com/cell-systems/fulltext/S2405-4712(19)30152-8). The 
implementation makes use of the [`basilisk`](https://bioconductor.org/packages/basilisk/) 
package for interaction between R and python.

## Installation

You can install the development version of geosketchR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("csoneson/geosketchR")
```

## Example

``` r
library(geosketchR)

## Create an example data matrix. Rows represent "samples" (the unit of 
## downsampling), columns represent features (e.g., principal components).
mat <- matrix(rnorm(5000), nrow = 500)

## Run geosketch. The output is a vector of indexes, which you can use 
## to subset the input matrix.
idx <- geosketch(mat, N = 100)
```

