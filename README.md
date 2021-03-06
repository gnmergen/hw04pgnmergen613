
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hw04pgnmergen613

<!-- badges: start -->

<!-- badges: end -->

The goal of hw04pgnmergen is to find the nth element of a recursive
sequence, and the sequence of which can be plotted using line graph.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gnmergen/hw04pgnmergen613")
```

## Example 1

Basic example of function myseq\_n:

``` r
library(hw04pgnmergen613)
myseq_n(x = c(2,10,10), n = 5)
#> [1] 8
myseq_n(x = c(3,5,6), n = 10)
#> [1] 5.399355
```

## Example 2

``` r
my_data <- tibble::tribble(
 ~x, ~y, ~z, ~n,
2,4,3,3,
2,4,3,4,
2,4,3,5,
2,4,3,6,
2,4,3,7,
2,4,3,8,
2,4,3,9,
2,4,3,10,
2,4,3,12)

plotter(my_data)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Basic example of function plotter. We create a 4 column dataframe and
utilizing our function myseq\_n the plotter will attempt to create a
line graph of the generated sequency to the nth element.
