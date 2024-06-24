
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silp

<!-- badges: start -->
<!-- badges: end -->

The goal of silp is to execute conditional process analysis with SEM
approach

## Installation

You can install the development version of silp like so:

``` r
devtools::install_github("TomBJJJ/silp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("silp")
## basic example code
data = generate_data(100, 0.3, 0.4, c(1,1,1,1), 0.9)
model = "
 fy =~ y1 + y2 + y3 + y4
 fx =~ x1 + x2 + x3 + x4
 fz =~ z1 + z2 + z3 + z4
 fy ~  fx + fz + fx:fz
"
fit = silp(model, data)
refit = resilp(fit)
#>  ■■■                                7% |  ETA: 33s
#>  ■■■■■■                            16% |  ETA: 29s
#>  ■■■■■■■■                          24% |  ETA: 26s
#>  ■■■■■■■■■■■                       33% |  ETA: 23s
#>  ■■■■■■■■■■■■■■                    42% |  ETA: 20s
#>  ■■■■■■■■■■■■■■■■                  51% |  ETA: 17s
#>  ■■■■■■■■■■■■■■■■■■■               59% |  ETA: 14s
#>  ■■■■■■■■■■■■■■■■■■■■■             68% |  ETA: 11s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA:  8s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■■■■       86% |  ETA:  5s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    95% |  ETA:  2s
```

``` r
summary(refit)
#> Length  Class   Mode 
#>      1   Silp     S4
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
