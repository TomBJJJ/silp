
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silp

<!-- badges: start -->
<!-- badges: end -->

The goal of silp is to …

## Installation

You can install the development version of silp from
[GitHub](https://github.com/TomBJJJ/silp) with:
devtools::install_github(“TomBJJJ/silp”)

``` r
# install.packages("devtools")
devtools::install_github("TomBJJJ/silp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(silp)
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

```
