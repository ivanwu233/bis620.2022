
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2023

<!-- badges: start -->

[![R-CMD-check](https://github.com/ivanwu233/bis620.2022/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ivanwu233/bis620.2022/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ivanwu233/bis620.2022/branch/master/graph/badge.svg)](https://codecov.io/gh/ivanwu233/bis620.2022)
<!-- badges: end -->

The goal of bis620.2023 is to provide a set of tools and a Shiny
application for analyzing and exploring clinical trials data. This
package is designed for the BIS620 class and offers functionalities for
data processing and visualization, along with an interactive Shiny
application for in-depth data exploration.

## Installation

You can install the development version of bis620.2023 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ivanwu233/bis620.2022")
```

\##Database Setup The package requires an external DuckDB database file
(ctgov.duckdb) to function properly. You need to download this file and
place it in a known directory. The package will attempt to locate this
file in the parent directory of your current working directory. If it is
located elsewhere, you will need to specify its path when launching the
Shiny application.

\##Launching the Shiny Application To launch the interactive Shiny
application:
