
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rundsoc

<!-- badges: start -->
<!-- badges: end -->

rundsoc is an R package which provides helper functions for working with
[Understanding Society](https://www.understandingsociety.ac.uk/) data.

## Installation

You can install rundsoc from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ch-williamson/rundsoc")
```

## Example

rundsoc currently contains the following functions:

-   **get_ind** - Get individual response data for a particular wave and
    set of variables
-   **get_ind_multiwave** - Get individual response data for multiple
    waves
-   **get_hh** - Get household data for a single wave
-   **make_na** - Replace special “missing” values with NA
