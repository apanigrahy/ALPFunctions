
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ALPFunctions

<!-- badges: start -->
<!-- badges: end -->

The goal of ALPFunctions is to …

## Installation

You can install the development version of ALPFunctions from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("apanigrahy/ALPFunctions")
```

## Example

Load the package and import test dataset

``` r
# Load package
library(ALPFunctions)
#> Warning: replacing previous import 'flextable::continuous_summary' by
#> 'gtsummary::continuous_summary' when loading 'ALPFunctions'

# Load test dataset
data(ct_data)
```

Use the gt_flex_able function to produce a summary table for reporting
the data

``` r
gt_flex_table(ct_data,
              group_by = trt_group,
              add_p = TRUE)
```

<img src="man/figures/README-gt_flex_table-1.png" width="100%" />
