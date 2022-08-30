
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fqacalc

## A Florisitic Quality Assessment Calculator for R

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for calculating floristic quality
metrics based on the 47 regional FQA databases which have been approved
for use by the US Army Core of Engineers.

## Installation

You can install the development version of fqacalc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ifoxfoot/fqacalc")
```

## Example

``` r
#load the package
library(fqacalc)

#the package comes with an example site assessment from crooked island, MI
head(crooked_island)
#>                       scientific_name acronym  common_name
#> 1                      Abies balsamea  ABIBAL   balsam fir
#> 2             Ammophila breviligulata  AMMBRE marram grass
#> 3 Anticlea elegans; zigadenus glaucus  ANTELE  white camas
#> 4             Arctostaphylos uva-ursi  ARCUVA    bearberry
#> 5                Artemisia campestris  ARTCAM     wormwood
#> 6              Calamagrostis epigeios  CALEPI    reedgrass

#here is an example of all metrics wrapped into one function
all_metrics(crooked_island, key = "acronym", db = "michigan_2014")
#>                                     metrics      values
#> 1                    Total Species Richness 35.00000000
#> 2                   Native Species Richness 28.00000000
#> 3    Proportion of Species with < 1 C score  0.20000000
#> 4  Proportion of Species with 1-3.9 C score  0.08571429
#> 5  Proportion of Species with 4-6.9 C score  0.34285714
#> 6   Proportion of Species with 7-10 C score  0.37142857
#> 7                                    Mean C  5.37142857
#> 8                             Native Mean C  6.71428571
#> 9                                 Total FQI 31.77779998
#> 10                               Native FQI 35.52866046
#> 11                             Adjusted FQI 60.05439711
```
