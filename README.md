
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fqacalc

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for calculating total species richness,
native species richness, mean C, native mean C, FQI, native FQI, and
adjusted FQI based on Michigan’s 2014 FQAI database.

## Installation

You can install the development version of fqacalc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ifoxfoot/fqacalc")
```

## Example

``` r
library(fqacalc)

#the package comes with an example site assessment
plants_of_crooked_island <- crooked_island

head(crooked_island)
#>                       scientific_name acronym  common_name
#> 1                      Abies balsamea  ABIBAL   balsam fir
#> 2             Ammophila breviligulata  AMMBRE marram grass
#> 3 Anticlea elegans; zigadenus glaucus  ANTELE  white camas
#> 4             Arctostaphylos uva-ursi  ARCUVA    bearberry
#> 5                Artemisia campestris  ARTCAM     wormwood
#> 6              Calamagrostis epigeios  CALEPI    reedgrass

#here is an example of all metrics wrapped into one
all_metrics(crooked_island)
#>                   metrics    values
#> 1  Total Species Richness 35.000000
#> 2 Native Species Richness 28.000000
#> 3                  Mean C  5.371429
#> 4           Native Mean C  6.714286
#> 5               Total FQI 31.777800
#> 6              Native FQI 35.528660
#> 7            Adjusted FQI 60.054397
```
