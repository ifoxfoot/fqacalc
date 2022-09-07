
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fqacalc

## A Florisitic Quality Assessment Calculator for R

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for calculating floristic quality
metrics based on the 47 regional FQA databases that have been approved
for use by the US Army Core of Engineers.

## Installation

You can install the development version of fqacalc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ifoxfoot/fqacalc")
```

``` r
#attach packages required for this tutorial
library(fqacalc)
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.3.5      ✔ purrr   0.3.4 
#> ✔ tibble  3.1.8      ✔ dplyr   1.0.10
#> ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
#> ✔ readr   2.1.2      ✔ forcats 0.5.1 
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```

## Package Data

`fqacalc` contains all 47 regional FQA databases that have been either
fully approved for use or approved with reservations by the US Army Core
of Engineers. By referencing these databases, the package knows what C
score to give each plant that the user inputs. Users can see a list of
regional databases using the `db_names()` function, and specific FQA
databases can be accessed using the `view_db()` function.

``` r
#view a list of all 47 available databases
head(db_names())
#>                 name                     status
#> 1   connecticut_2013 Approved with reservations
#> 2 massachusetts_2013                   Approved
#> 3      new_york_2013                   Approved
#> 4  rhode_island_2013                   Approved
#> 5       vermont_2013 Approved with reservations
#> 6       florida_2011                   Approved

#store the Connecticut database as an object
connecticut <- view_db("connecticut_2013")

#view it
head(connecticut)
#> # A tibble: 6 × 11
#>   scientific…¹ synonym family acronym native     c     w physi…² durat…³ commo…⁴
#>   <chr>        <chr>   <chr>  <chr>   <chr>  <dbl> <dbl> <chr>   <chr>   <chr>  
#> 1 ABELMOSCHUS… <NA>    <NA>   ABES    <NA>       0    NA <NA>    <NA>    okra   
#> 2 ABIES BALSA… <NA>    <NA>   ABBAB   <NA>       8    NA <NA>    <NA>    balsam…
#> 3 ABUTILON TH… <NA>    <NA>   ABTH    <NA>       0    NA <NA>    <NA>    velvet…
#> 4 ACALYPHA GR… <NA>    <NA>   ACGR2   <NA>      NA    NA <NA>    <NA>    slende…
#> 5 ACALYPHA RH… <NA>    <NA>   ACRH    <NA>       2    NA <NA>    <NA>    common…
#> 6 ACALYPHA VI… <NA>    <NA>   ACVI    <NA>       9    NA <NA>    <NA>    Virgin…
#> # … with 1 more variable: fqa_db <chr>, and abbreviated variable names
#> #   ¹​scientific_name, ²​physiognomy, ³​duration, ⁴​common_name
```

`fqacalc` also comes with a real site assessment from Crooked Island,
Michigan, downloaded from [Universal FQA
Calculator](https://universalfqa.org/).

The data is called `crooked_island` and is used to demonstrate how the
package works.

``` r
#take a look at crooked island's plants
head(crooked_island)
#>                       scientific_name acronym  common_name
#> 1                      Abies balsamea  ABIBAL   balsam fir
#> 2             Ammophila breviligulata  AMMBRE marram grass
#> 3 Anticlea elegans; zigadenus glaucus  ANTELE  white camas
#> 4             Arctostaphylos uva-ursi  ARCUVA    bearberry
#> 5                Artemisia campestris  ARTCAM     wormwood
#> 6              Calamagrostis epigeios  CALEPI    reedgrass

#look at the documentation for the data (bottom right pane of R studio)
?crooked_island

#load dataset into local environment
crooked_island <- crooked_island
```

## Functions to see if your observations will be counted

`fqacalc` also contains two functions that help the user understand how
the data they input relates to the regional database:
`accepted_entries()` and `unassigned_plants()`. `accepted_entries()` is
a function that shows which observations in the input data frame were
successfully matched to a regional database. To demonstrate how these
data viewing functions work I’m going to add a mistake to the
`crooked_island` data set.

``` r
#introduce a typo
mistake_island <- crooked_island %>% 
  mutate(scientific_name = str_replace(
    scientific_name, "Abies balsamea", "Abies blahblah"))

#store accepted entries
accepted_entries <- accepted_entries(#this is the data
                                     mistake_island, 
                                     #'key' to join the data to regional list
                                     key = "scientific_name", 
                                     #this is the regional list
                                     db = "michigan_2014", 
                                     #include native AND non-native entries
                                     native = F) 
#> Species ABIES BLAHBLAH not listed in database. It will be discarded.

#view accepted entries
head(accepted_entries)
#>                       scientific_name synonym        family acronym native  c
#> 1             AMMOPHILA BREVILIGULATA    <NA>       Poaceae  AMMBRE native 10
#> 2 ANTICLEA ELEGANS; ZIGADENUS GLAUCUS    <NA> Melanthiaceae  ANTELE native 10
#> 3             ARCTOSTAPHYLOS UVA-URSI    <NA>     Ericaceae  ARCUVA native  8
#> 4                ARTEMISIA CAMPESTRIS    <NA>    Asteraceae  ARTCAM native  5
#> 5              CALAMAGROSTIS EPIGEIOS    <NA>       Poaceae  CALEPI exotic  0
#> 6              CALAMOVILFA LONGIFOLIA    <NA>       Poaceae  CALLON native 10
#>    w physiognomy  duration     common_name        fqa_db
#> 1  5       grass perennial    marram grass michigan_2014
#> 2 -3        forb perennial     white camas michigan_2014
#> 3  5       shrub perennial       bearberry michigan_2014
#> 4  5        forb  biennial        wormwood michigan_2014
#> 5  3       grass perennial       reedgrass michigan_2014
#> 6  5       grass perennial sand reed grass michigan_2014
```

Now, when we use `accepted_entries()` to see which species were matched
to the regional dataset, we can see that we got a message about the
species typo being discarded and we can also see that the accepted
entries dataset we created only has 34 entries.

In some cases plants from the user list can be matched to the regional
database, but the plant is not associated with any C score! This is
usually because not enough is known about that plant. Plants that are
matched but have no C score will be excluded from FQI metric
calculation. `unassigned_plants()` is a function that shows the user
which plants are dropped because they have not been assigned a C score.

``` r
#To see unassigned_plants in action we're going to montana! 

#first I'll create a df of plants to input
no_c_plants<- data.frame(scientific_name = c("ABRONIA FRAGRANS", 
                                             "ACER GLABRUM", 
                                             "ACER GRANDIDENTATUM", 
                                             "ACER PLATANOIDES"))

#then I'll create a df of unassigned plants
unassigned_plants(no_c_plants, key = "scientific_name", db = "montana_2017")
#>       scientific_name synonym        family acronym               native  c  w
#> 1    ABRONIA FRAGRANS    <NA> Nyctaginaceae    <NA>               native NA NA
#> 2 ACER GRANDIDENTATUM    <NA>     Aceraceae    <NA> Unknown/Undetermined NA NA
#>   physiognomy duration                 common_name       fqa_db
#> 1        <NA>     <NA> Fragrant White Sand-verbena montana_2017
#> 2        <NA>     <NA>              Bigtooth Maple montana_2017
```

As you can see, two of these species have no C scores.

## Unweighted FQI Metrics

`fqacalc` also contains a variety of functions that calculate Total
Species Richness, Native Species Richness, Mean C, Native Mean C, Total
FQI, Native FQI, and Adjusted FQI. All of these functions eliminate
duplicate species, unmatched species, and species without a C score.

#### Function Arguments

All of the metric functions have the same arguments. (two don’t have the
native argument)

-   **x**: A data frame containing a list of plant species. This data
    frame *must* have one of the following columns: `scientific_name` or
    `acronym`. This is something I hope to improve upon.

-   **key**: A character string representing the column that will be
    used to join the input `x` with the regional FQA database. If a
    value is not specified the default is `"acronym"`.
    `"scientific_name"` and `"acronym"` are the only acceptable values
    for key.

-   **db**: A character string representing the regional FQA database to
    use. See `db_names()` for a list of potential values.

-   **native**: native Boolean (TRUE or FALSE). If TRUE, calculate
    metrics using only native species.

#### Functions

``` r
#total mean c
mean_c(crooked_island, key = "acronym", db = "michigan_2014", native = FALSE)
#> [1] 5.371429

#native mean C
mean_c(crooked_island, key = "acronym", db = "michigan_2014", native = TRUE)
#> [1] 6.714286

#total FQI
FQI(crooked_island, key = "acronym", db = "michigan_2014", native = FALSE)
#> [1] 31.7778

#native FQI
FQI(crooked_island, key = "acronym", db = "michigan_2014", native = TRUE)
#> [1] 35.52866

#adjusted FQI (always includes both natives and non-natives)
adjusted_FQI(crooked_island, key = "acronym", db = "michigan_2014")
#> [1] 60.0544
```

And finally, `all_metrics()` prints all the metrics in a data frame
format.

``` r
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

Also, all the functions are documented with help pages.

``` r
?all_metrics
```

## Cover-Weighted Functions

Cover-Weighted Functions calculate the same metrics but they are
weighted by how abundant each species is. Therefore, the input data
frame must have a column named `cover` containing cover values. Cover
values can be continuous (i.e. percent cover) or classed (i.e. using the
braun-blanquet method).

Cover-Weighted Functions come in two flavors: transect functions and
quadrat functions. quadrat functions don’t allow duplicate species
observations but transect functions (which are designed to calculate
metrics for a series of quadrats along a transect) do allow species
duplication.

#### Function Arguments

Cover-Weighted Functions have one additional argument:

-   **cover_metric** A character string representing the cover method
    used. Acceptable cover methods are: `"percent_cover"`,
    `"carolina_veg_survey"`, `"braun-blanquet"`,
    `"modified_braun-blanquet"`, `"plots2_braun-blanquet"`,
    `"doubinmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the
    default and is recommended because it is the most accurate.

#### Functions

``` r
#first I'll make a hypothetical plot with cover values
quadrat <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                      scientific_name = c("Abelmoschus esculentus", 
                      "Abies balsamea", "Ammophila breviligulata", 
                      "Anticlea elegans; zigadenus glaucus"),
                      cover = c(50, 4, 20, 30))

#now I'll make up a transect
transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", 
                                    "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
                      cover = c(50, 4, 20, 30, 40, 7, 60),
                      quad_id = c(1, 1, 1, 1, 2, 2, 2))

#quadrat mean c (no duplicates allowed!)
quadrat_mean_c(quadrat, key = "acronym", db = "michigan_2014", 
               native = F, cover_metric = "percent_cover")
#> [1] 4.923077

#transect mean c (duplicates allowed)
transect_mean_c(transect, key = "acronym", db = "michigan_2014", 
               native = F, cover_metric = "percent_cover")
#> [1] 5.946058

#cover-weighted FQI
cover_FQI(transect, key = "acronym", db = "michigan_2014", native = F, 
          cover_metric = "percent_cover")
#> [1] 11.89212

#note to self: should we be able to control duplicates in this function??

#cover summary function
all_cover_metrics(transect, key = "acronym", db = "michigan_2014")
#>                                     metrics    values
#> 1                    Total Species Richness  4.000000
#> 2                   Native Species Richness  3.000000
#> 3    Proportion of Species with < 1 C score  0.250000
#> 4  Proportion of Species with 1-3.9 C score  0.250000
#> 5  Proportion of Species with 4-6.9 C score  0.000000
#> 6   Proportion of Species with 7-10 C score  0.500000
#> 7                                    Mean C  5.750000
#> 8                             Native Mean C  7.666667
#> 9                     Cover-Weighted Mean C  5.946058
#> 10             Cover-Weighted Native Mean C  9.490066
#> 11                                Total FQI 11.500000
#> 12                               Native FQI 13.279056
#> 13                       Cover-Weighted FQI 11.892116
#> 14                Cover-Weighted Native FQI 16.437277
#> 15                             Adjusted FQI 66.395281
```

## Relative Functions

Relative functions calculate relative frequency, coverage, and
importance for each category.

Relative functions have some additional arguments which tell the
functions what to calculate the relative value of:

-   **species** Optional. A character string equal to the Latin name of
    a species to calculate relative value of that species.

-   **family** Optional. A character string equal to a taxonomic family
    to calculate the relative value of that family.

-   **physiog** Optional. A character string equal to a physiognomic
    state (i.e. tree, shrub) to calculate the relative value of that
    state.

Allthough the arguments are optional, the user has to choose at least
one!

``` r
#say I want to calculate the relative value of a tree

#relative frequency
relative_freq(transect, key = "acronym", db = "michigan_2014", 
              native = FALSE, physiog = "tree")
#> [1] 28.57143

#relative cover
relative_cover(transect, key = "acronym", db = "michigan_2014",
               native = FALSE, physiog = "tree")
#> [1] 5.21327

#relative importance
relative_importance(transect, key = "acronym", db = "michigan_2014", 
                    native = FALSE, physiog = "tree")
#> [1] 16.89235
```
