
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fqacalc

## A Floristic Quality Assessment Calculator for R

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for calculating Floristic Quality
Assessment (FQA) metrics based on 52 regional FQA databases that have
been approved for use by the US Army Corps of Engineers.

## Installation

You can install the development version of `fqacalc` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ifoxfoot/fqacalc")
```

``` r
#attach packages required for this tutorial
library(fqacalc)
library(stringr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Package Data

`fqacalc` contains all 52 regional FQA databases that have been either
fully approved for use or approved with reservations by the US Army
Corps of Engineers. By referencing these databases, the package knows
what Coefficient of Conservatism (or C Value) to give each plant that
the user inputs. Users can see a list of regional databases using the
`db_names()` function, and specific FQA databases can be accessed using
the `view_db()` function.

``` r
#view a list of all 51 available databases
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
Michigan, downloaded from the [Universal FQA
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

`fqacalc` contains two functions that help the user understand how the
data they input relates to the regional database: `accepted_entries()`
and `unassigned_plants()`. `accepted_entries()` is a function that shows
which observations in the input data frame were successfully matched to
a regional database. To demonstrate how these data viewing functions
work I’m going to add a mistake to the `crooked_island` data set.

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
#> 2             AMMOPHILA BREVILIGULATA    <NA>       Poaceae  AMMBRE native 10
#> 3 ANTICLEA ELEGANS; ZIGADENUS GLAUCUS    <NA> Melanthiaceae  ANTELE native 10
#> 4             ARCTOSTAPHYLOS UVA-URSI    <NA>     Ericaceae  ARCUVA native  8
#> 5                ARTEMISIA CAMPESTRIS    <NA>    Asteraceae  ARTCAM native  5
#> 6              CALAMAGROSTIS EPIGEIOS    <NA>       Poaceae  CALEPI exotic  0
#> 7              CALAMOVILFA LONGIFOLIA    <NA>       Poaceae  CALLON native 10
#>    w physiognomy  duration     common_name        fqa_db
#> 2  5       grass perennial    marram grass michigan_2014
#> 3 -3        forb perennial     white camas michigan_2014
#> 4  5       shrub perennial       bearberry michigan_2014
#> 5  5        forb  biennial        wormwood michigan_2014
#> 6  3       grass perennial       reedgrass michigan_2014
#> 7  5       grass perennial sand reed grass michigan_2014
```

Now, when we use `accepted_entries()` to see which species were matched
to the regional dataset, we can see that we got a message about the
species ‘typo’ being discarded and we can also see that the accepted
entries dataset we created only has 34 entries.

In some cases, plants from the user list can be matched to the regional
database, but the plant is not associated with any C Value. This is
usually because not enough is known about that plant. Plants that are
matched but have no C Value will be excluded from FQI metric calculation
but they can *optionally* be included in other metrics like species
richness, relative cover, relative frequency, relative importance, and
mean wetness, as well as any summarizing functions containing these
metrics. This option is denoted with the `allow_no_c` argument.

`unassigned_plants()` is a function that shows the user which plants
have not been assigned a C Value.

``` r
#To see unassigned_plants in action we're going to Montana! 

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

As you can see, two of these species have no C Values.

## Unweighted FQI Metrics

`fqacalc` also contains a variety of functions that calculate Total
Species Richness, Native Species Richness, Mean C, Native Mean C, Total
FQI, Native FQI, and Adjusted FQI. All of these functions eliminate
duplicate species, species that cannot be found in the regional
database, and species without a C Value.

#### Function Arguments

All of the metric functions have the same arguments. (and two don’t have
the native argument)

-   **x**: A data frame containing a list of plant species. This data
    frame *must* have one of the following columns: `scientific_name` or
    `acronym`.

-   **key**: A character string representing the column that will be
    used to join the input `x` with the regional FQA database. If a
    value is not specified the default is `"scientific_name"`.
    `"scientific_name"` and `"acronym"` are the only acceptable values
    for key.

-   **db**: A character string representing the regional FQA database to
    use. See `db_names()` for a list of potential values.

-   **native**: native Boolean (TRUE or FALSE). If TRUE, calculate
    metrics using only native species.

Additionally, `species_richness()` and `all_metrics()` have an argument
called `allow_no_c`. If `allow_no_c = TRUE` than species that are in the
regional FQAI but don’t have C Values will be included. If `allow_no_c`
is FALSE, then these species will be omitted. This argument is also
found in `mean_w()` and all the relative functions.

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
#>                           metrics     values
#> 1          Total Species Richness 35.0000000
#> 2         Native Species Richness 28.0000000
#> 3     Non-native Species Richness  7.0000000
#> 4    % of Species with no C Value  0.0000000
#> 5     % of Species with 0 C Value 20.0000000
#> 6   % of Species with 1-3 C Value  8.5714286
#> 7   % of Species with 4-6 C Value 34.2857143
#> 8  % of Species with 7-10 C Value 37.1428571
#> 9                          Mean C  5.3714286
#> 10                  Native Mean C  6.7142857
#> 11                      Total FQI 31.7778000
#> 12                     Native FQI 35.5286605
#> 13                   Adjusted FQI 60.0543971
#> 14                   Mean Wetness  0.7142857
#> 15            Native Mean Wetness  0.8571429
```

Also, all the functions are documented with help pages.

``` r
?all_metrics
```

## Cover-Weighted Functions

Cover-Weighted Functions calculate the same metrics but they are
weighted by how abundant each species is. Therefore, the input data
frame must also have a column named `cover` containing cover values.
Cover values can be continuous (i.e. percent cover) or classed
(i.e. using the Braun-Blanquet method).

Cover-Weighted Functions come in two flavors: those that allow duplicate
entries and those that don’t. Not allowing duplicate species
observations works best for calculating plot-level metrics, where each
species is counted once along with its total cover value. Allowing
duplicates work best for transect-level metrics, where repeated plots
along a transect may contain the same species. As a note, if
`allow_duplicates = FALSE` in a cover-weighted function, any duplicates
species will be counted once and their cover values will be added
together.

Even if you allow duplicate species, if you indicate a plot id column
using the `plot_id` argument, duplicate species in the same plots will
be counted once and their cover values will be added together.

#### Function Arguments

Cover-Weighted Functions have a few additional arguments:

-   **cover_metric**: A character string representing the cover method
    used. Acceptable cover methods are: `"percent_cover"`,
    `"carolina_veg_survey"`, `"braun-blanquet"`, `"doubinmire"`, and
    `"usfs_ecodata"`. `"percent_cover"` is the default and is
    recommended because it is the most accurate.
-   **allow_duplicates**: Boolean (TRUE or FALSE). If TRUE, allow
    duplicate entries of the same species. If FALSE, do not allow
    species duplication. Setting `allow_duplicates` to TRUE is best for
    calculating metrics for multiple plots/quadrants which potentially
    contain the same species. Setting `allow_duplicates` to FALSE is
    best for calculating metrics for a single plot/quadrant, where each
    species is entered once along with its total cover value.

#### Functions

``` r
#first I'll make a hypothetical plot with cover values
plot <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                      scientific_name = c("Abelmoschus esculentus", 
                      "Abies balsamea", "Ammophila breviligulata", 
                      "Anticlea elegans; zigadenus glaucus"),
                      cover = c(50, 4, 20, 30))

#now I'll make up a transect
transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", 
                                    "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
                      cover = c(50, 4, 20, 30, 40, 7, 60),
                      quad_id = c(1, 1, 1, 1, 2, 2, 2))

#cover mean c (no duplicates allowed!)
cover_mean_c(plot, key = "acronym", db = "michigan_2014", 
               native = FALSE, cover_metric = "percent_cover", 
             allow_duplicates = FALSE)
#> [1] 4.923077

#transect mean c (duplicates allowed)
cover_mean_c(transect, key = "acronym", db = "michigan_2014", 
               native = FALSE, cover_metric = "percent_cover",
               allow_duplicates = TRUE)
#> [1] 5.946058

#cover-weighted FQI (again, you can choose to allow duplicates or not)
cover_FQI(transect, key = "acronym", db = "michigan_2014", native = FALSE, 
          cover_metric = "percent_cover",
          allow_duplicates = TRUE)
#> [1] 11.89212

#transect summary function (allows duplicates)
transect_summary(transect, key = "acronym", db = "michigan_2014")
#>                           metrics     values
#> 1          Total Species Richness  4.0000000
#> 2         Native Species Richness  3.0000000
#> 3     Non-native Species Richness  1.0000000
#> 4    % of Species with no C Value  0.0000000
#> 5     % of Species with 0 C Value 25.0000000
#> 6   % of Species with 1-3 C Value 25.0000000
#> 7   % of Species with 4-6 C Value  0.0000000
#> 8  % of Species with 7-10 C Value 50.0000000
#> 9                          Mean C  5.7500000
#> 10                  Native Mean C  7.6666667
#> 11          Cover-Weighted Mean C  5.9460581
#> 12   Cover-Weighted Native Mean C  9.4900662
#> 13                      Total FQI 11.5000000
#> 14                     Native FQI 13.2790562
#> 15             Cover-Weighted FQI 11.8921162
#> 16      Cover-Weighted Native FQI 16.4372769
#> 17                   Adjusted FQI 66.3952810
#> 18                   Mean Wetness  1.7500000
#> 19            Native Mean Wetness  0.6666667
```

There is also a plot summary function that summarizes plots along a
transect. Data is input as a single data frame containing species per
plot. This data frame must also have a column representing the plot the
species was observed in. This column is then passed to an additional
argument

-   **plot_id**: A character string representing the name of the column
    in `x` that indicates which plot the species was observed in.

Because it is sometimes useful to calculate the total amount of bare
ground or un-vegetated water in a plot, the user can also choose to
include bare ground or water. To get this feature to work, the user must
set another argument:

-   **allow_non_veg**: Boolean (TRUE or FALSE). If TRUE, allow input to
    contain un-vegetated ground and un-vegetated water.

If `allow_non_veg` is true, the user can include species “UNVEGETATED
GROUND” or “UNVEGETATED WATER” along with their plant species. They can
also use acronyms “GROUND” or “WATER”.

``` r
#print transect to view structure of data
transect_unveg <- data.frame(acronym  = c("GROUND", "ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
                       cover = c(60, 50, 4, 20, 30, 20, 20, 40, 7, 60),
                       quad_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))

#plot summary of a transect (dplicates are not allowed)
plot_summary(x = transect_unveg, key = "acronym", db = "michigan_2014", 
             cover_metric = "percent_cover", 
             plot_id = "quad_id")
#>   quad_id species_richness native_species_richness mean_wetness   mean_c
#> 1       1                4                       3     1.750000 5.750000
#> 2       2                3                       2     3.333333 4.333333
#>   native_mean_c cover_mean_c       FQI native_FQI cover_FQI native_cover_FQI
#> 1      7.666667     4.923077 11.500000  13.279056  9.846154         16.42241
#> 2      6.500000     5.803738  7.505553   9.192388 10.052370         13.10786
#>   adjusted_FQI ground_cover water_cover
#> 1     66.39528           60          NA
#> 2     53.07228           20          20
```

## Relative Functions

Relative functions calculate relative frequency, coverage, and
importance for each category. There is also a species summary function
that produces a summary of each species’ relative metrics in the data
frame. Relative functions always allow duplicate species observations.
They also allow ground and water to be included.

Relative functions have one additional argument which tells the
functions what to calculate the relative value of:

-   **col** A character string equal to ‘species’, ‘family’, or
    ‘physiog’.

Relative functions do not have a native argument.

``` r
#say I want to calculate the relative value of a tree

#relative frequency
relative_freq(transect, key = "acronym", db = "michigan_2014", 
              col = "physiog")
#>   physiognomy rel_freq
#> 1        forb 42.85714
#> 2       grass 28.57143
#> 3        tree 28.57143

#relative cover
relative_cover(transect, key = "acronym", db = "michigan_2014", 
               col = "family", cover_metric = "percent_cover")
#>          family  rel_cov
#> 1     Malvaceae 42.65403
#> 2 Melanthiaceae 14.21801
#> 3      Pinaceae  5.21327
#> 4       Poaceae 37.91469

#relative importance
relative_importance(transect, key = "acronym", db = "michigan_2014", 
                    col = "species", cover_metric = "percent_cover")
#>                       scientific_name rel_import
#> 1              ABELMOSCHUS ESCULENTUS   35.61273
#> 2                      ABIES BALSAMEA   16.89235
#> 3             AMMOPHILA BREVILIGULATA   33.24306
#> 4 ANTICLEA ELEGANS; ZIGADENUS GLAUCUS   14.25186

#species summary
species_summary(transect, key = "acronym", db = "michigan_2014", 
                cover_metric = "percent_cover")
#>                       scientific_name acronym native  c  w frequency coverage
#> 1              ABELMOSCHUS ESCULENTUS  ABEESC exotic  0  5         2       90
#> 2                      ABIES BALSAMEA  ABIBAL native  3  0         2       11
#> 3             AMMOPHILA BREVILIGULATA  AMMBRE native 10  5         2       80
#> 4 ANTICLEA ELEGANS; ZIGADENUS GLAUCUS  ANTELE native 10 -3         1       30
#>   rel_freq  rel_cov rel_import
#> 1 28.57143 42.65403   35.61273
#> 2 28.57143  5.21327   16.89235
#> 3 28.57143 37.91469   33.24306
#> 4 14.28571 14.21801   14.25186

#physiognomy summary
physiog_summary(transect, key = "acronym", db = "michigan_2014", 
                cover_metric = "percent_cover")
#>   physiognomy frequency coverage rel_freq  rel_cov rel_import
#> 1        forb         3      120 42.85714 56.87204   49.86459
#> 2       grass         2       80 28.57143 37.91469   33.24306
#> 3        tree         2       11 28.57143  5.21327   16.89235
```

## Wetness metric

`fqacalc` has one wetness metric, which calculates the mean wetness
coefficient per site. The wetness coefficient is based off of the USFWS
Wetland Indicator Status. Negative wetness coefficients indicate a
stronger affinity for wetlands, while positive wetland coefficients
indicate an affinity for uplands.

``` r
#mean wetness
mean_w(crooked_island, key = "acronym", db = "michigan_2014")
#> [1] 0.7142857
```

## The End
