# fqacalc 1.1.0

## Breaking Changes

`fqadata` was updated to version 1.1.0 in the description file, producing the following changes: 

-   Southeastern regional databases were renamed to more closely match their names in other tools and publications. The changes are as follows:
    -   southeastern_mountains_2014 -\> appalachian_mountains_2013
    -   southeastern_plain_2014 -\> coastal_plain_southeast_2013
    -   southeastern_interior_plateau_2014 -\> interior_plateau_2013
    -   southeastern_piedmont_2014 -\> southeastern_piedmont_2013
    -   southeastern_southern_coastal_plain_2014 -\> southern_coastal_plain_2013

## Misc Changes

- Changed aggregate functions (`all_metrics`, `transect_summary`) to be produce a % hydrophyte score of NA instead of 0 if wetland status is not available for the regional database.

- Fixed a bug where no c scores would result in NA for cover mean c and cover fqi in transect summary function

# fqacalc 1.0.0

Package was submitted to CRAN

# fqacalc 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
