# Helper function to process luminex (Magpix/Intelliflex) data

Helper function to process luminex (Magpix/Intelliflex) data

## Usage

``` r
.clean_luminex(df, row1, row2)
```

## Arguments

- df:

  Raw luminex file

- row1:

  Leading row to subset

- row2:

  Final row to subset

## Value

Cleaned data fame

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{

your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
df            <- .read_luminex_file(your_raw_data)
cfg           <- .magpix_version_config("4.2")

row1         <- which(df$xPONENT == "Median")
row2         <- which(df$xPONENT == "Net MFI")

results      <- .clean_luminex(df, row1, row2)

# }
```
