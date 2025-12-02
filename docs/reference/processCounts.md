# Process Counts from Raw Serological Data file

A helper function to process counts data.

## Usage

``` r
processCounts(sero_data)
```

## Arguments

- sero_data:

  Output from \`readSeroData()\` (reactive).

## Value

Returns a long table of counts with "Warning" category (\<15 == 1 and
\\\>=\\ 15 == 0) for downstream wrangling.

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{
# Example demonstrating how to process bead count data.
# These files are included in the SeroTrackR package under inst/extdata.

your_raw_data <- c(
  system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
)

# Read in raw MAGPIX data
sero_data <- readSeroData(
  raw_data = your_raw_data,
  platform = "magpix"
)
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.

# Process counts
processed_master <- processCounts(sero_data = sero_data)

# }
```
