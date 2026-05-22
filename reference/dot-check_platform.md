# Check Platform

This function checks the platform the user has input and whether it
aligns with the correct format as expected. Will report error if NOT
aligned.

## Usage

``` r
.check_platform(raw_data, platform, file_name)
```

## Arguments

- raw_data:

  String with the raw data path.

- platform:

  "magpix", "bioplex" or "intelliflex".

- file_name:

  String with the raw data filename (for error messaging).

## Value

TRUE: if platform == file format, ERROR message when platform does not
equal file format.

## Author

Dionne Argyropoulos

## Examples

``` r
your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
.check_platform(raw_data = your_raw_data, platform = "magpix", file_name = basename(your_raw_data))
#> [1] TRUE
```
