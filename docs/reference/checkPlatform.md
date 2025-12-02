# Check Platform

This function checks the platform the user has input and whether it
aligns with the correct format as expected. Will report error if NOT
aligned.

## Usage

``` r
checkPlatform(raw_data, platform)
```

## Arguments

- raw_data:

  String with the raw data path (reactive).

- platform:

  "magpix" or "bioplex" (reactive).

## Value

TRUE: if platform == file format, ERROR message when platform does not
equal file format.

## Author

Dionne Argyropoulos

## Examples

``` r
your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
checkPlatform(raw_data = your_raw_data, platform = "magpix")
#> [1] TRUE
```
