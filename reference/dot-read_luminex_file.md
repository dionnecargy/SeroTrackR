# Helper function to read raw luminex files

Helper function to read raw luminex files

## Usage

``` r
.read_luminex_file(file)
```

## Arguments

- file:

  String with the raw data path.

## Value

raw data frame

## Author

Dionne Argyropoulos

## Examples

``` r
your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
df <- .read_luminex_file(your_raw_data)
```
