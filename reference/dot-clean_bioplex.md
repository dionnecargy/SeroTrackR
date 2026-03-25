# Helper function to process bioplex data

Helper function to process bioplex data

## Usage

``` r
.clean_bioplex(df)
```

## Arguments

- df:

  Output from \`.read_luminex.file()\`

## Value

Cleaned data fame

## Author

Dionne Argyropoulos

## Examples

``` r
your_raw_data <- system.file("extdata", "example_BioPlex_plate1.xlsx", package = "SeroTrackR")
df            <- .read_luminex_file(your_raw_data)
results       <- .clean_bioplex(df)
```
