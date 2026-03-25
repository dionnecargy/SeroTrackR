# Helper function to process luminex sections

Helper function to process luminex sections

## Usage

``` r
.extract_luminex_sections(df, cfg, plt)
```

## Arguments

- df:

  String with the raw data path.

- cfg:

  Magpix version output of .magpix_version_config().

- plt:

  Platform (magpix, intelliflex)

## Value

List of data_raw, results, counts, blanks, stds, run

## Author

Dionne Argyropoulos

## Examples

``` r
your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
df            <- .read_luminex_file(your_raw_data)
cfg           <- .magpix_version_config("4.2")
section       <- .extract_luminex_sections(df, cfg, "magpix")
```
