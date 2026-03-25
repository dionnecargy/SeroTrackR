# Plot Raw Median Fluorescent Intensity of Standard Curve Data

This function gets the standards data and plots the standard curves.

## Usage

``` r
plotStds(sero_data, std_point = 10, location, experiment_name)
```

## Arguments

- sero_data:

  Output from \`readSeroData()\`.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Default
  = 10. Value is an integer.

- location:

  "PNG" or "ETH" to filter WEHI standard curve data.

- experiment_name:

  User-input experiment name.

## Value

\- Dot and line plot of standard curves (S1-S10) with PNG or Ethiopia
stds underneath (ggplot). - WEHI-acceptable standard curve data on
background of plot with user data.

## Author

Dionne Argyropoulos, Shazia Ruybal-Pesantez

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

# Plot Standards
plotStds(
  sero_data = sero_data,
  location = "ETH",
  experiment_name = "experiment1"
)
#> Warning: Ignoring unknown aesthetics: text


# }
```
