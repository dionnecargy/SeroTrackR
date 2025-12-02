# Processing Serological Data for Pk/Pf/Pv MFI to RAU conversion

This is a pre-requisite function before running the
\`MFItoRAU_Plasmo()\` so that the appropriate MFI to RAU conversions can
be run for the respective antigens.

## Usage

``` r
processPkPfPv(sero_data, plate_list, panel = "panel1")
```

## Arguments

- sero_data:

  Output of \`readSeroData()\`

- plate_list:

  Output of \`readPlateLayout()\`

- panel:

  Panel of Pk/Pf/Pv antigens. Default = "panel1".

## Value

A list of two data frames: 1. Data frame with Pk antigens 2. Data frame
with Pf/Pv antigens

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{
# Example demonstrating multi-plate 5-standard processing workflow.
# These files are included in the SeroTrackR package under inst/extdata.

your_raw_data_5std <- c(
  system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
)

your_plate_layout_5std <- system.file(
  "extdata", "example_platelayout_pk_5std.xlsx",
  package = "SeroTrackR"
)

# Read in raw MAGPIX data
sero_data <- readSeroData(
  raw_data = your_raw_data_5std,
  platform = "magpix"
)
#> PASS: File example_magpix_pk_5std_plate1.csv successfully validated.
#> PASS: File example_magpix_pk_5std_plate2.csv successfully validated.

# Read matching plate layout
plate_list <- readPlateLayout(
  plate_layout = your_plate_layout_5std,
  sero_data = sero_data
)
#> Plate layouts correctly identified!

# Process multi-species panel
processed_master <- processPkPfPv(
  sero_data = sero_data,
  plate_list,
  panel = "panel1"
)
# }
```
