# Helper function to set up MFI to RAU function

Helper function to set up MFI to RAU function

## Usage

``` r
.setup_mfitorau_inputs(df, plate_list, std_point)
```

## Arguments

- df:

  Output from \`readSeroData()\`.

- plate_list:

  Output from \`readPlateLayout()\`.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = 10. Value is an integer.

## Value

A list of processed sero_data, processed plate layout, antigen names,
and parameters for standard curve.

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{

# Step 0: Load example raw data
your_raw_data <- c(
  system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
)
your_plate_layout <- system.file(
  "extdata",
  "example_platelayout_1.xlsx",
  package = "SeroTrackR"
)

# Step 1: Read serology data and plate layout
sero_data  <- readSeroData(your_raw_data,"magpix")
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
plate_list <- readPlateLayout(your_plate_layout, sero_data)
#> Plate layouts correctly identified!

# Step 2: Setup MFI to RAU
setup <- .setup_mfitorau_inputs(
  df = sero_data$results,
  plate_list = plate_list,
  std_point = 10
)

# }
```
