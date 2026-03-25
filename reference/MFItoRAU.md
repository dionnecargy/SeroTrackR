# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion

This function fits a 5-parameter logistic standard curve to the
dilutions of the positive controls for each protein and converts the MFI
values into relative antibody units (RAU) written by Connie Li Wai Suen.

## Usage

``` r
MFItoRAU(sero_data, plate_list, qc_results, std_point = 10, project = NULL)
```

## Arguments

- sero_data:

  Output from \`readSeroData()\`.

- plate_list:

  Output from \`readPlateLayout()\`.

- qc_results:

  Output from \`runQC()\`.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = 10. Value is an integer.

- project:

  Default = NULL. Only write "pkpfpv" if using Pk/Pf/Pv pipeline.

## Value

A list of three data frames: 1. Data frame with MFI data, converted RAU
data and matched SampleID's. 2. Plot information for \`plotModel\`
function 3. Data frame of RAU data for random forest classification use.

## Author

Dionne Argyropoulos, Connie Li Wai Suen

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

# Step 2: Process counts and perform quality control
qc_results  <- runQC(sero_data, plate_list)

# Step 3: Convert MFI to RAU
mfi_to_rau <- MFItoRAU(
  sero_data = sero_data,
  plate_list = plate_list,
  qc_results = qc_results
)

# }
```
