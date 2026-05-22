# Random Forest Classification

This function classifies unknown samples as recently exposed or not
(Note: MFItoRAU() or MFItoRAU_Adj() needs to be run first to convert to
RAU).

## Usage

``` r
classifyResults(
  mfi_to_rau_output,
  algorithm_type = "antibody_model",
  sens_spec = "balanced",
  qc_results,
  project = NULL
)
```

## Arguments

- mfi_to_rau_output:

  Output from \`MFItoRAU()\` or \`MFItoRAU_Adj()\`.

- algorithm_type:

  Algorithm: "antibody_model" (PvSEM algorithm; default)

- sens_spec:

  User-selected Sensitivity/Specificity threshold: "balanced" (default)
  or "90% specificity".

- qc_results:

  Output from \`runQC()\`.

- project:

  Default = NULL. Only write "pkpfpv" if using Pk/Pf/Pv pipeline.

## Value

\- Data frame with exposure status for every sample. - Summary table
with positive/negative results for each threshold.

## Author

Lauren Smith, Dionne Argyropoulos

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

# Step 3: Convert MFI to RAU using ETH beads
mfi_to_rau <- MFItoRAU_Adj(
  sero_data   = sero_data,
  plate_list  = plate_list,
  qc_results  = qc_results
)
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`

# Step 4: Perform Pv classification
pv_classified <- classifyResults(
  mfi_to_rau_output = mfi_to_rau,
  algorithm_type    = "antibody_model",
  sens_spec         = "balanced",
  qc_results        = qc_results
)
# }
```
