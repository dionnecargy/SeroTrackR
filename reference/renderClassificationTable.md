# All Classification Data

This function runs the classification algorithm for all possible
sensitivity and specificity options.

## Usage

``` r
renderClassificationTable(mfi_to_rau_output, algorithm_type, qc_results)
```

## Arguments

- mfi_to_rau_output:

  Output from \`MFItoRAU()\` or \`MFItoRAU_Adj()\`.

- algorithm_type:

  Algorithm: "antibody_model" (PvSEM algorithm; default)

- qc_results:

  Output from \`runQC()\`.

## Value

A table of all classification outputs.

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{

# Step 0: Load in Raw Data
your_raw_data <- c(
  system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
)
your_plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")

# Step 1: Reading in Raw Data
sero_data     <- readSeroData(raw_data = your_raw_data, "magpix")
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
plate_list    <- readPlateLayout(
  plate_layout = your_plate_layout,
  sero_data = sero_data
)
#> Plate layouts correctly identified!

# Step 2: Quality Control and MFI to RAU
qc_results <- runQC(sero_data, plate_list)

# Step 4: Run MFI to RAU (e.g., using ETH beads)
mfi_to_rau_output  <- MFItoRAU_Adj(sero_data, plate_list, qc_results)
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`
#> Joining with `by = join_by(antigen)`

# Step 5: Render classification table
renderClassificationTable(
  mfi_to_rau_output = mfi_to_rau_output,
  algorithm_type = "antibody_model",
  qc_results = qc_results
)
#> # A tibble: 2 × 3
#> # Groups:   Sensitivity/Specificity [2]
#>   `Sensitivity/Specificity` Seropositive Seronegative
#>   <chr>                            <int>        <int>
#> 1 90% specificity                     92           76
#> 2 balanced                           150           18

# }
```
