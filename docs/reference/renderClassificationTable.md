# All Classification Data

This function runs the classification algorithm for all possible
sensitivity and specificity options.

## Usage

``` r
renderClassificationTable(mfi_to_rau_output, algorithm_type, counts_QC_output)
```

## Arguments

- mfi_to_rau_output:

  Output from \`MFItoRAU()\` or \`MFItoRAU_ETH()\` (reactive).

- algorithm_type:

  User-selected algorithm choice: - "antibody_model" (PvSeroTaT model;
  default), or - "antibody_model_excLF016" (PvSeroTat excluding LF016).

- counts_QC_output:

  Output from \`getCountsQC()\` (reactive).

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
sero_data       <- readSeroData(raw_data = your_raw_data, "magpix")
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
plate_list    <- readPlateLayout(
  plate_layout = your_plate_layout,
  sero_data = sero_data
)
#> Plate layouts correctly identified!

# Step 2: Quality Control and MFI to RAU
processCounts_output      <- processCounts(sero_data)
getCounts_output          <- getCounts(processCounts_output)
sampleid_output           <- getSampleID(processCounts_output, plate_list)
getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)

# Step 4: Run MFI to RAU (e.g., using ETH beads)
mfi_to_rau_output  <- MFItoRAU_ETH(sero_data, plate_list, getCountsQC_output)
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
  counts_QC_output = getCountsQC_output
)
#> # A tibble: 7 × 3
#> # Groups:   Sensitivity/Specificity [7]
#>   `Sensitivity/Specificity` Seropositive Seronegative
#>   <chr>                            <int>        <int>
#> 1 85% sensitivity                    158           10
#> 2 85% specificity                    125           43
#> 3 90% sensitivity                    160            8
#> 4 90% specificity                     92           76
#> 5 95% sensitivity                    160            8
#> 6 95% specificity                     65          103
#> 7 maximised                          150           18

# }
```
