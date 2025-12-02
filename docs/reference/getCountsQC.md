# Get All Counts Data

This function obtains the count data from the raw Median Fluorescent
Intensity (MFI). This function relies on the output of the
Antigen-specific counts (\`getAntigenCounts\`) and the Well or
Sample-specific counts (\`getCounts\`).

## Usage

``` r
getCountsQC(antigen_counts_output, counts_output)
```

## Arguments

- antigen_counts_output:

  Output from \`getAntigenCounts\` (reactive).

- counts_output:

  Output from \`getCounts\` (reactive).

## Value

Joined data frame for all count data.

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

# Step 2: Process counts and perform quality control
counts      <- processCounts(sero_data)
counts_raw  <- getCounts(counts)
sample_ids  <- getSampleID(counts, plate_list)
antigen_cts <- getAntigenCounts(counts, plate_list)
counts_qc   <- getCountsQC(antigen_cts, counts_raw)

# }
```
