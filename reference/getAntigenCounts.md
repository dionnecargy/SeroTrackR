# Get Count Data for each Antigen from the Raw Median Fluorescent Intensity

This function obtains the count data from the raw Median Fluorescent
Intensity (MFI). This function relies on the \`readAntigens\` and
\`readSeroData\` data processing functions.

## Usage

``` r
getAntigenCounts(processed_counts, plate_list)
```

## Arguments

- processed_counts:

  Output from \`processCounts()\`.

- plate_list:

  Output from \`readPlateLayout()\`.

## Value

\(i\) Data frame providing bead counts per antigen per well per plate.
(ii) Designates whether wells should be repeated if there are \\\<=\\ 15
beads (repeat) or if they are sufficient with \> 15 beads (sufficient
beads).

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

# Get Antigen Counts:
antigen_cts <- getAntigenCounts(counts, plate_list)
# }
```
