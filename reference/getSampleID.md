# Get SampleID from Plate Layout

A helper function to extract Sample ID based on plate name and row/col

## Usage

``` r
getSampleID(processed_counts, plate_list)
```

## Arguments

- processed_counts:

  Output from \`processCounts()\`.

- plate_list:

  Plate name inside of the plate layout file.

## Value

Returns the corresponding Sample ID for the correct row/column in the
plate layout file. Henceforth "Sample ID" refers to the code in the
plate layout file, while "Sample" is the code in the Luminex file.

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
# }
```
