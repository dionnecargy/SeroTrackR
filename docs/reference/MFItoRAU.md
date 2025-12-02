# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion

This function fits a 5-parameter logistic standard curve to the
dilutions of the positive controls for each protein and converts the MFI
values into relative antibody units (RAU) written by Connie Li Wai Suen.

## Usage

``` r
MFItoRAU(sero_data, plate_list, counts_QC_output)
```

## Arguments

- sero_data:

  Output from \`readSeroData()\` (reactive).

- plate_list:

  Output from \`readPlateLayout()\` (reactive).

- counts_QC_output:

  Output from \`getCountsQC()\` (reactive).

## Value

A list of three data frames: 1. Data frame with MFI data, converted RAU
data and matched SampleID's. 2. Plot information for \`plotModel\`
function 3. Data frame of RAU data for random forest classification use.

## Author

Connie Li Wai Suen, Dionne Argyropoulos

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
#> Registered S3 methods overwritten by 'meltr':
#>   method           from 
#>   print.date_names readr
#>   print.locale     readr
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

# Step 3: Convert MFI to RAU using PNG beads
mfi_to_rau <- MFItoRAU(
  sero_data = sero_data,
  plate_list = plate_list,
  counts_QC_output = counts_qc
)

















# }
```
