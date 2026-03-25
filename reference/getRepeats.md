# Check Beads to Repeat

This function gets the count data and outputs a table of the isolates to
repeat or a statement to confirm that none need to be repeated.

## Usage

``` r
getRepeats(qc_results, plate_list)
```

## Arguments

- qc_results:

  Output from \`runQC()\`.

- plate_list:

  Output from \`readPlateLayout()\`.

## Value

A data frame with wells to "fail", OR if no "fail" found will return
text "No repeats necessary".

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{
# Step 0: Load example raw data and plate layout
raw_data <- c(
  system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
)
plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")

# Step 1: Read data and plate layout
sero_data   <- readSeroData(raw_data, platform = "magpix")
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
plate_list  <- readPlateLayout(plate_layout, sero_data)
#> Plate layouts correctly identified!

# Step 2: Process counts
qc_results  <- runQC(sero_data, plate_list)

# Step 3: Identify samples to repeat
repeats_table <- getRepeats(
  qc_results = qc_results,
  plate_list = plate_list
)

# View results
repeats_table
#> # A tibble: 2 × 4
#>   Location SampleID Plate  QC   
#>   <chr>    <chr>    <chr>  <chr>
#> 1 A1       Blank1   plate2 fail 
#> 2 A2       Blank2   plate2 fail 
# }
```
