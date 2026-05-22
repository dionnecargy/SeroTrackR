# Read Plate Layout/s

This function imports the plate layout. Each sheet of the plate layout
".xlsx" file must contain 13 columns (labelled Plate, 1, 2, 3, 4, 5, 6,
7, 8, 9, 10, 11, 12) (columns A-M) and 9 rows (Plate, A, B, C, D, E, F,
G, H) (rows 1-9). \*Note that the first row/column i.e., the A1 cell in
excel is called "Plate". This function also checks that the plate sheet
labels are consistent with the MAGPIX file input names, as a check prior
to merging downstream.

## Usage

``` r
readPlateLayout(plate_layout, sero_data)
```

## Arguments

- plate_layout:

  An ".xlsx" file with sheets labelled plate1, plate2... etc..

- sero_data:

  Output from \`readSeroData()\`.

## Value

A list of data frames, with each one representing an individual plate.

## Author

Shazia Ruybal-Pesántez, Dionne Argyropoulos

## Examples

``` r

# Example input files
your_raw_data <- c(
  system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
)

your_plate_layout <- system.file(
  "extdata",
  "example_platelayout_1.xlsx",
  package = "SeroTrackR"
)

# Step 1: Read and combine serological data
sero_data <- readSeroData(
  raw_data = your_raw_data,
  platform = "magpix"
)
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
# Step 2: Read plate layout
plate_list <- readPlateLayout(
  plate_layout = your_plate_layout,
  sero_data = sero_data
)
#> Plate layouts correctly identified!
```
