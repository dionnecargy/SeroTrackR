# Generate QC PDF Report

Generate QC PDF Report

## Usage

``` r
renderQCReport(
  raw_data,
  plate_layout,
  platform,
  experiment_name = "experiment1",
  date = format(Sys.Date(), "%Y%m%d"),
  experiment_notes = "no notes",
  location,
  std_point = 10,
  path = "."
)
```

## Arguments

- raw_data:

  A string with the raw data path.

- plate_layout:

  A string with the plate layout path.

- platform:

  A string: "magpix", "intelliflex", or "bioplex".

- experiment_name:

  A string for experiment name.

- date:

  A string or Date. Defaults to today's date.

- experiment_notes:

  A string of notes. Default is "no notes".

- location:

  A string for experiment location: "ETH" or "PNG" accepted.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = 10. Value is an integer.

- path:

  Output path for the PDF file. Defaults to current working directory.

## Value

Rendered PDF report.

## Author

Dionne Argyropoulos

## Examples

``` r
## Not run on CRAN because it requires interactive rendering and can be slow:
if (interactive()) {

  # Example raw data files (MAGPIX platform)
  your_raw_data <- c(
    system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
    system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
    system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
  )

  # Example plate layout file
  your_plate_layout <- system.file(
    "extdata",
    "example_platelayout_1.xlsx",
    package = "SeroTrackR"
  )

  # Generate the QC PDF report
  renderQCReport(
    raw_data     = your_raw_data,
    plate_layout = your_plate_layout,
    platform     = "magpix",
    location     = "ETH"
  )
}
```
