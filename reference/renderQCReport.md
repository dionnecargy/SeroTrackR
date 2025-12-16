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
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
#> Plate layouts correctly identified!
#> 
#> 
#> processing file: template.Rmd
#> 1/15                       
#> 2/15 [setup]               
#> 3/15                       
#> 4/15 [standard curves plot]
#> 5/15                       
#> 6/15 [model results plot]  
#> 7/15                       
#> 8/15 [bead counts plot]    
#> 9/15                       
#> 10/15 [check repeats table] 
#> 11/15                       
#> 12/15 [blank samples plot]  
#> 13/15                       
#> 14/15 [plate layouts]       
#> 15/15                       
#> output file: template.knit.md
#> /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64/pandoc +RTS -K512m -RTS template.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output /Users/Dionne/Documents/GitHub/SeroTrackR/experiment1_20251216_ETH_v1.4.0_QCreport.tex --lua-filter /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --highlight-style tango --pdf-engine pdflatex --variable graphics --include-in-header /var/folders/bh/0yzt0_x97vj_zktb_39c1xvh0000gn/T//RtmpxBCYB6/rmarkdown-str2179de9d916.html --variable 'geometry:margin=1in' 
#> 
#> Output created: /Users/Dionne/Documents/GitHub/SeroTrackR/experiment1_20251216_ETH_v1.4.0_QCreport.pdf
```
