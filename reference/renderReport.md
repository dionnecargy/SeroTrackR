# Render Markdown report

A short function to render the rmarkdown report on Shiny.

## Usage

``` r
renderReport(input, output, params)
```

## Arguments

- input:

  Input files

- output:

  Output files

- params:

  Parameters to generate outputs.

## Value

PDF output.

## Author

Dionne Argyropoulos

## Examples

``` r
# Minimal example that renders a temporary Rmd file.
# Safe for CRAN because it only writes to tempdir()
if (FALSE) { # \dontrun{
if (requireNamespace("rmarkdown", quietly = TRUE) &&
    rmarkdown::pandoc_available()) {

  # Create a temporary Rmd that declares params in the YAML
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: \"Test Report\"",
    "output: html_document",
    "params:",
    "  value: 0",
    "---",
    "",
    "This is a test report.",
    "",
    "Parameter value: `r params$value`"
  ), con = rmd_file)

  # Output location
  out_file <- tempfile(fileext = ".html")

  # Example parameters to pass in
  example_params <- list(value = 123)

  # Render report
  renderReport(
    input  = rmd_file,
    output = out_file,
    params = example_params
  )

  # Optionally inspect the output path
  out_file
}
} # }
```
