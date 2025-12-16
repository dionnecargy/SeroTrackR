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
# Safe for CRAN because it only writes to tempdir() and runs
# conditionally if rmarkdown is installed.
# \donttest{
if (requireNamespace("rmarkdown", quietly = TRUE)) {

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
#> 
#> 
#> processing file: file21797834cfe7.Rmd
#> 1/1
#> output file: file21797834cfe7.knit.md
#> /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64/pandoc +RTS -K512m -RTS file21797834cfe7.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output /var/folders/bh/0yzt0_x97vj_zktb_39c1xvh0000gn/T//RtmpxBCYB6/file2179533d8458.html --lua-filter /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /var/folders/bh/0yzt0_x97vj_zktb_39c1xvh0000gn/T//RtmpxBCYB6/rmarkdown-str21796bbbd819.html 
#> 
#> Output created: /var/folders/bh/0yzt0_x97vj_zktb_39c1xvh0000gn/T//RtmpxBCYB6/file2179533d8458.html
#> [1] "/var/folders/bh/0yzt0_x97vj_zktb_39c1xvh0000gn/T//RtmpxBCYB6/file2179533d8458.html"
# }
```
