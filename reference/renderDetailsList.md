# Create a Fluent UI Table

This function makes the table in a Fluent UI format.

## Usage

``` r
renderDetailsList(df)
```

## Arguments

- df:

  Any processed data frame

## Value

A table in the Fluent UI format

## Author

Dionne Argyropoulos

## Examples

``` r
# Minimal example using a small data frame.
# This example is safe for CRAN because it runs only if
# shiny.fluent and htmltools are installed.

if (requireNamespace("shiny.fluent", quietly = TRUE) &&
    requireNamespace("htmltools", quietly = TRUE)) {

  # Tiny example data frame
  example_df <- data.frame(
    Sample = c("A", "B"),
    Value = c(10, 20),
    stringsAsFactors = FALSE
  )

  # Render Fluent UI DetailsList
  renderDetailsList(example_df)
}
```
