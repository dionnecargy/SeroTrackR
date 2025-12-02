# Create two columns in Fluent UI

This function creates two columns in the Fluent UI format.

## Usage

``` r
renderTwoCols(
  first_col,
  second_col,
  first_width = "50%",
  second_width = "50%"
)
```

## Arguments

- first_col:

  A list of content for the first column.

- second_col:

  A list of content for the second column.

- first_width:

  Percent width of the column space (default: 50%).

- second_width:

  Percent width of the column space (default: 50%).

## Value

Fluent UI window with two columns.

## Author

Dionne Argyropoulos

## Examples

``` r
# Minimal example using htmltools elements.
# This example runs without starting a Shiny app and is safe for CRAN.

if (requireNamespace("shiny.fluent", quietly = TRUE) &&
    requireNamespace("htmltools", quietly = TRUE)) {

  # Create simple content for each column
  col1 <- list(htmltools::div("First column content"))
  col2 <- list(htmltools::div("Second column content"))

  # Render two columns with default widths
  renderTwoCols(first_col = col1, second_col = col2)
}
#> <div class="react-container" data-react-id="fadwffbyguhwhzptvtkd">
#>   <script class="react-data" type="application/json">{"type":"element","module":"@fluentui/react","name":"Stack","props":{"type":"object","value":{"horizontal":{"type":"raw","value":true},"tokens":{"type":"raw","value":{"childrenGap":40}},"children":{"type":"array","value":[{"type":"element","name":"div","props":{"type":"object","value":{"tokens":{"type":"raw","value":{"childrenGap":15}},"style":{"type":"raw","value":{"width":"50%"}},"children":{"type":"array","value":[{"type":"element","name":"div","props":{"type":"raw","value":{"children":"First column content"}}}]}}}},{"type":"element","name":"div","props":{"type":"object","value":{"tokens":{"type":"raw","value":{"childrenGap":15}},"style":{"type":"raw","value":{"width":"50%"}},"children":{"type":"array","value":[{"type":"element","name":"div","props":{"type":"raw","value":{"children":"Second column content"}}}]}}}}]}}}}</script>
#>   <script>jsmodule['@/shiny.react'].findAndRenderReactData('fadwffbyguhwhzptvtkd')</script>
#> </div>
```
