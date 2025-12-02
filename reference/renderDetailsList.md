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
#> <div class="ms-Grid-row">
#>   <div class="ms-Grid-col ms-sm12">
#>     <div class="react-container" data-react-id="rkjtrcwcwcmgreyijxvc">
#>       <script class="react-data" type="application/json">{"type":"element","module":"@fluentui/react","name":"Stack","props":{"type":"object","value":{"tokens":{"type":"raw","value":{"childrenGap":10}},"horizontal":{"type":"raw","value":true},"children":{"type":"element","name":"div","props":{"type":"object","value":{"style":{"type":"raw","value":"max-height: 600px; overflow: auto; width: 100%;"},"children":{"type":"element","module":"@fluentui/react","name":"DetailsList","props":{"type":"raw","value":{"items":[{"Sample":"A","Value":10},{"Sample":"B","Value":20}],"columns":[{"fieldName":"Sample","name":"Sample"},{"fieldName":"Value","name":"Value"}],"constrainMode":0,"checkboxVisibility":2,"styles":{"root":{"width":"100%","minWidth":"fit-content","overflowX":"auto"}}}}}}}}}}}</script>
#>       <script>jsmodule['@/shiny.react'].findAndRenderReactData('rkjtrcwcwcmgreyijxvc')</script>
#>     </div>
#>   </div>
#> </div>
```
