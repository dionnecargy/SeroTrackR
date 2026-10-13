# Make Card in Fluent UI

This function imports the makes a card following the Fluent UI format.

## Usage

``` r
makeCard(title, id, content, size = 12, style = "")
```

## Arguments

- title:

  String with the large title that will be printed in the card.

- id:

  Identifying tag for use to link.

- content:

  A list of content to be rendered.

- size:

  A value from 1 to 12 of the width of the screen (default = 12).

- style:

  Value for any css styling.

## Value

A "card" in the Fluent UI format with content.

## Examples

``` r
# Minimal example creating a simple Fluent UI card.
# Safe for CRAN: runs only if shiny.fluent, htmltools, and glue are installed.

if (requireNamespace("shiny.fluent", quietly = TRUE) &&
    requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("glue", quietly = TRUE)) {

  # Simple card content
  card_content <- list(
    htmltools::div("This is some example text inside the card.")
  )

  # Create a Fluent UI card
  makeCard(
    title   = "Example Card",
    id      = "example-card",
    content = card_content,
    size    = 6
  )
}
#> <div id="example-card" class="card ms-depth-8 ms-sm6 ms-xl6" style="">
#>   <div class="react-container" data-react-id="tfztbnsrafhqmlsyefrm">
#>     <script class="react-data" type="application/json">{"type":"element","module":"@fluentui/react","name":"Stack","props":{"type":"object","value":{"tokens":{"type":"raw","value":{"padding":20,"childrenGap":5}},"children":{"type":"array","value":[{"type":"element","module":"@fluentui/react","name":"Text","props":{"type":"raw","value":{"variant":"large","block":true,"children":"Example Card"}}},{"type":"array","value":[{"type":"element","name":"div","props":{"type":"raw","value":{"children":"This is some example text inside the card."}}}]}]}}}}</script>
#>     <script>jsmodule['@/shiny.react'].findAndRenderReactData('tfztbnsrafhqmlsyefrm')</script>
#>   </div>
#> </div>
```
