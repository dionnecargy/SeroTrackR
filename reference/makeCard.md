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

  Value for any css styling (reactive).

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
```
