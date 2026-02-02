#' Make Card in Fluent UI
#'
#' This function imports the makes a card following the Fluent UI format.
#'
#' @param title String with the large title that will be printed in the card.
#' @param id Identifying tag for use to link.
#' @param content A list of content to be rendered.
#' @param size A value from 1 to 12 of the width of the screen (default = 12).
#' @param style Value for any css styling.
#' @return A "card" in the Fluent UI format with content.
#' @export
#' @examples
#' # Minimal example creating a simple Fluent UI card.
#' # Safe for CRAN: runs only if shiny.fluent, htmltools, and glue are installed.
#'
#' if (requireNamespace("shiny.fluent", quietly = TRUE) &&
#'     requireNamespace("htmltools", quietly = TRUE) &&
#'     requireNamespace("glue", quietly = TRUE)) {
#'
#'   # Simple card content
#'   card_content <- list(
#'     htmltools::div("This is some example text inside the card.")
#'   )
#'
#'   # Create a Fluent UI card
#'   makeCard(
#'     title   = "Example Card",
#'     id      = "example-card",
#'     content = card_content,
#'     size    = 6
#'   )
#' }
makeCard <- function(title, id, content, size = 12, style = "") {

  # Check if shiny.fluent is installed
  if (!requireNamespace("shiny.fluent", quietly = TRUE)) {
    stop("Package 'shiny.fluent' is required for makeCard(). Please install it.", call. = FALSE)
  }

  # Check if glue is installed
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required for makeCard(). Please install it.", call. = FALSE)
  }

  # Check if htmltools is installed
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required for makeCard(). Please install it.", call. = FALSE)
  }

  # Use functions from shiny.fluent with :: prefix
  htmltools::div(
    id = id,
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    shiny.fluent::Stack(
      tokens = list(padding = 20, childrenGap = 5),
      shiny.fluent::Text(variant = "large", title, block = TRUE),
      content
    )
  )
}
