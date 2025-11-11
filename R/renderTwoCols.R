#' Create two columns in Fluent UI
#'
#' This function creates two columns in the Fluent UI format.
#'
#' @param first_col A list of content for the first column.
#' @param second_col A list of content for the second column.
#' @param first_width Percent width of the column space (default: 50\%).
#' @param second_width Percent width of the column space (default: 50\%).
#' @return Fluent UI window with two columns.
#' @export
renderTwoCols <- function(
    first_col,
    second_col,
    first_width = "50%",
    second_width = "50%"
) {

  # Check if shiny.fluent is installed
  if (!requireNamespace("shiny.fluent", quietly = TRUE)) {
    stop("Package 'shiny.fluent' is required for renderTwoCols(). Please install it.", call. = FALSE)
  }

  # Use functions from shiny.fluent with :: prefix
  shiny.fluent::Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 40),
    children = list(
      # First Column
      htmltools::div(
        tokens = list(childrenGap = 15),
        style = list(width = first_width),
        children = first_col  # First column content
      ),
      # Second Column
      htmltools::div(
        tokens = list(childrenGap = 15),
        style = list(width = second_width),
        children = second_col  # Second column content
      )
    )
  )
}
