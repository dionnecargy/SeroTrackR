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
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' # Minimal example using htmltools elements.
#' # This example runs without starting a Shiny app and is safe for CRAN.
#'
#' if (requireNamespace("shiny.fluent", quietly = TRUE) &&
#'     requireNamespace("htmltools", quietly = TRUE)) {
#'
#'   # Create simple content for each column
#'   col1 <- list(htmltools::div("First column content"))
#'   col2 <- list(htmltools::div("Second column content"))
#'
#'   # Render two columns with default widths
#'   renderTwoCols(first_col = col1, second_col = col2)
#' }
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

  # Check if htmltools is installed
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required for renderTwoCols(). Please install it.", call. = FALSE)
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
