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
#' @importFrom shiny.fluent Stack
renderTwoCols <- function(
    first_col,
    second_col,
    first_width = "50%",
    second_width = "50%"
) {
  shiny.fluent::Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 40),
    children = list(
      # First Column
      div(
        tokens = list(childrenGap = 15),
        style = list(width = first_width),
        children = first_col  # First column content
      ),
      # Second Column
      div(
        tokens = list(childrenGap = 15),
        style = list(width = second_width),
        children = second_col  # Second column content
      )
    )
  )
}
