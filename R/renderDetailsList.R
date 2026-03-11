#' Create a Fluent UI Table
#'
#' This function makes the table in a Fluent UI format.
#'
#' @param df Any processed data frame
#' @return A table in the Fluent UI format
#' @export
#'
#' @importFrom dplyr as_tibble
#' @author Dionne Argyropoulos
#'
#' @examples
#' # Minimal example using a small data frame.
#' # This example is safe for CRAN because it runs only if
#' # shiny.fluent and htmltools are installed.
#'
#' if (requireNamespace("shiny.fluent", quietly = TRUE) &&
#'     requireNamespace("htmltools", quietly = TRUE)) {
#'
#'   # Tiny example data frame
#'   example_df <- data.frame(
#'     Sample = c("A", "B"),
#'     Value = c(10, 20),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # Render Fluent UI DetailsList
#'   renderDetailsList(example_df)
#' }
renderDetailsList <- function(df) {

  # Check if shiny.fluent is installed
  if (!requireNamespace("shiny.fluent", quietly = TRUE)) {
    stop("Package 'shiny.fluent' is required for renderDetailsList(). Please install it.", call. = FALSE)
  }

  # Check if htmltools is installed
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required for renderDetailsList(). Please install it.", call. = FALSE)
  }

  # Use functions from shiny.fluent with :: prefix
  htmltools::div(
    class = "ms-Grid-row",
    htmltools::div(
      class = "ms-Grid-col ms-sm12",  # Use ms-sm12 for full width on small screens
      shiny.fluent::Stack(
        tokens = list(childrenGap = 10),
        horizontal = TRUE,
        htmltools::div(
          style = "max-height: 600px; overflow: auto; width: 100%;",
          shiny.fluent::DetailsList(
            items = df,
            columns = dplyr::tibble(fieldName = names(df), name = names(df)),
            constrainMode = 0,
            checkboxVisibility = 2,
            styles = list(
              root = list(
                width = "100%",  # Ensure table width is constrained within the available space
                minWidth = "fit-content",  # Allow table to grow to fit content
                overflowX = "auto"  # Enable horizontal scrolling only when necessary
              )
            )
          )
        )
      )
    )
  )
}
