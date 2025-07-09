#' Create a Fluent UI Table
#'
#' This function makes the table in a Fluent UI format.
#'
#' @param df Any processed data frame
#' @return A table in the Fluent UI format
#' @export
#' @importFrom shiny.fluent Stack DetailsList
#' @author Dionne Argyropoulos
renderDetailsList <- function(df) {
  div(
    class = "ms-Grid-row",
    div(
      class = "ms-Grid-col ms-sm12",  # Use ms-sm12 for full width on small screens
      shiny.fluent::Stack(
        tokens = list(childrenGap = 10),
        horizontal = TRUE,
        div(
          style = "max-height: 600px; overflow: auto; width: 100%;",
          shiny.fluent::DetailsList(
            items = df,
            columns = tibble(fieldName = names(df), name = names(df)),
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
