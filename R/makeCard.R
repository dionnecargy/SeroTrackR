#' Make Card in Fluent UI
#'
#' This function imports the makes a card following the Fluent UI format.
#'
#' @param title String with the large title that will be printed in the card.
#' @param id Identifying tag for use to link.
#' @param content A list of content to be rendered.
#' @param size A value from 1 to 12 of the width of the screen (default = 12).
#' @param style Value for any css styling (reactive).
#' @return A "card" in the Fluent UI format with content.
#' @export
#' @importFrom shiny.fluent Stack
makeCard <- function(title, id, content, size = 12, style = "") {
  div(
    id = id,
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    shiny.fluent::Stack(
      tokens = list(padding = 20, childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}
