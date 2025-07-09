#' Render Markdown report
#'
#' A short function to render the rmarkdown report on Shiny.
#'
#' @param input Input files
#' @param output Output files
#' @param params Parameters to generate outputs.
#' @return PDF output.
#' @export
#' @importFrom rmarkdown render
#' @author Dionne Argyropoulos
renderReport <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}
