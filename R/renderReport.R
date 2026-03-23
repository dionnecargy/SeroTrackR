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
#'
#' @examples
#' # Minimal example that renders a temporary Rmd file.
#' # Safe for CRAN because it only writes to tempdir()
#' \dontrun{
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'
#'   # Create a temporary Rmd that declares params in the YAML
#'   rmd_file <- tempfile(fileext = ".Rmd")
#'   writeLines(c(
#'     "---",
#'     "title: \"Test Report\"",
#'     "output: html_document",
#'     "params:",
#'     "  value: 0",
#'     "---",
#'     "",
#'     "This is a test report.",
#'     "",
#'     "Parameter value: `r params$value`"
#'   ), con = rmd_file)
#'
#'   # Output location
#'   out_file <- tempfile(fileext = ".html")
#'
#'   # Example parameters to pass in
#'   example_params <- list(value = 123)
#'
#'   # Render report
#'   renderReport(
#'     input  = rmd_file,
#'     output = out_file,
#'     params = example_params
#'   )
#'
#'   # Optionally inspect the output path
#'   out_file
#' }
#' }
renderReport <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}
