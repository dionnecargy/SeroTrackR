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

  # Turn on debugging
  Sys.setenv(TINYTEX_DEBUG = "1")

  # Render without cleaning (keeps .log file)
  rmarkdown::render(
    input = input,
    output_file = output,
    params = params,
    envir = new.env(parent = globalenv()),
    clean = FALSE
  )

  # Locate the .log file
  log_file <- sub("\\.pdf$", ".log", output)

  # If not found, try .tex → .log
  if (!file.exists(log_file)) {
    tex_file <- sub("\\.pdf$", ".tex", output)
    log_file <- sub("\\.tex$", ".log", tex_file)
  }

  # Print log into Shiny logs
  if (file.exists(log_file)) {
    cat("\n===== LATEX LOG START =====\n")
    cat(readLines(log_file), sep = "\n")
    cat("\n===== LATEX LOG END =====\n")
  } else {
    cat("No LaTeX log file found.\n")
  }
}
