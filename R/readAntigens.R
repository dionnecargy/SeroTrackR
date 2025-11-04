#' ARCHIVE: Standardise Antigen Names
#'
#' This function ensures that the antigens in the raw data adheres to our
#' nomenclature format in the data processing and model steps. This relies on
#' the output of the `readSeroData` to then use our nomenclature for the eight
#' antigens of interest in PvSeroApp.
#'
#' @return List of data frames with relabelled column names for our antigen names.
#' @export
#'
#' @author Dionne Argyropoulos
readAntigens <- function(){
  message("Function redundant. Please use the readSeroData() function.")
}
