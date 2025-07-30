#' Check Platform
#'
#' This function checks the platform the user has input and whether it aligns
#' with the correct format as expected. Will report error if NOT aligned.
#'
#' @param raw_data String with the raw data path (reactive).
#' @param platform "magpix" or "bioplex" (reactive).
#' @return TRUE: if platform == file format, ERROR message when platform does
#' not equal file format.
#' @export
#' @importFrom meltr melt_csv melt_csv2
#' @importFrom readxl read_excel
#' @importFrom janitor row_to_names
#' @author Dionne Argyropoulos
checkPlatform <- function(raw_data, platform) {

  if (length(raw_data) == 0) {
    stop("No raw data files were provided.")
  }

  file_extension <- tools::file_ext(raw_data)  # Identify the file extension and read the file accordingly

  if (file_extension == "xlsx") {
    df <- suppressMessages(readxl::read_excel(raw_data, n_max = 5))
  } else if (file_extension == "csv") {
    first_lines <- readLines(raw_data, n = 5)           # Read the first few lines of the file
    if (any(grepl(";", first_lines))) {
      # IF EUROPEAN CSV WITH ; DELLIMITER
      df <- suppressMessages(suppressWarnings(meltr::melt_csv2(raw_data)))
    } else {
      # IF CONVENTIONAL CSV WITH , DELLIMITER
      df <-  suppressMessages(suppressWarnings(meltr::melt_csv(raw_data)))
    }
  }

  # Extract the first two column names
  col_names <- colnames(df)
  if (all(grepl("^X\\d+$", col_names))) {
    df <- suppressWarnings(df %>% janitor::row_to_names(row_number = 1))
  }
  first_two_cols <- colnames(df)[1:2]

  # Detect if the file is Magpix based on column names
  is_magpix <- any(grepl("Program|row", first_two_cols, ignore.case = TRUE)) ||
    any(grepl("xPonent|col", first_two_cols, ignore.case = TRUE))

  # User selected "magpix" but the file does not have "Program" or "xPonent"
  if (platform == "magpix" && !is_magpix) {
    stop(paste("Error: The file", file_name, "does not appear to be a 'magpix' file, but the platform was set to 'magpix'. Please check your selection."))
  }

  # User selected "bioplex" but the file contains "Program" or "xPonent"
  if (platform == "bioplex" && is_magpix) {
    stop(paste("Error: The file", file_name, "appears to be a 'magpix' file, but the platform was set to 'bioplex'. Please check your selection."))
  }

  return(TRUE)

}
