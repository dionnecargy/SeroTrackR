#' Read Plate Layout/s
#'
#' This function imports the plate layout. Each sheet of the plate layout
#' ".xlsx" file must contain 13 columns (labelled
#' Plate, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) (columns A-M) and 9 rows
#' (Plate, A, B, C, D, E, F, G, H) (rows 1-9). *Note that the first row/column
#' i.e., the A1 cell in excel is called "Plate". This function also checks that
#' the plate sheet labels are consistent with the MAGPIX file input names, as a
#' check prior to merging downstream.
#'
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2...
#' etc..
#' @param sero_data Output from `readSeroData()`.
#' @return A list of data frames, with each one representing an individual plate.
#' @export
#' @importFrom openxlsx getSheetNames read.xlsx
#' @author Shazia Ruybal-Pesántez, Dionne Argyropoulos
#'
#' @examples
#'
#' # Example input files
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Step 1: Read and combine serological data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
#
#' # Step 2: Read plate layout
#' plate_list <- readPlateLayout(
#'   plate_layout = your_plate_layout,
#'   sero_data = sero_data
#' )
readPlateLayout <- function(plate_layout, sero_data) {

  if (is.null(plate_layout) || !file.exists(plate_layout)) {
    stop("ERROR: Invalid plate layout file provided.")
  }

  sheet_names <- tryCatch({
    openxlsx::getSheetNames(plate_layout)
  }, error = function(e) {
    stop("ERROR: Failed to read sheet names. Ensure the file is a valid Excel file.")
  })

  # Step 1: Get the sheet names to confirm
  sheet_names <- openxlsx::getSheetNames(plate_layout)

  # Step 2: Read all sheets into plate_list using indices
  plate_list <- lapply(1:length(sheet_names), function(i) {
    openxlsx::read.xlsx(plate_layout, sheet = i)
  })

  # Step 3: Name each element in the list after the corresponding sheet name
  names(plate_list) <- sheet_names

  # Step 4: Check if 'Plate' column exists in sero_data$results
  antigen_output_results <- sero_data$results

  if (!"Plate" %in% colnames(antigen_output_results)) {
    stop("ERROR: 'Plate' column is missing from sero_data$results.")
  }

  # Step 5: Extract levels from 'Plate' column
  antigen_output_levels <- unique(as.character(sero_data$results$Plate))  # Convert factor to character

  # Step 6: Compare plate names
  if (all(antigen_output_levels %in% sheet_names)) {
    message("Plate layouts correctly identified!")
  } else {
    stop("Plate layout sheets and plates labeled in raw data file names do not match. Ensure plate sheets are correctly labeled.")
  }

  return(plate_list)
}
