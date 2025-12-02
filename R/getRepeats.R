#' Check Beads to Repeat
#'
#' This function gets the count data and outputs a table of the isolates to
#' repeat or a statement to confirm that none need to be repeated.
#'
#' @param counts_output Output from `getCounts()` (reactive).
#' @param processed_counts Output from `processCounts()`.
#' @param plate_list Output from `readPlateLayout()` (reactive).
#' @return A data frame with wells to "fail", OR if no "fail" found will return
#' text "No repeats necessary".
#' @export
#' @importFrom tidyr drop_na
#' @importFrom dplyr left_join select distinct filter
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' # Step 0: Load example raw data and plate layout
#' raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#' plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")
#'
#' # Step 1: Read data and plate layout
#' sero_data <- readSeroData(raw_data, platform = "magpix")
#' plate_list <- readPlateLayout(plate_layout, sero_data)
#'
#' # Step 2: Process counts
#' processed_counts <- processCounts(sero_data)
#' counts_output <- getCounts(processed_counts)
#'
#' # Step 3: Identify samples to repeat
#' repeats_table <- getRepeats(
#'   counts_output = counts_output,
#'   processed_counts = processed_counts,
#'   plate_list = plate_list
#' )
#'
#' # View results
#' repeats_table
#' }
getRepeats <- function(counts_output, processed_counts, plate_list) {

  # 1. Filter "Repeats" in Counts Output
  repeats <- counts_output %>% dplyr::filter(QC_total == "fail")
  # 2. If zero "Repeats" found, then write text. If "Repeats" found, then output table.
  if (nrow(repeats) == 0) {
    return("No repeats necessary.")
  } else {
    table <- getSampleID(processed_counts, plate_list) %>% dplyr::distinct(SampleID, Location, Plate)
    table <- table %>%
      dplyr::left_join(repeats, by = c("Location", "Plate")) %>%
      tidyr::drop_na() %>%
      dplyr::select(Location, SampleID, Plate, QC = QC_total)
    return(table)
  }
}
