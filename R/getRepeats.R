#' Check Beads to Repeat
#'
#' This function gets the count data and outputs a table of the isolates to
#' repeat or a statement to confirm that none need to be repeated.
#'
#' @param qc_results Output from `runQC()`.
#' @param plate_list Output from `readPlateLayout()`.
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
#' sero_data   <- readSeroData(raw_data, platform = "magpix")
#' plate_list  <- readPlateLayout(plate_layout, sero_data)
#'
#' # Step 2: Process counts
#' qc_results  <- runQC(sero_data, plate_list)
#'
#' # Step 3: Identify samples to repeat
#' repeats_table <- getRepeats(
#'   qc_results = qc_results,
#'   plate_list = plate_list
#' )
#'
#' # View results
#' repeats_table
#' }
getRepeats <- function(qc_results, plate_list) {

  counts_output     <- qc_results$getCounts_output
  sample_id_output  <- qc_results$sampleid_output

  # 1. Filter "Repeats" in Counts Output
  repeats <- counts_output %>% dplyr::filter(QC_total == "fail")
  # 2. If zero "Repeats" found, then write text. If "Repeats" found, then output table.
  if (nrow(repeats) == 0) {
    return("No repeats necessary.")
  } else {
    table <- sample_id_output %>% dplyr::distinct(SampleID, Location, Plate)
    table <- table %>%
      dplyr::left_join(repeats, by = c("Location", "Plate")) %>%
      tidyr::drop_na() %>%
      dplyr::select(Location, SampleID, Plate, QC = QC_total)
    return(table)
  }
}
