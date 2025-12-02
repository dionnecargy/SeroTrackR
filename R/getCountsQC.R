#' Get All Counts Data
#'
#' This function obtains the count data from the raw Median Fluorescent
#' Intensity (MFI). This function relies on the output of the Antigen-specific
#' counts (`getAntigenCounts`) and the Well or Sample-specific counts
#' (`getCounts`).
#'
#' @param antigen_counts_output Output from `getAntigenCounts` (reactive).
#' @param counts_output Output from `getCounts` (reactive).
#' @return Joined data frame for all count data.
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select rename_with left_join all_of
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' # Step 0: Load example raw data
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Step 1: Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Step 2: Process counts and perform quality control
#' counts      <- processCounts(sero_data)
#' counts_raw  <- getCounts(counts)
#' sample_ids  <- getSampleID(counts, plate_list)
#' antigen_cts <- getAntigenCounts(counts, plate_list)
#' counts_qc   <- getCountsQC(antigen_cts, counts_raw)
#'
#' }
getCountsQC <- function(antigen_counts_output, counts_output){

  #############################################################################
  # Data Wrangling
  #############################################################################

  # 1. Data Wrangling to store counts per antigen output
  antigen_counts_only <- antigen_counts_output %>%
    tidyr::pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "Count") %>% ungroup() %>%
    dplyr::select(Location, SampleID, Plate, everything()) %>%
    dplyr::rename_with(~ paste0(., "_Count"), .cols = where(is.numeric))

  # 2. Data Wrangling to store QC pass/fail per antigen output
  antigen_QC_only <- antigen_counts_output %>%
    tidyr::pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "QC_antigen") %>%
    dplyr::rename_with(~ paste0(., "_QC"), .cols = -c(SampleID, Location, Plate))

  # 3. Join both antigen-specific data frames together
  joined_antigen_counts <- antigen_counts_only %>%
    dplyr::left_join(antigen_QC_only, by = c("SampleID", "Location", "Plate"))

  #############################################################################
  # Re-arrange data
  #############################################################################

  # Get all base marker names by stripping _Count
  marker_bases <- names(joined_antigen_counts) %>%
    grep("_Count$", ., value = TRUE) %>%
    sub("_Count$", "", .)

  # Create the desired column order
  new_order <- c(
    "Location", "SampleID", "Plate",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_Count"), paste0(x, "_QC"))))
  )

  # Reordered data frame
  joined_antigen_counts <- joined_antigen_counts %>%
    dplyr::select(all_of(new_order))

  #############################################################################
  # Add total counts
  #############################################################################

  total_counts_only <- counts_output %>%
    dplyr::select(Location, Plate, QC_total)

  total_counts_final_output <- joined_antigen_counts %>%
    dplyr::left_join(total_counts_only, by = c("Location", "Plate"))

  return(total_counts_final_output)

}
