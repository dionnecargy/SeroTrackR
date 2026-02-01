#' Run Quality Control Pipeline
#'
#' A master function containing each quality control processing step.
#'
#' @param sero_data Output from `readSeroData()` (reactive).
#' @param plate_list Output from `readPlateLayout()` (reactive).
#' @param location  "PNG" or "ETH" to filter WEHI standard curve data.
#' @param experiment_name User-input experiment name.
#'
#' @returns processCounts_output, getCounts_output, sampleid_output, getAntigenCounts_output, getCountsQC_output, mfi_to_rau_output
#' @export
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#'
#' \donttest{
#' # Example data supplied with the package
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' plate_layout <- system.file(
#'   "extdata", "example_platelayout_1.xlsx", package = "SeroTrackR"
#' )
#'
#' # Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Run full pipeline including classification
#' runQC(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   location = "PNG",
#'   experiment_name = "experiment1"
#' )
#' }
runQC <- function(sero_data, plate_list, location, experiment_name){

  # Quality Control and MFI to RAU
  processCounts_output      <- processCounts(sero_data)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, plate_list)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)
  if(location == "ETH"){
    mfi_to_rau_output       <- suppressMessages(MFItoRAU_ETH(sero_data, plate_list, getCountsQC_output))
  } else if(location == "PNG"){
    mfi_to_rau_output       <- suppressMessages(MFItoRAU(sero_data, plate_list, getCountsQC_output))
  }

  qc_outputs <- list(
    processCounts_output = processCounts_output,
    getCounts_output = getCounts_output,
    sampleid_output = sampleid_output,
    getAntigenCounts_output = getAntigenCounts_output,
    getCountsQC_output = getCountsQC_output,
    mfi_to_rau_output = mfi_to_rau_output
  )

  return(qc_outputs)
}
