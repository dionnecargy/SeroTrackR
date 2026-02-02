#' Run LDH Pipeline from Start to End
#'
#' A master function combining the entire LDH pipeline into one command to run in R.
#'
#' @param raw_data  String with the raw data path.
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param platform  "magpix" or "bioplex". Default: "Bioplex"
#' @param dilution  A list of numbers ranging from S1 to S10. Default: 1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81.
#' @param experiment_name User-input experiment name. Default: "experiment1".
#' @param file_path A file path to write the .csv final file. Default: Current working directory.
#'
#' @return A data frame containing the MFI and RAU Dilution values for each sample, QC plots for standard curve, bead counts and blanks.
#' @export
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' # Example input files
#' your_raw_data <- system.file(
#'   "extdata",
#'   "example_BioPlex_PvLDH_plate1.xlsx",
#'   package = "SeroTrackR"
#' )
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Run full LDH processing pipeline
#' runLDHPipeline(
#'   raw_data      = your_raw_data,        # Vector of raw data files
#'   plate_layout  = your_plate_layout,    # Plate layout file
#' )
#'
#' }
runLDHPipeline <- function(
    raw_data,
    plate_layout,
    platform = "bioplex",
    dilution = c(1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81),
    experiment_name = "experiment1",
    file_path = NULL
){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  sero_data                 <- readSeroData(raw_data, platform)
  plate_list                <- readPlateLayout(plate_layout, sero_data)

  #############################################################
  # Step 2: Quality Control
  #############################################################
  qc_results                <- runQC(sero_data, plate_list)

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds_all(sero_data, experiment_name))
  plateqc_plot              <- plotCounts(qc_results, experiment_name)
  check_repeats_output      <- getRepeats(qc_results, plate_list)
  blanks_plot               <- plotBlanks(sero_data, experiment_name)

  #############################################################
  # Step 4: MFI to RAU Conversion
  #############################################################
  mfitorau_output           <- MFItoRAU_LDH(sero_data, plate_list, dilution, file_path)

  return(list(stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, mfitorau_output))
}
