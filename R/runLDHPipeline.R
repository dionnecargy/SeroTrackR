#' Run LDH Pipeline from Start to End
#'
#' A master function combining the entire LDH pipeline into one command to run in R.
#'
#' @param raw_data  String with the raw data path.
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param platform  "magpix" or "bioplex". Default: "Bioplex"
#' @param location  "PNG" or "ETH" to filter WEHI standard curve data. Default: "ETH"/
#' @param dilution  A list of numbers ranging from S1 to S10. Default: 1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81.
#' @param experiment_name User-input experiment name. Default: "experiment1".
#' @param file_path A file path to write the .csv final file. Default: Current working directory.
#'
#' @return A data frame containing the MFI and RAU Dilution values for each sample, QC plots for standard curve, bead counts and blanks.
#' @export
#'
#' @author Dionne Argyropoulos
runLDHPipeline <- function(
    raw_data,
    plate_layout,
    platform = "bioplex",
    location = "ETH",
    dilution = c(1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81),
    experiment_name = "experiment1",
    file_path = NULL
){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  serodata_output           <- readSeroData(raw_data, platform)
  antigen_output            <- serodata_output
  plate_list                <- readPlateLayout(plate_layout, antigen_output)

  #############################################################
  # Step 2: Quality Control and MFI to RAU
  #############################################################
  processCounts_output      <- processCounts(antigen_output)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, plate_list)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds_all(antigen_output, experiment_name))
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, plate_list)
  blanks_plot               <- plotBlanks(antigen_output, experiment_name)

  #############################################################
  # Step 4: MFI to RAU Conversion
  #############################################################
  mfitorau_output           <- MFItoRAU_LDH(antigen_output, plate_list, dilution, file_path)

  return(list(stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, mfitorau_output))
}
