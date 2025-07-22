#' Run Serology Analysis Pipeline from Start to End
#'
#' A master function combining the entire Serology Analysis pipeline into one command to run in R.
#' This is a similar function as `runPvSeroPipeline()` except can be used for non-Pv samples as there is no classification step.
#'
#' @param raw_data  String with the raw data path (reactive).
#' @param raw_data_filenames  String with the raw data filenames (reactive).
#' @param platform  "magpix" or "bioplex" (reactive).
#' @param location  "PNG" or "ETH" to filter WEHI standard curve data (reactive).
#' @param experiment_name User-input experiment name (reactive).
#'
#' @returns mfi_to_rau_output, stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot
#' @export
#'
#' @import ggplot2  drc dplyr
#' @importFrom janitor row_to_names
#' @importFrom meltr melt_csv2 melt_csv
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom plyr join
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace  str_detect
#' @importFrom tidyr  pivot_longer  pivot_wider drop_na nest unnest
#' @importFrom tools file_ext
#'
#' @author Dionne Argyropoulos
runSeroPipeline <- function(raw_data, raw_data_filenames, platform, location, experiment_name){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  serodata_output           <- readSeroData(raw_data, raw_data_filenames, platform)
  antigen_output            <- readAntigens(serodata_output)
  plate_list                <- readPlateLayout(plate_layout, antigen_output)

  #############################################################
  # Step 2: Quality Control and MFI to RAU
  #############################################################
  processCounts_output      <- processCounts(antigen_output)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, plate_list)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)
  mfi_to_rau_output         <- MFItoRAU_ETH(antigen_output, plate_list, getCountsQC_output)

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- plotStds(antigen_output, location, experiment_name)
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, plate_list)
  blanks_plot               <- plotBlanks(antigen_output, experiment_name)
  model_plot                <- plotModel_ETH(mfi_to_rau_output, antigen_output)

  return(mfi_to_rau_output, stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot)
}
