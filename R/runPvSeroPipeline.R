#' Run PvSero Pipeline from Start to End
#'
#' A master function combining the entire PvSeroApp pipeline into one command to run in R.
#'
#' @param raw_data  String with the raw data path.
#' @param platform  "magpix" or "bioplex".
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param location  "PNG" or "ETH" to filter WEHI standard curve data.
#' @param experiment_name User-input experiment name.
#' @param classify "Yes" or "No" depending on whether you would like classification or not.
#' @param algorithm_type  User-selected algorithm choice:
#' - "antibody_model" (PvSeroTaT model; default), or
#' - "antibody_model_excLF016" (PvSeroTat excluding LF016).
#' @param sens_spec User-selected Sensitivity/Specificity threshold:
#' - "maximised" (default),
#' - "85\% sensitivity",
#' - "90\% sensitivity",
#' - "95\% sensitivity",
#' - "85\% specificity",
#' - "90\% specificity".
#' - "95\% specificity".
#'
#' @returns classifyResults_output, stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot
#' @export
#'
#' @import workflows parsnip  ggplot2 drc dplyr
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
runPvSeroPipeline <- function(raw_data, plate_layout, platform, location, experiment_name, classify, algorithm_type, sens_spec){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  serodata_output           <- readSeroData(raw_data, platform)
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
  mfi_to_rau_output         <- suppressMessages(MFItoRAU_ETH(antigen_output, plate_list, getCountsQC_output))

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds(antigen_output, location, experiment_name))
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, plate_list)
  blanks_plot               <- plotBlanks(antigen_output, experiment_name)
  model_plot                <- plotModel_ETH(mfi_to_rau_output, antigen_output)

  #############################################################
  # Step 4: Classification
  #############################################################
  if(classify == "Yes"){
    classifyResults_output    <- classifyResults(mfi_to_rau_output, algorithm_type, sens_spec, getCountsQC_output)
    return(list(classifyResults_output, stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot))
  } else {
    message("No Classification Performed")
    return(list(mfi_to_rau_output[[2]], stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot))
  }

}
