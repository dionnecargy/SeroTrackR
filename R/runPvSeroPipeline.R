#' Run PvSero Pipeline from Start to End
#'
#' A master function combining the entire PvSeroApp pipeline into one command to run in R.
#'
#' @param raw_data  String with the raw data path.
#' @param platform  "magpix" or "bioplex". Default = "magpix".
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param location  "PNG" or "ETH" to filter WEHI standard curve data.
#' @param experiment_name User-input experiment name.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#' @param classify "Yes" or "No" depending on whether you would like classification or not. Default = "Yes".
#' @param algorithm_type Algorithm: "antibody_model" (PvSEM algorithm; default)
#' @param sens_spec User-selected Sensitivity/Specificity threshold: "balanced"
#' (default) or "90\% specificity".
#'
#' @returns classifyResults_output, stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot
#' @export
#'
#' @import workflows parsnip  ggplot2 drc dplyr
#' @importFrom janitor row_to_names
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace  str_detect
#' @importFrom tidyr  pivot_longer  pivot_wider drop_na nest unnest
#' @importFrom tools file_ext
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
#' # Run full pipeline including classification
#' runPvSeroPipeline(
#'   raw_data = your_raw_data,
#'   plate_layout = plate_layout,
#'   platform = "magpix",
#'   location = "PNG",
#'   experiment_name = "experiment1",
#'   std_point = 10,
#'   algorithm_type = "antibody_model",
#'   sens_spec = "balanced",
#'   classify = "Yes"
#' )
#'
#' # Run processing pipeline only (no classification)
#' runPvSeroPipeline(
#'   raw_data = your_raw_data,
#'   plate_layout = plate_layout,
#'   platform = "magpix",
#'   location = "PNG",
#'   experiment_name = "experiment1",
#'   std_point = 10,
#'   algorithm_type = "antibody_model",
#'   sens_spec = "balanced",
#'   classify = "No"
#' )
#' }
runPvSeroPipeline <- function(
    raw_data,
    plate_layout,
    platform = "magpix",
    location,
    experiment_name = "experiment1",
    std_point = 10,
    classify = "Yes",
    algorithm_type = "antibody_model",
    sens_spec = "balanced"
  ){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  sero_data                 <- readSeroData(raw_data, platform)
  plate_list                <- readPlateLayout(plate_layout, sero_data)

  #############################################################
  # Step 2: Quality Control and
  #############################################################
  qc_results                <- runQC(sero_data, plate_list)
  message("QC Processes completed.")

  #############################################################
  # Step 3: MFI to RAU Conversion
  #############################################################
  if(location == "ETH"){
    mfi_to_rau_output       <- suppressMessages(MFItoRAU_Adj(sero_data, plate_list, qc_results, std_point, project = NULL))
  } else if(location == "PNG"){
    mfi_to_rau_output       <- suppressWarnings(MFItoRAU(sero_data, plate_list, qc_results, std_point, project = NULL))
  }
  message("MFI to RAU conversion completed.")

  #############################################################
  # Step 4: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds(sero_data, location, experiment_name))
  plateqc_plot              <- plotCounts(qc_results, experiment_name)
  check_repeats_output      <- getRepeats(qc_results, plate_list)
  blanks_plot               <- plotBlanks(sero_data, experiment_name)
  if(location == "ETH"){
    model_plot              <- plotModel_Adj(mfi_to_rau_output, sero_data)
  } else if(location == "PNG"){
    model_plot              <- plotModel(mfi_to_rau_output, sero_data)
  }
  message("QC Plotting completed.")

  #############################################################
  # Step 5: Classification
  #############################################################
  if(classify == "Yes"){
    classifyResults_output    <- classifyResults(mfi_to_rau_output, algorithm_type, sens_spec, qc_results, project = NULL)
    message("Pv classification completed.")
    return(list(classifyResults_output, stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot))
  } else {
    message("No Classification Performed.")
    return(list(mfi_to_rau_output[[2]], stdcurve_plot, plateqc_plot, check_repeats_output, blanks_plot, model_plot))
  }

}
