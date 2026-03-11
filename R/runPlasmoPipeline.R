#' Run Pk/Pf/Pv Data Analysis Pipeline from Start to End
#'
#' @param raw_data  String with the raw data path.
#' @param platform  "magpix" or "bioplex". Default: "Bioplex"
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1" or user provided csv of Antigens and Species.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param experiment_name User-input experiment name. Default: "experiment1".
#' @param classify "Yes" or "No" depending on whether you would like classification or not. Default = "Yes".
#' @param algorithm_type User-selected algorithm choice:
#' - "antibody_model" (PvSeroTaT model; default), or
#' - "antibody_model_excLF016" (PvSeroTaT excluding LF016).
#' @param sens_spec User-selected Sensitivity/Specificity threshold:
#' - "balanced" (default),
#' - "85\% sensitivity",
#' - "90\% sensitivity",
#' - "95\% sensitivity",
#' - "85\% specificity",
#' - "90\% specificity".
#' - "95\% specificity".
#'
#' @return A data frame containing the MFI and RAU Dilution values for each sample, QC plots for standard curve, bead counts and blanks.
#' @export
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' # Helper to avoid repetition in examples
#' run_example_std <- function(std_point) {
#'   # Load raw data for given standard curve
#'   your_raw_data <- c(
#'     system.file("extdata",
#'                 paste0("example_MAGPIX_pk_", std_point, "std_plate1.csv"),
#'                 package = "SeroTrackR"),
#'     system.file("extdata",
#'                 paste0("example_MAGPIX_pk_", std_point, "std_plate2.csv"),
#'                 package = "SeroTrackR")
#'   )
#'
#'   layout_file <- system.file(
#'     "extdata",
#'     paste0("example_platelayout_pk_", std_point, "std.xlsx"),
#'     package = "SeroTrackR"
#'   )
#'
#'   # Run pipeline
#'   runPlasmoPipeline(
#'     raw_data = your_raw_data,
#'     platform = "magpix",
#'     plate_layout = layout_file,
#'     panel = "panel1",
#'     std_point = std_point,
#'     experiment_name = paste0(std_point, "-point standard curve")
#'   )
#' }
#'
#' # ---- 5-point standard curve ----
#' results_5std <- run_example_std(5)
#'
#' # ---- 10-point standard curve ----
#' results_10std <- run_example_std(10)
#'
#' }
runPlasmoPipeline <- function(
    raw_data,
    platform = "magpix",
    plate_layout,
    panel = "panel1",
    std_point,
    experiment_name = "experiment1",
    classify = "Yes",
    algorithm_type = "antibody_model",
    sens_spec = "balanced"
  ){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  sero_data                 <- readSeroData(raw_data = raw_data, platform)
  plate_list                <- readPlateLayout(plate_layout = plate_layout, sero_data = sero_data)

  #############################################################
  # Step 2: Quality Control
  #############################################################
  qc_results                <- runQC(sero_data, plate_list)
  message("QC Processes completed.")

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds_PkPfPv(sero_data, experiment_name, panel))
  plateqc_plot              <- plotCounts(qc_results, experiment_name)
  check_repeats_output      <- getRepeats(qc_results, plate_list)
  blanks_plot               <- plotBlanks(sero_data, experiment_name)
  message("QC Plotting completed.")

  #############################################################
  # Step 4: Run new 5-point MFI to RAU
  #############################################################
  mfi_outputs               <- MFItoRAU_Plasmo(
    sero_data = sero_data,
    plate_list = plate_list,
    panel = panel,
    std_point = std_point,
    qc_results = qc_results
  )
  message("MFI to RAU conversion completed.")

  #############################################################
  # Step 5: Perform Pv classification
  #############################################################
  if(classify == "Yes"){
    Pv_classified             <- classifyResults(mfi_outputs, algorithm_type, sens_spec, qc_results, project = "pkpfpv")
    message("Pv classification completed.")
    return(list(
      std_curve = stdcurve_plot,
      bead_counts = plateqc_plot,
      blanks = blanks_plot,
      mfi_outputs = mfi_outputs,
      pv_classification = Pv_classified
    ))
  } else {
    message("No Classification Performed.")
    return(list(
      std_curve = stdcurve_plot,
      bead_counts = plateqc_plot,
      blanks = blanks_plot,
      mfi_outputs = mfi_outputs
    ))
  }

}
