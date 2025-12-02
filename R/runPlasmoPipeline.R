#' Run Pk/Pf/Pv Data Analysis Pipeline from Start to End
#'
#' @param raw_data  String with the raw data path.
#' @param platform  "magpix" or "bioplex". Default: "Bioplex"
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1".
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param experiment_name User-input experiment name. Default: "experiment1".
#' @param algorithm_type User-selected algorithm choice:
#' - "antibody_model" (PvSeroTaT model; default), or
#' - "antibody_model_excLF016" (PvSeroTaT excluding LF016).
#' @param sens_spec User-selected Sensitivity/Specificity threshold:
#' - "maximised" (default),
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
    algorithm_type = "antibody_model",
    sens_spec = "maximised"
  ){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  sero_data       <- readSeroData(raw_data = raw_data, platform)
  plate_list    <- readPlateLayout(plate_layout = plate_layout, sero_data = sero_data)

  #############################################################
  # Step 2: Quality Control and MFI to RAU
  #############################################################
  processCounts_output      <- processCounts(sero_data)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, plate_list)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)
  message("QC Processes completed.")

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds_PkPfPv(sero_data, experiment_name))
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, plate_list)
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
    counts_QC_output = getCountsQC_output
  )
  message("MFI to RAU conversion completed.")

  #############################################################
  # Step 5: Perform Pv classification
  #############################################################
  Pv_classified <- classifyPv(mfi_outputs, algorithm_type, sens_spec, getCountsQC_output)
  message("Pv classification completed.")

  #############################################################
  # Outputs
  #############################################################
  return(list(
    std_curve = stdcurve_plot,
    bead_counts = plateqc_plot,
    blanks = blanks_plot,
    mfi_outputs = mfi_outputs,
    pv_classification = Pv_classified
  ))

}
