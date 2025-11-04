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
  readSeroData_Output       <- readSeroData(raw_data = raw_data, platform)
  readPlateLayout_Output    <- readPlateLayout(plate_layout = plate_layout, serodata_output = readSeroData_Output)

  #############################################################
  # Step 2: Quality Control and MFI to RAU
  #############################################################
  processCounts_output      <- processCounts(readSeroData_Output)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, readPlateLayout_Output)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, readPlateLayout_Output)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)
  message("QC Processes completed.")

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds_PkPfPv(readSeroData_Output, experiment_name))
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, readPlateLayout_Output)
  blanks_plot               <- plotBlanks(readSeroData_Output, experiment_name)
  message("QC Plotting completed.")

  #############################################################
  # Step 4: Run new 5-point MFI to RAU
  #############################################################
  mfi_outputs               <- MFItoRAU_Plasmo(
    serodata_output = readSeroData_Output,
    plate_list = readPlateLayout_Output,
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
