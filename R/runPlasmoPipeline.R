#' Run Pk/Pf/Pv Data Analysis Pipeline from Start to End
#'
#' @param raw_data  String with the raw data path.
#' @param platform  "magpix" or "bioplex". Default: "Bioplex"
#' @param plate_layout An ".xlsx" file with sheets labelled plate1, plate2... etc.
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1".
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param experiment_name User-input experiment name. Default: "experiment1".
#'
#' @return A data frame containing the MFI and RAU Dilution values for each sample, QC plots for standard curve, bead counts and blanks.
#' @export
#'
#' @author Dionne Argyropoulos
runPlasmoPipeline <- function(raw_data, platform = "magpix", plate_layout, panel = "panel1", std_point, experiment_name = "experiment1"){

  #############################################################
  # Step 1: Reading in Raw Data
  #############################################################
  readSeroData_Output       <- readSeroData(raw_data = raw_data, platform)
  readPlateLayout_Output    <- readPlateLayout(plate_layout = plate_layout, antigen_output = readSeroData_Output)

  #############################################################
  # Step 2: Quality Control and MFI to RAU
  #############################################################
  processCounts_output      <- processCounts(readSeroData_Output)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, readPlateLayout_Output)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, readPlateLayout_Output)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)

  #############################################################
  # Step 3: Plotting
  #############################################################
  stdcurve_plot             <- suppressWarnings(plotStds_PkPfPv(readSeroData_Output, experiment_name))
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, plate_list)
  blanks_plot               <- plotBlanks(readSeroData_Output, experiment_name)

  #############################################################
  # Step 4: Run new 5-point MFI to RAU
  #############################################################
  mfi_outputs               <- MFItoRAU_Plasmo(readSeroData_Output, readPlateLayout_Output, panel, std_point, getCountsQC_output)

  #############################################################
  # Outputs
  #############################################################
  return(list(
    std_curve = stdcurve_plot,
    bead_counts = plateqc_plot,
    blanks = blanks_plot,
    mfi_outputs = mfi_outputs
  ))

}
