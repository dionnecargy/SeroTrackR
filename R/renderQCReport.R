#' Generate QC PDF Report
#'
#' @param raw_data A string with the raw data path.
#' @param platform A string: "magpix", "intelliflex", or "bioplex".
#' @param plate_layout A string with the plate layout path.
#' @param experiment_name A string for experiment name.
#' @param date A string or Date. Defaults to today's date.
#' @param experiment_notes A string of notes. Default is "no notes".
#' @param location A string for experiment location: "ETH" or "PNG" accepted.
#' @param path Output path for the PDF file. Defaults to current working directory.
#'
#' @return Rendered PDF report.
#' @export
#'
#' @author Dionne Argyropoulos
renderQCReport <- function(
    raw_data,
    plate_layout,
    platform,
    experiment_name = "experiment1",
    date = format(Sys.Date(), "%Y%m%d"), # default to today's date
    experiment_notes = "no notes",
    location,
    path = "." # Default to current working directory
  ) {

  ###############################################################################
  # ----- Load Data functions -----
  ###############################################################################

  serodata_output           <- readSeroData(raw_data, platform)
  antigen_output            <- readAntigens(serodata_output)
  raw_data_info             <- antigen_output$data_raw
  raw_data_filename         <- tolower(basename(raw_data))
  plate_list                <- readPlateLayout(plate_layout, antigen_output)
  version                   <- getGithubRelease("dionnecargy", "PvSeroApp")

  processCounts_output      <- processCounts(antigen_output)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, plate_list)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)
  mfi_to_rau_output         <- suppressMessages(MFItoRAU_ETH(antigen_output, plate_list, getCountsQC_output))

  stdcurve_plot             <- suppressWarnings(plotStds(antigen_output, location, experiment_name))
  plateqc_plot              <- plotCounts(getCounts_output, experiment_name)
  check_repeats_output      <- getRepeats(getCounts_output, processCounts_output, plate_list)
  blanks_plot               <- plotBlanks(antigen_output, experiment_name)
  model_plot                <- plotModel_ETH(mfi_to_rau_output, antigen_output)

  ###############################################################################
  # ----- Create helper functions -----
  ###############################################################################

  operator_output <- function() {
    if (platform %in% c("magpix", "intelliflex")) {
      op <- raw_data_info %>%
        dplyr::filter(Program == "Operator") %>%
        dplyr::select(Plate, Operator = xPONENT)
      paste(paste0(op$Plate, ": ", op$Operator), collapse = ", ")
    } else {
      return("Information not available for bioplex machine run.")
    }
  }

  volume_output <- function() {
    if (platform == "magpix") {
      vol <- raw_data_info %>%
        dplyr::filter(Program == "SampleVolume") %>%
        dplyr::select(Plate, `Acquisition Volume` = xPONENT)
      paste(paste0(vol$Plate, ": ", vol$`Acquisition Volume`), collapse = ", ")
    } else if (platform == "intelliflex") {
      vol <- raw_data_info %>%
        dplyr::filter(Program == "MaxSampleUptakeVolume") %>%
        dplyr::select(Plate, `Acquisition Volume` = xPONENT)
      paste(paste0(vol$Plate, ": ", vol$`Acquisition Volume`), collapse = ", ")
    } else {
      return("Information not available for bioplex machine run.")
    }
  }

  calibration_output <- function() {
    if (platform == "magpix") {
      calib <- raw_data_info %>%
        dplyr::filter(Program %in% c("Last CAL Calibration", "Last VER Verification", "Last Fluidics Test")) %>%
        dplyr::select(Plate, Program, Result = xPONENT)
      paste(paste0(calib$Plate, ": ", calib$Program, ": ", calib$Result), collapse = ", ")
    } else if (platform == "intelliflex") {
      calib <- raw_data_info %>%
        dplyr::filter(Program %in% c("Last Calibration", "Last Verification", "Last Fluidics Test")) %>%
        dplyr::select(Plate, Program, Result = xPONENT)
      paste(paste0(calib$Plate, ": ", calib$Program, ": ", calib$Result), collapse = ", ")
    } else {
      return("Information not available for bioplex machine run.")
    }
  }

  machine_output <- function() {
    if (platform == "magpix") {

      # String to search for
      search_str <- "MachineSerialNo"
      # Find cols with search string in any row
      matching_cols <- names(raw_data_info)[sapply(raw_data_info, function(col) any(grepl(search_str, col)))]
      # Filter the data frame to include only those cols
      filtered_df <- raw_data_info %>% dplyr::select(all_of(matching_cols))
      # Get the unknown column name
      col_name <- names(filtered_df)[1]
      # Find row indices where the string appears
      matching_indices <- which(filtered_df[[col_name]] == search_str)
      # Get indices of the rows BELOW the matching rows
      below_indices <- matching_indices + 1
      # Remove indices that are out of bounds (i.e., last row has no row below it)
      below_indices <- below_indices[below_indices <= nrow(filtered_df)]
      # Filter the data frame for these rows
      machine <- filtered_df[below_indices, , drop = FALSE] %>% dplyr::rename(`Machine Serial Number` = col_name)
      machine_levels <- unique(raw_data_info$Plate)
      paste(paste0(machine_levels, ": ", machine$`Machine Serial Number`), collapse = ", ")

    } else if (platform == "bioplex") {

      machine <- raw_data_info %>% filter(str_detect(Run, "Reader Serial Number")) %>% mutate(Run = gsub("Reader Serial Number: ", "", Run)) %>% dplyr::select(Run)
      machine_levels <- unique(raw_data_info$Plate)
      paste(paste0(machine_levels, ": ", machine$Run), collapse = ", ")

    } else if (platform == "intelliflex") {
      machine <- raw_data_info %>%
        dplyr::filter(Program == "SN") %>%
        dplyr::select(Plate, `Machine Serial Number` = xPONENT)
      paste(paste0(machine$Plate, ": ", machine$`Machine Serial Number`), collapse = ", ")

    }
  }

  check_repats_table_pdf <- function(check_repeats_output) {
    if (is.data.frame(check_repeats_output)) {
      return(check_repeats_output)
    } else {
      return(NULL)
    }
  }

  plate_list_output <- function() {
    tables_output <- lapply(seq_along(plate_list), function(i) {
      table_header <- paste0("##### Plate: ", i, "\n\n")
      table_content <- knitr::kable(plate_list[[i]], format = "latex", booktabs = TRUE)
      paste0(table_header, table_content)
    })
    knitr::asis_output(paste(tables_output, collapse = "\n\n"))
  }

  ###############################################################################
  # ----- Generate PDF Document -----
  ###############################################################################

  suppressWarnings(
    rmarkdown::render(
      input = system.file("rmd/template.Rmd", package="SeroTrackR"),
      output_file = paste0(experiment_name, "_", date, "_", location, "_", version, "_QCreport.pdf"),
      output_dir = here::here(),
      params = list(
        raw_data_filename = raw_data_filename,
        experiment_name = experiment_name,
        date = date,
        experiment_notes = experiment_notes,
        platform = platform,
        stdcurve_plot = stdcurve_plot,
        plateqc_plot = plateqc_plot,
        blanks_plot = blanks_plot,
        check_repeats_output = check_repeats_output,
        check_repats_table_pdf = check_repats_table_pdf(check_repeats_output),
        model_plot = model_plot,
        operator_output = operator_output(),
        volume_output = volume_output(),
        calibration_output = calibration_output(),
        machine_output = machine_output(),
        plate_list_output = plate_list_output()
      )
    )
  )

}
