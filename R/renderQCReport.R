#' Generate QC PDF Report
#'
#' @param raw_data A string with the raw data path.
#' @param platform A string: "magpix", "intelliflex", or "bioplex".
#' @param plate_layout A string with the plate layout path.
#' @param experiment_name A string for experiment name.
#' @param date A string or Date. Defaults to today's date.
#' @param experiment_notes A string of notes. Default is "no notes".
#' @param location A string for experiment location: "ETH" or "PNG" accepted.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#' @param path Output path for the PDF file. Defaults to current working directory.
#'
#' @return Rendered PDF report.
#' @export
#'
#' @importFrom dplyr filter select all_of rename
#' @importFrom knitr kable asis_output
#' @importFrom rmarkdown render
#' @importFrom here here
#' @importFrom kableExtra kable_styling
#' @importFrom janitor clean_names
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' ## Not run on CRAN because it requires interactive rendering and can be slow:
#' \dontrun{
#'   # Example raw data files (MAGPIX platform)
#'   your_raw_data <- c(
#'     system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'     system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'     system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#'   )
#'
#'   # Example plate layout file
#'   your_plate_layout <- system.file(
#'     "extdata",
#'     "example_platelayout_1.xlsx",
#'     package = "SeroTrackR"
#'   )
#'
#'   # Generate the QC PDF report
#'   renderQCReport(
#'     raw_data     = your_raw_data,
#'     plate_layout = your_plate_layout,
#'     platform     = "magpix",
#'     location     = "ETH"
#'   )
#' }
renderQCReport <- function(
    raw_data,
    plate_layout,
    platform,
    experiment_name = "experiment1",
    date = format(Sys.Date(), "%Y%m%d"), # default to today's date
    experiment_notes = "no notes",
    location,
    std_point = 10,
    path = "." # Default to current working directory
  ) {

  ###############################################################################
  # ----- Load Data functions -----
  ###############################################################################

  sero_data                 <- readSeroData(raw_data, platform)
  raw_data_info             <- sero_data$data_raw %>% janitor::clean_names()
  raw_data_filename         <- tolower(basename(raw_data))
  plate_list                <- readPlateLayout(plate_layout, sero_data)
  version                   <- getGithubRelease("dionnecargy", "PvSeroApp")

  qc_results                <- runQC(sero_data, plate_list)
  if(location == "ETH"){
    mfi_to_rau_output       <- suppressMessages(MFItoRAU_Adj(sero_data, plate_list, qc_results, std_point, project = NULL))
  } else if(location == "PNG"){
    mfi_to_rau_output       <- suppressWarnings(MFItoRAU(sero_data, plate_list, qc_results, std_point, project = NULL))
  }

  stdcurve_plot             <- suppressWarnings(plotStds(sero_data, location, experiment_name))
  plateqc_plot              <- plotCounts(qc_results, experiment_name)
  check_repeats_output      <- getRepeats(qc_results, plate_list)
  blanks_plot               <- plotBlanks(sero_data, experiment_name)
  if(location == "ETH"){
    model_plot              <- plotModel_Adj(mfi_to_rau_output, sero_data)
  } else if(location == "PNG"){
    model_plot              <- plotModel(mfi_to_rau_output, sero_data)
  }

  ###############################################################################
  # ----- Create helper functions -----
  ###############################################################################

  operator_output <- function() {
    if (platform %in% c("magpix", "intelliflex")) {
      op <- raw_data_info %>%
        dplyr::filter(program == "Operator") %>%
        dplyr::select(plate, Operator = x_ponent)
      paste(paste0(op$plate, ": ", op$Operator), collapse = ", ")
    } else {
      return("Information not available for bioplex machine run.")
    }
  }

  volume_output <- function() {
    if (platform == "magpix") {
      vol <- raw_data_info %>%
        dplyr::filter(program == "SampleVolume") %>%
        dplyr::select(plate, `Acquisition Volume` = x_ponent)
      paste(paste0(vol$plate, ": ", vol$`Acquisition Volume`), collapse = ", ")
    } else if (platform == "intelliflex") {
      vol <- raw_data_info %>%
        dplyr::filter(program == "MaxSampleUptakeVolume") %>%
        dplyr::select(plate, `Acquisition Volume` = x_ponent)
      paste(paste0(vol$plate, ": ", vol$`Acquisition Volume`), collapse = ", ")
    } else {
      return("Information not available for bioplex machine run.")
    }
  }

  calibration_output <- function() {
    if (platform == "magpix") {
      calib <- raw_data_info %>%
        dplyr::filter(program %in% c("Last CAL Calibration", "Last VER Verification", "Last Fluidics Test")) %>%
        dplyr::select(plate, program, Result = x_ponent)
      paste(paste0(calib$plate, ": ", calib$program, ": ", calib$Result), collapse = ", ")
    } else if (platform == "intelliflex") {
      calib <- raw_data_info %>%
        dplyr::filter(program %in% c("Last Calibration", "Last Verification", "Last Fluidics Test")) %>%
        dplyr::select(plate, program, Result = x_ponent)
      paste(paste0(calib$plate, ": ", calib$program, ": ", calib$Result), collapse = ", ")
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
      filtered_df <- raw_data_info %>% dplyr::select(dplyr::all_of(matching_cols))
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
      machine_levels <- unique(raw_data_info$plate)
      paste(paste0(machine_levels, ": ", machine$`Machine Serial Number`), collapse = ", ")

    } else if (platform == "bioplex") {

      machine <- raw_data_info %>%
        dplyr::filter(str_detect(run_column, "Reader Serial Number")) %>%
        dplyr::mutate(run_column = gsub("Reader Serial Number: ", "", run_column)) %>%
        dplyr::select(run_column)
      machine_levels <- unique(raw_data_info$plate)
      paste(paste0(machine_levels, ": ", machine$run_column), collapse = ", ")

    } else if (platform == "intelliflex") {
      machine <- raw_data_info %>%
        dplyr::filter(program == "SN") %>%
        dplyr::select(plate, `Machine Serial Number` = x_ponent)
      paste(paste0(machine$plate, ": ", machine$`Machine Serial Number`), collapse = ", ")

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
      output_dir = here::here(path),
      params = list(
        raw_data_filename        = raw_data_filename,
        experiment_name          = experiment_name,
        date                     = date,
        experiment_notes         = experiment_notes,
        platform                 = platform,
        stdcurve_plot            = stdcurve_plot,
        plateqc_plot             = plateqc_plot,
        blanks_plot              = blanks_plot,
        check_repeats_output     = check_repeats_output,
        check_repats_table_pdf   = check_repats_table_pdf(check_repeats_output),
        model_plot               = model_plot,
        operator_output          = operator_output(),
        volume_output            = volume_output(),
        calibration_output       = calibration_output(),
        machine_output           = machine_output(),
        plate_list_output        = plate_list_output()
      )
    )
  )

}
