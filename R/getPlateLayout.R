#' Find and create a master plate layout file
#'
#' Join multiple a plate layout files into one master file with multiple tabs
#'
#' @param folder_path A string containing your main folder for your project or the plate layout files. Default = current working directory.
#' @param output_file A string for the path for your output master file.
#'
#' @returns An .xlsx file saved to your current working directory with multiple tabs, one tab for each plate layout.
#' @export
#'
#' @importFrom openxlsx getSheetNames read.xlsx write.xlsx
#'
#' @author Dionne Argyropoulos
getPlateLayout <- function(folder_path = getwd(), output_file = NULL) {

  # Case 1: folder_path is length 1 and it's a folder
  if (length(folder_path) == 1 && dir.exists(folder_path)) {
    layout_files <- list.files(
      path = folder_path,
      pattern = "layout.*\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(layout_files) == 0) {
      stop("No layout Excel files found in the specified folder.")
    }
  } else {
    layout_files <- folder_path
  }

  plate_list_all <- list()

  for (file in layout_files) {
    sheet_names <- openxlsx::getSheetNames(file)

    for (sheet in sheet_names) {
      df <- openxlsx::read.xlsx(file, sheet = sheet)

      # Instead of renaming duplicates, enforce identical names to Antigen data
      if (sheet %in% names(plate_list_all)) {
        stop(sprintf(
          "Duplicate plate name detected: '%s' in file '%s'.
           Please rename sheets so each plate name is unique across all files.",
          sheet, file
        ))
      }

      plate_list_all[[sheet]] <- df
    }
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".xlsx")
  }

  openxlsx::write.xlsx(plate_list_all, file = output_file, colNames = TRUE)

  list(
    path = output_file,
    data = plate_list_all
  )
}

