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

  # Case 1: folder_path is length 1 and it's a folder: search for "layout" files
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
    # Case 2: folder_path is a vector of file paths: skip search
    layout_files <- folder_path
  }

  # Initialise list
  plate_list_all <- list()

  for (file in layout_files) {
    # Get sheet names
    sheet_names <- openxlsx::getSheetNames(file)

    for (sheet in sheet_names) {
      df <- openxlsx::read.xlsx(file, sheet = sheet)

      # Excel sheet names must be ≤31 characters
      safe_sheet_name <- substr(sheet, 1, 31)

      # Handle duplicate sheet names across files
      if (safe_sheet_name %in% names(plate_list_all)) {
        counter <- 1
        new_name <- paste0(substr(sheet, 1, 28), "_", counter)
        while (new_name %in% names(plate_list_all)) {
          counter <- counter + 1
          new_name <- paste0(substr(sheet, 1, 28), "_", counter)
        }
        safe_sheet_name <- new_name
      }

      # Add to master list
      plate_list_all[[safe_sheet_name]] <- df
    }
  }

  # Define output file path if not provided
  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".xlsx")
  }

  # Write to file
  openxlsx::write.xlsx(plate_list_all, file = output_file, colNames = TRUE)

  # Optionally return both file path and list
  list(
    path = output_file,
    data = plate_list_all
  )

}
