#' Find and create a master plate layout file
#'
#' Join multiple a plate layout files into one master file with multiple tabs
#'
#' @param folder_path A string containing your main folder for your project. Default = current working directory.
#'
#' @returns An .xlsx file saved to your current working directory with multiple tabs, one tab for each plate layout.
#' @export
#'
#' @importFrom openxlsx getSheetNames read.xlsx write.xlsx
#'
#' @author Dionne Argyropoulos
getPlateLayout <- function(folder_path = getwd()) {
  # Find all .xlsx files with "layout" in the name, recursively
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

  # Initialise list
  plate_list_all <- list()

  for (file in layout_files) {
    # Get sheet names
    sheet_names <- openxlsx::getSheetNames(file)

    # Read each sheet and assign sheet name (truncated to 31 chars if needed)
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

  # Write to Excel
  output_file <- file.path(getwd(), "plate_layout_all.xlsx")
  openxlsx::write.xlsx(plate_list_all, file = output_file, colNames = TRUE)
  message("File saved at: ", output_file)

}
