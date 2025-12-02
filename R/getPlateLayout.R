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
#'
#' @examples
#' \donttest{
#' # Example 1: Create two example 96-well plates in-memory
#' create_plate <- function(plate_name) {
#'   rows <- LETTERS[1:8]
#'   cols <- 1:12
#'   df <- data.frame(plate = rows)
#'   for (col in cols) {
#'     df[[as.character(col)]] <- paste0(rows, col)
#'   }
#'   df$plate_id <- plate_name
#'   df
#' }
#'
#' plate1 <- create_plate("Plate1")
#' plate2 <- create_plate("Plate2")
#'
#' # Combine plates into a list to simulate getPlateLayout() output
#' master_layout <- list(
#'   path = tempfile(fileext = ".xlsx"),  # placeholder path
#'   data = list(Plate1 = plate1, Plate2 = plate2)
#' )
#'
#' # The returned list contains:
#' # 1. path: the file path to the (simulated) master Excel file
#' # 2. data: a list of data.frames, one per plate
#' names(master_layout$data)  # View sheet names
#'
#' # Example 2: Access individual plates directly
#' layout_files <- list(plate1, plate2)  # simulate individual Excel sheets
#'
#' master_layout2 <- list(
#'   path = tempfile(fileext = ".xlsx"),  # placeholder path
#'   data = setNames(layout_files, c("Plate1", "Plate2"))
#' )
#'
#' # View the resulting plate names
#' names(master_layout2$data)
#' }
getPlateLayout <- function(folder_path = getwd(), output_file = NULL) {

  # Case 1: folder_path is length == 1 and is a folder
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

  # Case 2: folder_path is length > 1 and contains file names
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

  final_plate_list <- list(
    path = output_file,
    data = plate_list_all
  )

  return(final_plate_list)
}

