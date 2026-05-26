#' Read Raw Serological Data
#'
#' This function imports the raw data from the Magpix or Bioplex machine
#' and matches the sample names from the plate layout based on their plate/well
#' location.
#'
#' @param raw_data String with the raw data path.
#' @param raw_data_filenames String with the raw data filename path.
#' Default is NA as it can be deduced from raw_data. Needs to be a parameter for the PvSeroApp.
#' @param version xPONENT software version. For "magpix" can be 4.2 or 4.3. Default: 4.2.
#' @param platform "magpix", "bioplex" or "intelliflex".
#'
#' @return List of data frames: (i) raw data output, (ii) cleaned all results
#' (iii) count data, (iv) blanks only, (v) standards only, (vi) run
#' information.
#'
#' @export
#'
#' @author Dionne Argyropoulos, Shazia Ruybal-Pesantez
#'
#' @examples
#' # Example raw data files (MAGPIX platform)
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' # Read and combine raw serology data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
readSeroData <- function(raw_data, platform, version = "4.2", raw_data_filenames = NULL){
  platemap_file <- system.file("extdata", "platemap.csv", package = "SeroTrackR")
  platemap <- read.csv(platemap_file)

  raw_data_filenames <- tolower(
    if (is.null(raw_data_filenames)) basename(raw_data) else raw_data_filenames
  )

  # Initialise master list to store files
  master_list <- list(
    data_raw  = NULL,  # Placeholder for raw data combined across files
    results   = NULL,  # Placeholder for processed results combined
    counts    = NULL,  # Placeholder for any count data combined
    blanks    = NULL,  # Placeholder for any blanks data combined
    stds      = NULL,  # Placeholder for any stds data combined
    run       = NULL   # Placeholder for any run data combined
  )

  # Loop through each file and process accordingly
  for (i in seq_along(raw_data)) {
    file <- raw_data[i]
    file_name <- raw_data_filenames[i]

    if (.check_platform(file, platform, file_name) == TRUE) {
      message("PASS: File ", file_name, " successfully validated.")
    }

    if (platform == "magpix") {

      cfg         <- .magpix_version_config(version)
      df          <- .read_luminex_file(file)
      sections    <- .extract_luminex_sections(df, cfg, "magpix")
      master_list <- .post_process_luminex(sections, file_name, master_list)

    } else if (platform == "bioplex"){

      df          <- .read_luminex_file(file)
      sections    <- .post_process_bioplex(df)
      master_list <- .post_process_luminex(sections, file_name, master_list)

    } else if (platform == "intelliflex"){

      df          <- .read_luminex_file(file)
      sections    <- .extract_luminex_sections(df, cfg, "intelliflex")
      master_list <- .post_process_luminex(sections, file_name, master_list)

    } else {
      stop(
        paste0(
          "Unsupported platform: '", platform, "'.\n",
          "  Supported platforms are: 'magpix', 'bioplex', or 'intelliflex'.\n",
          "  Please check your platform argument and try again."
        ),
        call. = FALSE
      )
    }

  }

  return(master_list)

}
#' Check Platform
#'
#' This function checks the platform the user has input and whether it aligns
#' with the correct format as expected. Will report error if NOT aligned.
#'
#' @param raw_data String with the raw data path.
#' @param platform "magpix", "bioplex" or "intelliflex".
#' @param file_name String with the raw data filename (for error messaging).
#'
#' @return TRUE: if platform == file format, ERROR message when platform does
#' not equal file format.
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom janitor row_to_names
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
#' .check_platform(raw_data = your_raw_data, platform = "magpix", file_name = basename(your_raw_data))
.check_platform <- function(raw_data, platform, file_name) {

  if (length(raw_data) == 0) {
    stop("No raw data files were provided.", call. = FALSE)
  }

  # Read file
  df <- .read_luminex_file(raw_data)

  # Extract the first two column names
  col_names <- colnames(df)
  if (all(grepl("^X\\d+$", col_names))) {
    df <- suppressWarnings(df %>% janitor::row_to_names(row_number = 1))
  }
  first_two_cols <- colnames(df)[1:2]

  # Detect if the file is Magpix based on column names
  is_magpix <- any(grepl("Program|row", first_two_cols, ignore.case = TRUE)) ||
    any(grepl("xPonent|col", first_two_cols, ignore.case = TRUE))

  # Detect if file is Bioplex (inverse of Magpix detection)
  is_bioplex <- !is_magpix

  # User selected "magpix" but the file does not have "Program" or "xPonent"
  if (platform == "magpix" && !is_magpix) {
    stop(
      paste0(
        "Platform mismatch for file '", file_name, "':\n",
        "  You specified platform = 'magpix', but this file appears to be 'bioplex' or 'intelliflex'.\n",
        "  Please try platform = 'bioplex' or platform = 'intelliflex'.\n",
        "  MagPix files contain columns like 'xPONENT', 'Program', or 'row'."
      ),
      call. = FALSE
    )
  }

  # User selected "bioplex" but the file contains "Program" or "xPonent"
  if (platform == "bioplex" && is_magpix) {
    stop(
      paste0(
        "Platform mismatch for file '", file_name, "':\n",
        "  You specified platform = 'bioplex', but this file appears to be 'magpix'.\n",
        "  Please try platform = 'magpix'.\n",
        "  Bioplex files do not contain 'xPONENT' or 'Program' columns."
      ),
      call. = FALSE
    )
  }

  # User selected "intelliflex" but file is Magpix
  if (platform == "intelliflex" && is_magpix) {
    stop(
      paste0(
        "Platform mismatch for file '", file_name, "':\n",
        "  You specified platform = 'intelliflex', but this file appears to be 'magpix'.\n",
        "  Please try platform = 'magpix'.\n",
        "  Intelliflex files have a different structure than Magpix files."
      ),
      call. = FALSE
    )
  }

  return(TRUE)

}
#' Helper function to read raw luminex files
#'
#' @param file String with the raw data path.
#'
#' @returns raw data frame
#' @export
#'
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom utils read.csv2 count.fields
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
#' df <- .read_luminex_file(your_raw_data)
.read_luminex_file <- function(file) {
  ext <- tools::file_ext(file)

  if (ext == "xlsx") {
    df <- suppressMessages(readxl::read_excel(file)) %>% as.data.frame()
  } else if (ext == "csv") {
    first_lines <- readLines(file, n = 5)
    df <- if (any(grepl(";", first_lines))) {
      suppressWarnings(read.csv2(file, header = F, col.names = paste0("x", 1:max(count.fields(file, sep = ";"),na.rm = T)), fill = T)%>%
                         janitor::row_to_names(1))
    } else {
      suppressWarnings(read.csv(file, header = F, col.names = paste0("x", 1:max(count.fields(file, sep = ","),na.rm = T)), fill = T) %>%
                         janitor::row_to_names(1))
    }
    colnames(df) <- make.names(colnames(df), unique = T)
    df <- dplyr::filter(df, rowSums(is.na(df)) != ncol(df))
  } else {
    stop("Unsupported file format! Please use .csv or .xlsx", call. = FALSE)
  }

  df
}
#' Relabel column names to Standardised Naming Convention
#'
#' This is a helper function to be used inside `readSeroData()` to relabel
#' columns for each plate.
#'
#' @param df  Data frame from `readSeroData()` processing.
#'
#' @returns A data fame with columns renamed
#' @export
#'
#' @importFrom stringr str_detect
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
#'
#' if (
#'  requireNamespace("dplyr", quietly = TRUE) &&
#'  requireNamespace("janitor", quietly = TRUE)
#' ) {
#'
#'   # Read in raw luminex file
#'   df <- .read_luminex_file(your_raw_data)
#'
#'   # Get the start and end rows of the data section: start = "Median", end = "Net MFI"
#'   row1    <- which(df$xPONENT == "Median")
#'   row2    <- which(df$xPONENT == "Net MFI")
#'
#'   # Apply data processing pipeline, including .relabel_columns()
#'   df |>
#'     dplyr::slice((row1 + 1):(row2 - 1)) |>
#'     janitor::row_to_names(row_number = 1) |>
#'     janitor::clean_names() |>
#'     dplyr::select(dplyr::where(~ !all(is.na(.x)))) |>
#'     dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) |>
#'     dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN", 0, .))) |>
#'     .relabel_columns()
#' }
#'}
.relabel_columns <- function(df) {
  colnames(df) <- dplyr::case_when(
    stringr::str_detect(colnames(df), regex("EBP", ignore_case = TRUE)) ~ "EBP",
    stringr::str_detect(colnames(df), regex("(LF005|Pv.fam.a|fam.a|Pv-fam-a)", ignore_case = TRUE)) ~ "LF005",
    stringr::str_detect(colnames(df), regex("(LF010|MSP5)", ignore_case = TRUE)) ~ "LF010",
    stringr::str_detect(colnames(df), regex("(LF016|PvMSP1-19|PvMSP1.19)", ignore_case = TRUE)) ~ "LF016",
    stringr::str_detect(colnames(df), regex("(MSP8|L34)", ignore_case = TRUE)) ~ "MSP8",
    stringr::str_detect(colnames(df), regex("(P87|RBP2b-P87|RBP2b|PvRBP2b)", ignore_case = TRUE)) ~ "RBP2b.P87",
    stringr::str_detect(colnames(df), regex("(PTEX|PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150",
    stringr::str_detect(colnames(df), regex("PkTRAMPCSS|PkTRAMP-CSS|PkPC", ignore_case = TRUE)) ~ "PkTRAMP-CSS",
    stringr::str_detect(colnames(df), regex("CSS", ignore_case = TRUE)) ~ "PvCSS",
    stringr::str_detect(colnames(df), regex("(PfMSP1-19|PfMSP1|PfMSP1.19)", ignore_case = TRUE)) ~ "PfMSP1-19",
    stringr::str_detect(colnames(df), regex("PfAMA1", ignore_case = TRUE)) ~ "PfAMA1",
    stringr::str_detect(colnames(df), regex("Pfetramp5Ag1|Pfetramp", ignore_case = TRUE)) ~ "Pfetramp5Ag1",
    stringr::str_detect(colnames(df), regex("HSP40Ag1", ignore_case = TRUE)) ~ "PfHSP40Ag1",
    stringr::str_detect(colnames(df), regex("PfGexp18", ignore_case = TRUE)) ~ "PfGexp18",
    stringr::str_detect(colnames(df), regex("PkSSP2", ignore_case = TRUE)) ~ "PkSSP2",
    stringr::str_detect(colnames(df), regex("PkMSP10", ignore_case = TRUE)) ~ "PkMSP10",
    stringr::str_detect(colnames(df), regex("PkRIPR", ignore_case = TRUE)) ~ "PkRIPR",
    stringr::str_detect(colnames(df), regex("Sera3ag1", ignore_case = TRUE)) ~ "PkSERA3ag1",
    stringr::str_detect(colnames(df), regex("Pk8", ignore_case = TRUE)) ~ "Pk8",
    stringr::str_detect(colnames(df), regex("SERA3Ag2", ignore_case = TRUE)) ~ "PkSERA3Ag2",
    TRUE ~ colnames(df) # Keep unmatched names as-is
  )
  return(df)
}
#' Helper function to identify Magpix version
#'
#' @param version String with the raw data path.
#'
#' @returns specific column names for filtering for xPONENT software v4.2 and v4.3
#' @export
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' version = "4.2"
#' .magpix_version_config(version)
#'
.magpix_version_config <- function(version) {
  switch(
    version,
    "4.2" = list(end_count = "Avg Net MFI"),
    "4.3" = list(end_count = "Result"),
    stop("Unsupported MagPix version", call. = FALSE)
  )
}
#' Helper function to process luminex sections
#'
#' @param df  String with the raw data path.
#' @param cfg Magpix version output of .magpix_version_config().
#' @param plt Platform (magpix, intelliflex)
#'
#' @returns List of data_raw, results, counts, blanks, stds, run
#' @export
#'
#' @importFrom dplyr slice select filter if_any everything  where as_tibble
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
#' df            <- .read_luminex_file(your_raw_data)
#' cfg           <- .magpix_version_config("4.2")
#' section       <- .extract_luminex_sections(df, cfg, "magpix")
.extract_luminex_sections <- function(df, cfg, plt) {

  if (plt == "magpix") {

    median_row_number    <- which(df$xPONENT == "Median")
    endmedian_row_number <- which(df$xPONENT == "Net MFI")
    count_row_number     <- which(df$xPONENT == "Count")
    endcount_row_number  <- which(df$xPONENT == cfg$end_count)

  } else if (plt == "intelliflex") {

    median_row_number     <- which(df$xPONENT == "Median")
    endmedian_row_number  <- which(df$xPONENT == "Mean")
    count_row_number      <- which(df$xPONENT == "Count")
    endcount_row_number   <- which(df$xPONENT == "%CV")
  }

  results <- .clean_luminex(df, median_row_number, endmedian_row_number)
  counts  <- .clean_luminex(df, count_row_number, endcount_row_number)

  run <- df %>%
    dplyr::slice(1:median_row_number) %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x)))) %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x)))

  blanks <- results %>%
    dplyr::filter(grepl("Blank|^B$", Sample, ignore.case = TRUE))

  stds <- results %>%
    dplyr::filter(grepl("^S", Sample, ignore.case = TRUE))

  if (nrow(blanks) == 0) stop("No blanks were found.", call. = FALSE)
  if (nrow(stds) == 0)   stop("No standards were found.", call. = FALSE)

  list(
    data_raw = df,
    results  = results,
    counts   = dplyr::as_tibble(counts),
    blanks   = blanks,
    stds     = stds,
    run      = run
  )
}
#' Helper function to process luminex (Magpix/Intelliflex) data
#'
#' @param df Raw luminex file
#' @param row1 Leading row to subset
#' @param row2 Final row to subset
#'
#' @returns Cleaned data fame
#' @export
#'
#' @importFrom dplyr slice select filter if_any across everything across any_of mutate
#' @importFrom stringr str_remove
#' @importFrom janitor row_to_names
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")
#' df            <- .read_luminex_file(your_raw_data)
#' cfg           <- .magpix_version_config("4.2")
#'
#' row1         <- which(df$xPONENT == "Median")
#' row2         <- which(df$xPONENT == "Net MFI")
#'
#' results      <- .clean_luminex(df, row1, row2)
#'
#' }
.clean_luminex <- function(df, row1, row2){

  df2 <- df %>%
    # Filter to correct section of df
    dplyr::slice((row1 + 1):(row2 - 1)) %>%
    #make blank cells NA so they are dropped in next line
    mutate(across(.cols = where(is.character), .fns = ~na_if(.x, ""))) %>%
    # Drop all-NA columns
    dplyr::select(dplyr::where(~ !all(is.na(.x)))) %>%
    # Drop all-NA rows
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) %>%
    # Change all NaNs to 0s
    dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN", 0, .))) %>%
    # Remove first row
    janitor::row_to_names(row_number = 1) %>%
    # Make all numeric columns numeric
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ all(is.na(.x) | grepl("^[-+]?[0-9]*\\.?[0-9]+$", .x))),
        as.numeric
      )
    ) %>%
    # Use custom function to ensure columns are SeroTrackR friendly
    .relabel_columns() %>%
    # Remove Total Events column
    dplyr::select(-dplyr::any_of("Total Events")) %>%
    # Relabel Blanks and Samples
    dplyr::mutate(
      # Sequentially relabel Blank rows and keep other Sample values unchanged
      Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()),
                      ifelse(Sample == "B", paste0("Blank", row_number()), Sample)),
      # Sequentially relabel Sample rows and keep other Sample values
      Sample = ifelse(Sample == "S", paste0("S", cumsum(Sample == "S")), Sample)
    )

  return(df2)
}
#' Helper function to process bioplex data
#'
#' @param df Output from `.read_luminex.file()`
#'
#' @returns Cleaned data fame
#' @export
#'
#' @importFrom dplyr rename rename_with slice select filter across any_of bind_rows left_join arrange mutate if_else
#' @importFrom stringr str_remove str_replace_all str_remove_all str_sub
#' @importFrom janitor row_to_names
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#' your_raw_data <- system.file("extdata", "example_BioPlex_plate1.xlsx", package = "SeroTrackR")
#' df            <- .read_luminex_file(your_raw_data)
#' results       <- .clean_bioplex(df)
.clean_bioplex <- function(df){

  platemap      <- read.csv(system.file("extdata", "platemap.csv", package = "SeroTrackR"))
  col_name <- colnames(df)[2]  # second column
  df2 <- df %>%
    # Rename first column
    dplyr::rename(Run = 1) %>%
    # Drop rows before "Type"
    dplyr::slice(which(.data[[col_name]] == "Type") : dplyr::n()) %>%
    # Promote first row to header
    janitor::row_to_names(row_number = 1) %>%
    # Clean column names (remove "(...)")
    dplyr::rename_with(~ stringr::str_remove(.x, "\\s*\\(.*\\)")) %>%
    # Helper function to relabel to be compatible with SeroTrackR
    .relabel_columns() %>%
    # Recode + ordering helpers so that standards and blanks are at the top
    dplyr::mutate(
      Type   = dplyr::if_else(Type == "B", "Blank", Type),
      suffix = as.numeric(stringr::str_remove_all(Type, "\\D")),
      prefix = stringr::str_sub(Type, 1, 1)
    ) %>%
    # Order so that standards and blanks are at the top
    dplyr::arrange(prefix, suffix) %>%
    # Join platemap
    dplyr::left_join(platemap, by = "Well") %>%
    # Drop unwanted columns if present
    dplyr::select(
      -dplyr::any_of(c(
        "prefix", "suffix", "Region", "Gate", "Total",
        "% Agg Beads", "Sampling Errors", "Well", "Description"
      ))
    ) %>%
    # Align with MagPix schema
    dplyr::select(Location, Sample = Type, everything()) %>%
    # Clean bad values + relabel blanks
    dplyr::mutate(
      # change "NaN" to 0s and "***" to 0s
      dplyr::across(dplyr::everything(), ~ stringr::str_replace_all(.x, c("NaN" = "0", "\\*\\*\\*" = "0"))),
      # Sequentially relabel Blank rows and keep other
      Sample = dplyr::if_else(
        Sample == "Blank",
        paste0("Blank", row_number()),
        Sample
      )
    )
  return(df2)
}
#' Helper function to process bioplex sections
#'
#' @param df Output from `.read_luminex_file()`
#'
#' @returns List of data_raw, results, counts, blanks, stds, run
#' @export
#'
#' @importFrom dplyr mutate across  where filter slice select
#'
#' @author Dionne Argyropoulos
#'
#' your_raw_data <- system.file("extdata", "example_BioPlex_plate1.xlsx", package = "SeroTrackR")
#' df            <- .read_luminex_file(your_raw_data)
#' sections      <- .post_process_bioplex(df)
.post_process_bioplex <- function(df){

  results <- .clean_bioplex(df) %>%
    dplyr::mutate(dplyr::across(-c(Location, Sample), ~ gsub("\\s*\\(.*\\)", "", .))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ all(is.na(.x) | grepl("^[-+]?[0-9]*\\.?[0-9]+$", .x))),
        as.numeric
      )
    )
  counts  <- .clean_bioplex(df) %>%
    dplyr::mutate(dplyr::across(-c(Location, Sample), ~ gsub(".*\\((.*)\\).*", "\\1", .))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ all(is.na(.x) | grepl("^[-+]?[0-9]*\\.?[0-9]+$", .x))),
        as.numeric
      )
    )

  blanks <- results %>% dplyr::filter(grepl("Blank", Sample, ignore.case = TRUE))

  stds <- results %>% dplyr::filter(grepl("^S", Sample, ignore.case = TRUE))

  colnames(df)[1] <- "RunColumn"
  well_row <- which(df$RunColumn == "Well")[1]
  run <- df %>%
    dplyr::slice(1:(well_row - 2)) %>%
    dplyr::select(RunColumn)


  if (nrow(blanks) == 0) stop("No blanks were found.", call. = FALSE)
  if (nrow(stds) == 0)   stop("No standards were found.", call. = FALSE)

  list(
    data_raw = df,
    results  = results,
    counts   = dplyr::as_tibble(counts),
    blanks   = blanks,
    stds     = stds,
    run      = run
  )

}
#' Helper function to process luminex into master_list
#'
#' @param sections Output from `.post_process_bioplex()`.
#' @param file_name User input file name.
#' @param master_list Intermediary df from `readSeroData()`.
#'
#' @returns List of data_raw, results, counts, blanks, stds, run
#' @export
#'
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows mutate
#'
#' @author Dionne Argyropoulos
.post_process_luminex <- function(sections, file_name, master_list) {

  plate <- stringr::str_extract(file_name, "(?i)(repeat)?plate\\d+(?=[._-]|$)")

  add_plate <- function(x) dplyr::mutate(x, Plate = plate)

  master_list$data_raw <- dplyr::bind_rows(master_list$data_raw, add_plate(sections$data_raw))
  master_list$results  <- dplyr::bind_rows(master_list$results,  add_plate(sections$results))
  master_list$counts   <- dplyr::bind_rows(master_list$counts,   add_plate(sections$counts))
  master_list$blanks   <- dplyr::bind_rows(master_list$blanks,   add_plate(sections$blanks))
  master_list$stds     <- dplyr::bind_rows(master_list$stds,     add_plate(sections$stds))
  master_list$run      <- dplyr::bind_rows(master_list$run,      add_plate(sections$run))

  return(master_list)
}
