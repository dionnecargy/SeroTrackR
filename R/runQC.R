#' Run Quality Control Pipeline
#'
#' A master function containing each quality control processing step.
#'
#' @param sero_data Output from `readSeroData()`.
#' @param plate_list Output from `readPlateLayout()`.
#'
#' @returns processCounts_output, getCounts_output, sampleid_output, getAntigenCounts_output, getCountsQC_output
#' @export
#'
#' @author Dionne Argyropoulos
#'
#' @examples
#'
#' \donttest{
#' # Example data supplied with the package
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' your_plate_layout <- system.file(
#'   "extdata", "example_platelayout_1.xlsx", package = "SeroTrackR"
#' )
#'
#' # Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Run full pipeline including classification
#' runQC(
#'   sero_data = sero_data,
#'   plate_list = plate_list
#' )
#' }
runQC <- function(sero_data, plate_list){

  # Quality Control and MFI to RAU
  processCounts_output      <- processCounts(sero_data)
  getCounts_output          <- getCounts(processCounts_output)
  sampleid_output           <- getSampleID(processCounts_output, plate_list)
  getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
  getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)

  qc_outputs <- list(
    processCounts_output = processCounts_output,
    getCounts_output = getCounts_output,
    sampleid_output = sampleid_output,
    getAntigenCounts_output = getAntigenCounts_output,
    getCountsQC_output = getCountsQC_output
  )

  return(qc_outputs)
}
#' Process Counts from Raw Serological Data file
#'
#' A helper function to process counts data.
#'
#' @param sero_data Output from `readSeroData()`.
#' @return Returns a long table of counts with "Warning" category (<15 == 1 and
#' \eqn{>=}  15 == 0) for downstream wrangling.
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr pivot_longer
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' # Example demonstrating how to process bead count data.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' # Read in raw MAGPIX data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
#'
#' # Process counts
#' processed_master <- processCounts(sero_data = sero_data)
#'
#' }
#'
processCounts <- function(sero_data){

  # 1. Store Counts Data
  counts_data <- sero_data$counts

  # 2. Data Wrangling
  counts_data <- counts_data %>%
    dplyr::mutate(Location=gsub(".*,", "", Location)) %>%
    dplyr::mutate(Location=substr(Location, 1, nchar(Location)-1))  %>%
    tidyr::pivot_longer(-c(Sample, Location, Plate), names_to = "Antigen", values_to = "Count") %>%
    dplyr::mutate(Warning = case_when(
      # Bead count below 15: warning
      as.numeric(Count)<15~1,
      # Bead count above or equal to 15: good
      as.numeric(Count)>=15~0,
      # Bead count above 500: warning (impossible: 500 is theoretical maximum per antigen)
      as.numeric(Count)>500~1,
    ))

  return(counts_data)
}
#' Get Count Data from Raw Median Fluorescent Intensity
#'
#' This function obtains the count data from the raw Median Fluorescent
#' Intensity (MFI). This is an interim function used for the plotCounts
#' function. This function relies on the `readAntigens` and `readSeroData` data
#' processing functions.
#'
#' @param processed_counts Output from `processCounts()`.
#' @return (i) Data frame providing bead counts per well per plate. (ii)
#' Designates whether wells should be repeated if there are \eqn{<=}  15 beads (repeat)
#' or if they are sufficient with > 15 beads (sufficient beads).
#' @export
#' @importFrom dplyr select group_by summarise mutate
#' @author Shazia Ruybal-Pesántez, Dionne Argyropoulos
#' @examples
#' \donttest{
#'
#' # Step 0: Load example raw data
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Step 1: Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Step 2: Process counts and perform quality control
#' counts      <- processCounts(sero_data)
#' counts_raw  <- getCounts(counts)
#' }
getCounts <- function(processed_counts){

  counts <- processed_counts %>%
    dplyr::select(Location, Warning, Plate) %>%
    dplyr::group_by(Location, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Row = as.factor(substr(Location, 1, nchar(Location)-1)),
      Row = gsub("1", "", Row),
      Col = as.numeric(substr(Location, 2, nchar(Location))),
      QC_total = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )

  return(counts)
}
#' Get SampleID from Plate Layout
#'
#' A helper function to extract Sample ID based on plate name and row/col
#'
#' @param processed_counts Output from `processCounts()`.
#' @param plate_list Plate name inside of the plate layout file.
#' @return Returns the corresponding Sample ID for the correct row/column in
#' the plate layout file. Henceforth "Sample ID" refers to the code in the
#' plate layout file, while "Sample" is the code in the Luminex file.
#' @export
#' @importFrom dplyr left_join select mutate bind_rows across
#' @importFrom tidyselect matches
#' @importFrom tidyr pivot_longer
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' # Step 0: Load example raw data
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Step 1: Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Step 2: Process counts and perform quality control
#' counts      <- processCounts(sero_data)
#' counts_raw  <- getCounts(counts)
#' sample_ids  <- getSampleID(counts, plate_list)
#' }
getSampleID <- function(processed_counts, plate_list) {
  plate_layout_longer <- list()

  for (plate_level in seq_along(plate_list)) {

    # Get plate name (or fallback to index)
    plate_name <- names(plate_list)[plate_level]
    if (is.null(plate_name) || plate_name == "") {
      plate_name <- as.character(plate_level)
    }

    # Read and wrangle Plate i
    plate_layout <- plate_list[[plate_level]]
    names(plate_layout)[1] <- "Row"

    plate_layout_level <- plate_layout %>%
      dplyr::mutate(dplyr::across(tidyselect::matches("^[0-9]+$"), as.character)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::matches("^[.x]*[0-9]+$"),
        names_to = "Col",
        values_to = "SampleID"
      ) %>%
      dplyr::mutate(
        Location = paste0(Row, Col),
        Plate = plate_name  # Add Plate info here
      )

    # Save to list
    plate_layout_longer[[plate_level]] <- plate_layout_level
  }

  # Combine all into a single data frame
  plate_layout_longer_df <- dplyr::bind_rows(plate_layout_longer) %>%
    dplyr::mutate(Plate = factor(Plate))  # Make Plate a factor

  # Join to antigen_specific_df
  final_table <- plate_layout_longer_df %>%
    dplyr::left_join(processed_counts, by = c("Location", "Plate")) %>%
    dplyr::select(-c(Row, Col))

  return(final_table)

}
#' Get Count Data for each Antigen from the Raw Median Fluorescent Intensity
#'
#' This function obtains the count data from the raw Median Fluorescent
#' Intensity (MFI). This function relies on the `readAntigens` and
#' `readSeroData` data processing functions.
#'
#' @param processed_counts Output from `processCounts()`.
#' @param plate_list Output from `readPlateLayout()`.
#' @return (i) Data frame providing bead counts per antigen per well per plate.
#' (ii) Designates whether wells should be repeated if there are \eqn{<=}  15 beads
#' (repeat) or if they are sufficient with > 15 beads (sufficient beads).
#' @export
#' @importFrom dplyr select group_by summarise mutate ungroup left_join arrange
#' @author Dionne Argyropoulos
#' @examples
#' \donttest{
#'
#' # Step 0: Load example raw data
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Step 1: Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Step 2: Process counts and perform quality control
#' counts      <- processCounts(sero_data)
#' counts_raw  <- getCounts(counts)
#' sample_ids  <- getSampleID(counts, plate_list)
#'
#' # Get Antigen Counts:
#' antigen_cts <- getAntigenCounts(counts, plate_list)
#' }
getAntigenCounts <- function(processed_counts, plate_list){
  #############################################################################
  # Data Wrangling
  #############################################################################

  antigen_specific_df <- processed_counts %>%
    dplyr::select(Location, Antigen, Warning, Count, Plate) %>%
    dplyr::group_by(Location, Antigen, Count, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Count = as.numeric(Count),
      Repeat = factor(Repeat, levels = c("sufficient beads", "repeat")),
      QC_antigen = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )

  #############################################################################
  # Create Table Output
  #############################################################################

  table <- getSampleID(processed_counts, plate_list) %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Location, Antigen, Plate, Count) %>%
    dplyr::mutate(Count = as.numeric(Count))
  antigen_specific_df_final <- antigen_specific_df %>%
    dplyr::left_join(table, by = c("Plate", "Count", "Antigen", "Location")) %>%
    dplyr::select(-Sum) %>%
    dplyr::arrange(Location, Antigen, Plate)

  return(antigen_specific_df_final)

}
#' Get All Counts Data
#'
#' This function obtains the count data from the raw Median Fluorescent
#' Intensity (MFI). This function relies on the output of the Antigen-specific
#' counts (`getAntigenCounts`) and the Well or Sample-specific counts
#' (`getCounts`).
#'
#' @param antigen_counts_output Output from `getAntigenCounts`.
#' @param counts_output Output from `getCounts`.
#' @return Joined data frame for all count data.
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select rename_with left_join all_of
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' # Step 0: Load example raw data
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout <- system.file(
#'   "extdata",
#'   "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Step 1: Read serology data and plate layout
#' sero_data  <- readSeroData(your_raw_data,"magpix")
#' plate_list <- readPlateLayout(your_plate_layout, sero_data)
#'
#' # Step 2: Process counts and perform quality control
#' counts      <- processCounts(sero_data)
#' counts_raw  <- getCounts(counts)
#' sample_ids  <- getSampleID(counts, plate_list)
#' antigen_cts <- getAntigenCounts(counts, plate_list)
#' counts_qc   <- getCountsQC(antigen_cts, counts_raw)
#'
#' }
getCountsQC <- function(antigen_counts_output, counts_output){

  #############################################################################
  # Data Wrangling
  #############################################################################

  # 1. Data Wrangling to store counts per antigen output
  antigen_counts_only <- antigen_counts_output %>%
    tidyr::pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "Count") %>% ungroup() %>%
    dplyr::select(Location, SampleID, Plate, everything()) %>%
    dplyr::rename_with(~ paste0(., "_Count"), .cols = where(is.numeric))

  # 2. Data Wrangling to store QC pass/fail per antigen output
  antigen_QC_only <- antigen_counts_output %>%
    tidyr::pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "QC_antigen") %>%
    dplyr::rename_with(~ paste0(., "_QC"), .cols = -c(SampleID, Location, Plate))

  # 3. Join both antigen-specific data frames together
  joined_antigen_counts <- antigen_counts_only %>%
    dplyr::left_join(antigen_QC_only, by = c("SampleID", "Location", "Plate"))

  #############################################################################
  # Re-arrange data
  #############################################################################

  # Get all base marker names by stripping _Count
  marker_bases <- names(joined_antigen_counts) %>%
    grep("_Count$", ., value = TRUE) %>%
    sub("_Count$", "", .)

  # Create the desired column order
  new_order <- c(
    "Location", "SampleID", "Plate",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_Count"), paste0(x, "_QC"))))
  )

  # Reordered data frame
  joined_antigen_counts <- joined_antigen_counts %>%
    dplyr::select(all_of(new_order))

  #############################################################################
  # Add total counts
  #############################################################################

  total_counts_only <- counts_output %>%
    dplyr::select(Location, Plate, QC_total)

  total_counts_final_output <- joined_antigen_counts %>%
    dplyr::left_join(total_counts_only, by = c("Location", "Plate"))

  return(total_counts_final_output)

}
