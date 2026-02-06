#' Plot the Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' Results Data based on ETH standard
#'
#' This function gets the Median Fluorescent Intensity (MFI) to Relative
#' Antibody Units (RAU) model results data and plots the model fits based on
#' `MFItoRAU_Adj.`
#'
#' @param mfi_to_rau_output Output from `MFItoRAU_Adj()`.
#' @param sero_data Output from `readSeroData()`.
#' @return List of dot and line plots of MFI to RAU model standard curve,
#' with each one representing an individual plate (ggplot).
#' @export
#' @import ggplot2
#' @importFrom dplyr bind_rows
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
#' # Step 3: Convert MFI to RAU using ETH beads
#' mfi_to_rau <- MFItoRAU_Adj(
#'   sero_data = sero_data,
#'   plate_list         = plate_list,
#'   counts_QC_output   = counts_qc
#' )
#'
#' # Step 4: Plot Model Results
#' plotModel_Adj(mfi_to_rau, sero_data)
#' }
plotModel_Adj <- function(mfi_to_rau_output, sero_data){

  # Load model results
  model_results <- mfi_to_rau_output[[3]]

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  # Convert the list of data frames into a single data frame
  combined_data <- model_results %>%
    dplyr::bind_rows(.id = "Plate") %>%
    dplyr::mutate(antigen = dplyr::recode(antigen, !!!name_lookup))

  # Generate plots for each plate, grouping antigens together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot2::ggplot(
      data = subset(combined_data, Plate == plate_name),
      aes(x = dilution, y = mfi_pred, color = antigen)
      ) +  # Use 'Antigen' to differentiate lines
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::geom_point(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = mfi, color = antigen)) +
    ggplot2::labs(
      x = "Antibody Dilution",
      y = "Standard Curve (MFI)",
      fill = "Antigen",
      title = paste("Standard Curves for Plate:", plate_name)
    ) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~ antigen, scales = "free_y")  # Create a separate plot for each Antigen
  })

  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)

  return(plots_model)
}
