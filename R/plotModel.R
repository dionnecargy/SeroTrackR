#' Plot the Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' Results Data
#'
#' This function gets the Median Fluorescent Intensity (MFI) to Relative
#' Antibody Units (RAU) model results data and plots the model fits based on
#' `MFItoRAU`.
#'
#' @param mfi_to_rau_output Output from `MFItoRAU()`.
#' @param sero_data Output from `readSeroData()`.
#' @return List of dot and line plots of MFI to RAU model standard curve,
#' with each one representing an individual plate (ggplot).
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate across
#' @importFrom tidyr pivot_longer
#' @author Shazia Ruybal-Pesantez, Dionne Argyropoulos
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
#' qc_results <- runQC(sero_data, plate_list)
#'
#' # Step 3: Convert MFI to RAU using ETH beads
#' mfi_to_rau <- MFItoRAU(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   qc_results = qc_results
#' )
#'
#' # Step 4: Plot Model Results
#' plotModel(mfi_to_rau, sero_data)
#' }
plotModel <- function(mfi_to_rau_output, sero_data){

  model_results <- mfi_to_rau_output[[3]]

  combined_data <- model_results %>%
    dplyr::mutate(
      Plate = gsub("^plate", "", Plate)
    )

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  ### Get Standards for points
  stds_file <- sero_data$stds
  stds_log <-
    stds_file %>%
    dplyr::mutate(across(-c(Location, Sample, Plate), ~ as.numeric(.))) %>%
    tidyr::pivot_longer(-c(Location, Sample, Plate), names_to = "Antigen", values_to = "stdcurve") %>%
    dplyr::mutate(
      dilution = case_when(
        Sample == "S1" ~ 1/50,
        Sample == "S2" ~ 1/100,
        Sample == "S3" ~ 1/200,
        Sample == "S4" ~ 1/400,
        Sample == "S5" ~ 1/800,
        Sample == "S6" ~ 1/1600,
        Sample == "S7" ~ 1/3200,
        Sample == "S8" ~ 1/6400,
        Sample == "S9" ~ 1/12800,
        Sample == "S10" ~ 1/25600,
        .default = NA
      ),
      Antigen = dplyr::recode(Antigen, !!!name_lookup)
    )

  # Generate plots for each plate, grouping proteins together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot2::ggplot() +  # Use 'protein' to differentiate lines
      ggplot2::geom_line(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = exp(log.std), color = Antigen)) +
      ggplot2::geom_point(data = subset(stds_log, Plate == plate_name), aes(x = dilution, y = stdcurve, color = Antigen)) +
      ggplot2::scale_x_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                             labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::labs(
        x = "Antibody Dilution",
        y = "Standard Curve (log(MFI))",
        title = paste("Standard Curves for Plate:", plate_name)
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ Antigen, scales = "free")  # Create a separate plot for each antigen
  })


  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)

  return(plots_model)
}
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
#' qc_results <- runQC(sero_data, plate_list)
#'
#' # Step 3: Convert MFI to RAU using ETH beads
#' mfi_to_rau <- MFItoRAU_Adj(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   qc_results = qc_results
#' )
#'
#' # Step 4: Plot Model Results
#' plotModel_Adj(mfi_to_rau, sero_data)
#' }
plotModel_Adj <- function(mfi_to_rau_output, sero_data){

  # Load model results
  model_results <- mfi_to_rau_output[[3]]

  # Generate plots for each plate, grouping antigens together
  plots_model <- lapply(unique(model_results$Plate), function(plate_name) {
    ggplot2::ggplot(
      data = subset(model_results, Plate == plate_name),
      aes(x = dilution, y = mfi_pred, color = antigen)
    ) +  # Use 'Antigen' to differentiate lines
      ggplot2::geom_line() +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::geom_point(data = subset(model_results, Plate == plate_name), aes(x = dilution, y = mfi, color = antigen)) +
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
  names(plots_model) <- unique(model_results$Plate)

  return(plots_model)
}
