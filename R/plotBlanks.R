#' Plot Raw Median Fluorescent Intensity Blanks Data
#'
#' This function gets the blank sample data and plots the blank sample Median
#' Fluorescent Intensity (MFI) values.
#'
#' @param sero_data Output from `readSeroData()`.
#' @param experiment_name User-input experiment name.
#' @return Bar plot showing whether MFI values for the blanks for each antigen
#' per plate is above or below the threshold MFI = 50 (ggplot).
#' @export
#' @import ggplot2
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @author Shazia Ruybal-Pesantez, Dionne Argyropoulos
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
#' # Plot blanks
#' plotBlanks(
#'   sero_data = sero_data,
#'   experiment_name = "experiment1"
#' )
#'
#' }
#'
plotBlanks <- function(sero_data, experiment_name){
  master_file <- sero_data
  blanks <- master_file$blanks

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  blanks %>%
    dplyr::select(-Location) %>%
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])),  # Reorder by plate number
      Antigen = dplyr::recode(Antigen, !!!name_lookup)
    ) %>%
    ggplot2::ggplot(aes(x = factor(Antigen), y = as.numeric(MFI), fill = Sample)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    ggplot2::labs(
      x = "Antigen",
      y = "MFI",
      title = experiment_name
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggplot2::facet_wrap(~ Plate)  # Create separate facets for each 'plate'
}
