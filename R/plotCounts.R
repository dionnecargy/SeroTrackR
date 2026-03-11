#' Plot Bead Count Data
#'
#' This function gets the count data and plots the plate image, creating a new
#' facet (i.e., panel) for each antigen and each line represents the
#' different plates so that they can be visualised.
#'
#' @param qc_results Output from `runQC()`.
#' @param experiment_name User-input experiment name.
#' @return Tile Plot showing binary result of "sufficient beads" with cut-off
#' >15 beads and "repeat" less than or equal to 15 beads (ggplot).
#' @export
#' @import ggplot2
#' @importFrom forcats fct_rev
#' @author Shazia Ruybal-Pesántez, Dionne Argyropoulos
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
#' qc_results  <- runQC(sero_data, plate_list)
#'
#' # Step 3: Plot Counts
#' plotCounts(qc_results, "experiment1")
#' }
plotCounts <- function(qc_results, experiment_name){

  bead_counts <- qc_results$getCounts_output
  bead_counts$Plate <- factor(
    bead_counts$Plate,
    levels = unique(bead_counts$Plate[order(as.numeric(str_extract(bead_counts$Plate, "\\d+")))])
  ) # reorder by plate number
  bead_counts %>%
    ggplot2::ggplot(mapping = aes(x = Col, y = forcats::fct_rev(Row), fill = Repeat)) +
    ggplot2::geom_tile(aes(height = 0.90, width = 0.90)) +
    ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), position = "top") +
    ggplot2::scale_fill_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", title = experiment_name , fill = "") +
    ggplot2::facet_wrap( ~ Plate, ncol = 3, scales = "free_y")  # This will create separate facets for each level of 'Plate'
}
