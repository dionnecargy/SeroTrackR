#' Plot Bead Counts per Plate per Antigen
#'
#' Enhances the `plotCounts()` output by providing greater resolution,
#' displaying antigens per plate, and enabling SampleID name visibility via
#' hover (transformed to Plotly in server.R)
#'
#' @param qc_results Output from `runQC()`.
#' @return Dot plot with values > 15 threshold coloured in blue (sufficient
#' beads) and less than or equal to 15 beads coloured in red (repeat)
#' faceted by each antigen
#' (ggplot).
#' @export
#' @import ggplot2
#'
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
#' qc_results  <- runQC(sero_data, plate_list)
#'
#' # Step 3: Plot Bead Counts
#' plotBeadCounts(qc_results)
#' }
plotBeadCounts <- function(qc_results){

  antigen_counts_output <- qc_results$getAntigenCounts_output

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  antigen_counts_output <- antigen_counts_output %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number,
      Antigen = dplyr::recode(Antigen, !!!name_lookup)
    )

  antigen_counts_output %>%
    ggplot2::ggplot(
      aes(Plate, Count, colour = Repeat, alpha = Repeat, size = Repeat,
          text = paste("Sample:", SampleID, "<br>Bead Count:", Count, "<br>Location:", Location,"<br>Plate:", Plate))) +
    ggplot2::geom_hline(yintercept = 15, linetype = "dashed", colour = "#861e18") +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(breaks = c(0, 15, 50, 100, 150, 200)) +
    ggplot2::scale_colour_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    ggplot2::scale_alpha_manual(values = c("sufficient beads" = 0.5, "repeat" = 1)) +
    ggplot2::scale_size_manual(values = c("sufficient beads" = 1, "repeat" = 3)) +
    ggplot2::labs(x = "Plate", y = "Bead Counts", alpha = "", colour = "", size = "") +  # Add legend title
    ggplot2::facet_grid(~ Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") + # Show legend
    ggplot2::guides(alpha = "none") +
    ggplot2::guides(size = "none")

}
