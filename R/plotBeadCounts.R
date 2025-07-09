#' Plot Bead Counts per Plate per Antigen
#'
#' Enhances the `plotCounts()` output by providing greater resolution,
#' displaying antigens per plate, and enabling SampleID name visibility via
#' hover (transformed to Plotly in server.R)
#'
#' @param antigen_counts_output Output from `getAntigenCounts()` (reactive).
#' @return Dot plot with values > 15 threshold coloured in blue (sufficient
#' beads) and ≤15 beads coloured in red (repeat) faceted by each antigen
#' (ggplot).
#' @export
#' @import ggplot2
#' @author Dionne Argyropoulos
plotBeadCounts <- function(antigen_counts_output){

  antigen_counts_output$Plate <- factor(antigen_counts_output$Plate, levels = unique(antigen_counts_output$Plate[order(as.numeric(str_extract(antigen_counts_output$Plate, "\\d+")))])) # reorder by plate number
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
