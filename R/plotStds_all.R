#' Plot Raw Median Fluorescent Intensity of Standard Curve Data
#'
#' This function gets the standards data and plots the standard curves for any antigens (i.e., non-PvSeroTaT specific).
#'
#' @param serodata_output Output from `readSeroData()` (reactive).
#' @param experiment_name User-input experiment name (reactive).
#' @return
#' - Dot and line plot of standard curves (S1-S10)
#' - WEHI-acceptable standard curve data on background of plot with user data.
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr  pivot_longer
#' @import ggplot2
#' @export
#' @author Shazia Ruybal-Pesantez, Dionne Argyropoulos
plotStds_all <- function(serodata_output, experiment_name){
  master_file <- serodata_output
  stds <- master_file$stds

  stds_1 <- stds %>%
    dplyr::select(-Location) %>%
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number
                  Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")),
                  MFI = as.numeric(MFI))

  ggplot() +
    ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
    ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::labs(x = "Standard Curve",
                  y = "MFI",
                  title = experiment_name) +
    ggplot2::facet_wrap(~Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

  }
