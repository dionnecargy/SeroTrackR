#' Relative Antibody Unit (RAU) Box Plots
#'
#' Boxplot of the RAU values.
#'
#' @param mfi_to_rau_output Output from `MFItoRAU_PNG()` or `MFItoRAU_ETH()`
#' (reactive).
#' @param location "PNG" or "ETH" (reactive).
#' @return Box plots with RAU values for each protein (ggplot).
#' @export
#' @importFrom dplyr select rename_with mutate ends_with
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @import ggplot2
#' @author Dionne Argyropoulos
plotRAU <- function(mfi_to_rau_output, location){

  df_results <- mfi_to_rau_output[[2]]
  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "RAU") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number
                  RAU = as.numeric(RAU))

  longitudinal_RAU <- system.file("extdata", "longitudinal_RAU.csv", package = "SeroTrackR")
  df_wehi <- read.csv(longitudinal_RAU)

  plot <- df_results %>%
    ggplot2::ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
    ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = RAU), fill = "grey", colour = "darkgrey") +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                           labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
    ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
    ggplot2::labs(x = "Antigen", y = "Antibody RAU") +
    ggplot2::facet_wrap( ~ Plate) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(plot)

}

