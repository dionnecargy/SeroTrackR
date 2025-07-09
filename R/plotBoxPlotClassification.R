#' Plot Classification
#'
#' One example of data visualisation to detect the median and interquartile
#' range of the RAU values per antigen for seropositive and seronegative
#' individuals. Please note that the `classifyResults()` function must
#' be run first.
#'
#' @param all_classifications Data frame of `classifyResults()` for all
#' Sens_Spec thresholds.
#' @param selected_threshold String with the threshold (reactive).
#' @return Box plots with RAU values for each protein stratified by
#' classification (ggplot).
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate
#' @importFrom tidyr pivot_longer
#' @author Dionne Argyropoulos
plotBoxPlotClassification <- function(all_classifications, selected_threshold){

  all_classifications %>%
    dplyr::filter(Sens_Spec == selected_threshold) %>%
    tidyr::pivot_longer(-c(SampleID, Plate, QC_total, pred_class_max, Sens_Spec), names_to = "Antigen", values_to = "RAU") %>%
    dplyr::mutate(pred_class_max = factor(pred_class_max, levels = c("seronegative", "seropositive"))) %>%
    ggplot2::ggplot(aes(x = pred_class_max, y = RAU, fill = pred_class_max)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = c(seronegative = "#878787", seropositive = "#d6604d")) +
    ggplot2::labs(title = paste0("Threshold Chosen: "), selected_threshold,
                  x = "Classification", y = "RAU", fill = "Classification") +
    ggplot2::facet_grid(~Antigen) +
    ggplot2:: theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
