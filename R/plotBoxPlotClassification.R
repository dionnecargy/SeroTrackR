#' Plot Classification
#'
#' One example of data visualisation to detect the median and interquartile
#' range of the RAU values per antigen for seropositive and seronegative
#' individuals. Please note that the `classifyResults()` function must
#' be run first.
#'
#' @param all_classifications Data frame of `classifyResults()` for all
#' sens_spec thresholds.
#' @param selected_threshold String with the threshold.
#' @return Box plots with RAU values for each protein stratified by
#' classification (ggplot).
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr
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
#'   sero_data    = sero_data,
#'   plate_list   = plate_list,
#'   qc_results   = qc_results
#' )
#'
#' # Step 4: Define sens/spec thresholds
#' sens_spec_all <- c(
#'   "balanced", "85% sensitivity", "90% sensitivity", "95% sensitivity",
#'   "85% specificity", "90% specificity", "95% specificity"
#' )
#'
#' # Step 5: Classify results across all thresholds
#' all_classifications <- purrr::map_dfr(sens_spec_all, ~{
#'   classifyResults(
#'     mfi_to_rau_output = mfi_to_rau,
#'     algorithm_type = "antibody_model",
#'     sens_spec = .x,
#'     qc_results = qc_results
#'   ) |>
#'   as.data.frame() |>
#'   dplyr::mutate(sens_spec = .x)
#' })
#'
#' # Plot classification for a single threshold
#' plotBoxPlotClassification(all_classifications, "balanced")
#' }
plotBoxPlotClassification <- function(all_classifications, selected_threshold){

  all_classifications %>%
    dplyr::filter(sens_spec == selected_threshold) %>%
    tidyr::pivot_longer(
      -c(SampleID, Plate, QC_total, pred_class_max, sens_spec),
      names_to = "Antigen",
      values_to = "RAU"
    ) %>%
    dplyr::mutate(
      pred_class_max = factor(pred_class_max, levels = c("seronegative", "seropositive"))
    ) %>%
    ggplot2::ggplot(aes(x = pred_class_max, y = RAU, fill = pred_class_max)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = c(seronegative = "#878787", seropositive = "#d6604d")) +
    ggplot2::labs(
      title = paste0("Threshold Chosen: ", as.character(selected_threshold)),
      x = "Classification",
      y = "RAU",
      fill = "Classification"
    ) +
    ggplot2::facet_grid(~Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
