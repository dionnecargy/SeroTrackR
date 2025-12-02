#' Median Fluorescent Intensity (MFI) Box Plots
#'
#' Boxplot of the MFI values.
#'
#' @param mfi_to_rau_output Output from `MFItoRAU()` or `MFItoRAU_ETH()`
#' (reactive).
#' @param location "PNG" or "ETH" (reactive).
#' @return Box plots with MFI values for each protein (ggplot).
#' @export
#' @importFrom dplyr select rename_with ends_with mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @import ggplot2
#' @author Dionne Argyropoulos
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
#' mfi_to_rau <- MFItoRAU_ETH(
#'   sero_data = sero_data,
#'   plate_list         = plate_list,
#'   counts_QC_output   = counts_qc
#' )
#'
#' # Step 4: Plot MFI values
#' plotMFI(mfi_to_rau, "MFI")
#' }
plotMFI <- function(mfi_to_rau_output, location){

  df_results <- mfi_to_rau_output[[2]]
  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_MFI")) %>%
    dplyr::rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number
                  MFI = as.numeric(MFI))

  df_wehi <- read.csv(url("https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/longitudinal_MFI.csv"))

  plot <- df_results %>%
    ggplot2::ggplot(aes(x= Antigen, y = MFI)) +
    ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = MFI), fill = "grey", colour = "darkgrey") +
    ggplot2::geom_boxplot(aes(fill = Antigen)) +
    ggplot2::scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(10, 10000), labels = c("10", "100", "1,000", "10,000")) +
    ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
    ggplot2::labs(x = "Antigen", y = "Antibody MFI") +
    ggplot2::facet_wrap( ~ Plate) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

  return(plot)

}
