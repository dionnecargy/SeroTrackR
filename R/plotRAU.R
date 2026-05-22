#' Relative Antibody Unit (RAU) Box Plots
#'
#' Boxplot of the RAU values.
#'
#' @param mfi_to_rau_output Output from `MFItoRAU()` or `MFItoRAU_Adj()`
#'.
#' @param location "PNG" or "ETH".
#' @return Box plots with RAU values for each protein (ggplot).
#' @export
#' @importFrom dplyr select rename_with mutate ends_with
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @import ggplot2
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
#'   sero_data   = sero_data,
#'   plate_list  = plate_list,
#'   qc_results  = qc_results
#' )
#'
#' # Step 4: Plot RAU values
#' plotRAU(mfi_to_rau, "ETH")
#' }
plotRAU <- function(mfi_to_rau_output, location){

  df_results <- mfi_to_rau_output[[2]]

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "RAU") %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number
      RAU = as.numeric(RAU)
    )

  df_wehi <- read.csv(system.file("extdata", "longitudinal_RAU.csv", package = "SeroTrackR")) %>%
    dplyr::mutate(Antigen = dplyr::recode(Antigen, !!!name_lookup))

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

