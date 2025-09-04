#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for Pk/Pf/Pv Master Function
#'
#' This function leverages `MFItoRAU_Pk()` and `MFItoRAU_PfPv()` to create a final MFI to RAU
#' output for Pk/Pf/Pv analyses.
#'
#' @param serodata   Output of `readSeroData()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1".
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param counts_QC_output Output from `getCountsQC()`.
#'
#' @return A list of three data frames:
#' 1. Data frame with MFI data, converted RAU data, matched SampleID's, all intermediate dilution conversion factors
#' 2. Data frame with only SampleID's, MFI and RAU data
#' 3. Data frame #2 in long-format
#'
#' @importFrom dplyr select rename_with left_join ends_with right_join
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_replace
#' @importFrom utils read.csv
#'
#' @export
#' @author Dionne Argyropoulos
MFItoRAU_Plasmo <- function(serodata, plate_list, panel = "panel1", std_point, counts_QC_output){

  processed_master    <- processPkPfPv(serodata, plate_list, panel = "panel1")
  processed_PfPv      <- processed_master$PfPv
  processed_Pk        <- processed_master$Pk

  #############################################################################
  # Pfk MFI to RAU processing pipeline
  #############################################################################
  Pk_Final            <- MFItoRAU_Pk(processed_Pk, plate_list, std_point, counts_QC_output)

  #############################################################################
  # Pf/Pv MFI to RAU processing pipeline
  #############################################################################
  PfPv_PNG_Final      <- suppressWarnings(MFItoRAU_PfPv(processed_PfPv, plate_list, std_point, "PNG", counts_QC_output))
  PfPv_ETH_Final      <- suppressMessages(MFItoRAU_PfPv(processed_PfPv, plate_list, std_point, "ETH", counts_QC_output))

  #############################################################################
  # Join Dataframes Together
  #############################################################################
  pk_final_results            <- Pk_Final
  pfpv_PNG_final_results      <- PfPv_PNG_Final[[1]]
  pfpv_ETH_final_results      <- PfPv_ETH_Final[[1]]

  PkPfPv_Final <- suppressWarnings(pk_final_results %>%
                                     left_join(pfpv_PNG_final_results, by = c("SampleID", "Location.2", "Location", "Sample", "Plate", "QC_total")) %>%
                                     left_join(pfpv_ETH_final_results))
  PkPfPv_Final_MFI_RAU <- PkPfPv_Final %>%
    dplyr::select(SampleID, Plate, ends_with("_MFI", ignore.case = FALSE), ends_with("_Dilution", ignore.case = FALSE))

  #############################################################################
  # Create long df for downstream analyses (clean)
  #############################################################################
  PkPfPv_Panel_1 <- system.file("extdata", "PkPfPv_Panel_1.csv", package = "SeroTrackR")
  PkPfPv_Panel_1 <- read.csv(PkPfPv_Panel_1)

  PkPfPv_long_mfi <- PkPfPv_Final_MFI_RAU %>%
    dplyr::select(-ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigens", values_to = "MFI") %>%
    dplyr::left_join(PkPfPv_Panel_1, by = "Antigens")
  PkPfPv_long_rau <- suppressWarnings(PkPfPv_Final_MFI_RAU %>%
                                        dplyr::select(-ends_with("_MFI")) %>%
                                        dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
                                        tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigens", values_to = "RAU") %>%
                                        tidyr::separate(Antigens, c("Antigens", "Beads"), "_") %>%
                                        dplyr::left_join(PkPfPv_Panel_1, by = "Antigens"))
  PkPfPv_long_mfi_rau <- suppressWarnings(PkPfPv_long_mfi %>%
                                            right_join(PkPfPv_long_rau, by = c("SampleID", "Plate", "Antigens", "Species")))

  return(list(All_Results = PkPfPv_Final, MFI_RAU = PkPfPv_Final_MFI_RAU, MFI_RAU_long = PkPfPv_long_mfi_rau))

}
