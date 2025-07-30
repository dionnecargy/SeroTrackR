#' Get Count Data for each Antigen from the Raw Median Fluorescent Intensity
#'
#' This function obtains the count data from the raw Median Fluorescent
#' Intensity (MFI). This function relies on the `readAntigens` and
#' `readSeroData` data processing functions.
#'
#' @param processed_counts Output from `processCounts()` (reactive).
#' @param plate_list Output from `readPlateLayout()` (reactive).
#' @return (i) Data frame providing bead counts per antigen per well per plate.
#' (ii) Designates whether wells should be repeated if there are ≤ 15 beads
#' (repeat) or if they are sufficient with > 15 beads (sufficient beads).
#' @export
#' @importFrom dplyr select group_by summarise mutate ungroup left_join arrange
#' @author Dionne Argyropoulos
getAntigenCounts <- function(processed_counts, plate_list){
  #############################################################################
  # Data Wrangling
  #############################################################################

  antigen_specific_df <- processed_counts %>%
    dplyr::select(Location, Antigen, Warning, Count, Plate) %>%
    dplyr::group_by(Location, Antigen, Count, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Count = as.numeric(Count),
      Repeat = factor(Repeat, levels = c("sufficient beads", "repeat")),
      QC_antigen = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )

  #############################################################################
  # Create Table Output
  #############################################################################

  table <- getSampleID(processed_counts, plate_list) %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Location, Antigen, Plate, Count) %>%
    dplyr::mutate(Count = as.numeric(Count))
  antigen_specific_df_final <- antigen_specific_df %>%
    dplyr::left_join(table, by = c("Plate", "Count", "Antigen", "Location")) %>%
    dplyr::select(-Sum) %>%
    dplyr::arrange(Location, Antigen, Plate)

  return(antigen_specific_df_final)

}
