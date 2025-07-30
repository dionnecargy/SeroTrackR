#' Get Count Data from Raw Median Fluorescent Intensity
#'
#' This function obtains the count data from the raw Median Fluorescent
#' Intensity (MFI). This is an interim function used for the plotCounts
#' function. This function relies on the `readAntigens` and `readSeroData` data
#' processing functions.
#'
#' @param processed_counts Output from `processCounts()` (reactive).
#' @return (i) Data frame providing bead counts per well per plate. (ii)
#' Designates whether wells should be repeated if there are ≤ 15 beads (repeat)
#' or if they are sufficient with > 15 beads (sufficient beads).
#' @export
#' @importFrom dplyr select group_by summarise mutate
#' @author Shazia Ruybal-Pesántez, Dionne Argyropoulos
getCounts <- function(processed_counts){

  counts <- processed_counts %>%
    dplyr::select(Location, Warning, Plate) %>%
    dplyr::group_by(Location, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Row = as.factor(substr(Location, 1, nchar(Location)-1)),
      Row = gsub("1", "", Row),
      Col = as.numeric(substr(Location, 2, nchar(Location))),
      QC_total = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )

  return(counts)
}
