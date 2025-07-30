#' Process Counts from Raw Serological Data file
#'
#' A helper function to process counts data.
#'
#' @param antigen_output Output from `readAntigens()` (reactive).
#' @return Returns a long table of counts with "Warning" category (<15 == 1 and
#' ≥ 15 == 0) for downstream wrangling.
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr pivot_longer
#' @author Dionne Argyropoulos
processCounts <- function(antigen_output){

  # 1. Store Counts Data
  counts_data <- antigen_output$counts

  # 2. Data Wrangling
  counts_data <- counts_data %>%
    dplyr::mutate(Location=gsub(".*,", "", Location)) %>%
    dplyr::mutate(Location=substr(Location, 1, nchar(Location)-1))  %>%
    tidyr::pivot_longer(-c(Sample, Location, Plate), names_to = "Antigen", values_to = "Count") %>%
    dplyr::mutate(Warning = case_when(
      as.numeric(Count)<15~1,
      as.numeric(Count)>=15~0
    ))

  return(counts_data)
}
