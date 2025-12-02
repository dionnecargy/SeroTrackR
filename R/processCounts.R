#' Process Counts from Raw Serological Data file
#'
#' A helper function to process counts data.
#'
#' @param sero_data Output from `readSeroData()` (reactive).
#' @return Returns a long table of counts with "Warning" category (<15 == 1 and
#' \eqn{>=}  15 == 0) for downstream wrangling.
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr pivot_longer
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' # Example demonstrating how to process bead count data.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' # Read in raw MAGPIX data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
#'
#' # Process counts
#' processed_master <- processCounts(sero_data = sero_data)
#'
#' }
#'
processCounts <- function(sero_data){

  # 1. Store Counts Data
  counts_data <- sero_data$counts

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
