#' Get SampleID from Plate Layout
#'
#' A helper function to extract Sample ID based on plate name and row/col
#'
#' @param processed_counts Output from `processCounts()` (reactive).
#' @param plate_list Plate name inside of the plate layout file.
#' @return Returns the corresponding Sample ID for the correct row/column in
#' the plate layout file. Henceforth "Sample ID" refers to the code in the
#' plate layout file, while "Sample" is the code in the Luminex file.
#' @export
#' @importFrom dplyr left_join select mutate bind_rows
#' @importFrom tidyr pivot_longer
#' @author Dionne Argyropoulos
getSampleID <- function(processed_counts, plate_list) {
  plate_layout_longer <- list()

  for (plate_level in seq_along(plate_list)) {

    # Get plate name (or fallback to index)
    plate_name <- names(plate_list)[plate_level]
    if (is.null(plate_name) || plate_name == "") {
      plate_name <- as.character(plate_level)
    }

    # Read and wrangle Plate i
    plate_layout <- plate_list[[plate_level]]
    names(plate_layout)[1] <- "Row"

    plate_layout_level <- plate_layout %>%
      tidyr::pivot_longer(cols = `1`:`12`, names_to = "Col", values_to = "SampleID") %>%
      dplyr::mutate(
        Location = paste0(Row, Col),
        Plate = plate_name  # Add Plate info here
      )

    # Save to list
    plate_layout_longer[[plate_level]] <- plate_layout_level
  }

  # Combine all into a single data frame
  plate_layout_longer_df <- dplyr::bind_rows(plate_layout_longer) %>%
    dplyr::mutate(Plate = factor(Plate))  # Make Plate a factor

  # Join to antigen_specific_df
  final_table <- plate_layout_longer_df %>%
    dplyr::left_join(processed_counts, by = c("Location", "Plate")) %>%
    dplyr::select(-c(Row, Col))

  return(final_table)

}
