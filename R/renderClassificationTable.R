#' All Classification Data
#'
#' This function runs the classification algorithm for all possible sensitivity
#' and specificity options.
#'
#' @param mfi_to_rau_output Output from `MFItoRAU()` or `MFItoRAU_Adj()`.
#' @param algorithm_type User-selected algorithm choice:
#' - "antibody_model" (PvSeroTaT model; default), or
#' - "antibody_model_excLF016" (PvSeroTat excluding LF016).
#' @param qc_results Output from `runQC()`.
#' @return A table of all classification outputs.
#' @export
#' @importFrom dplyr group_by summarise select mutate
#' @importFrom tidyr pivot_wider
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#'
#' # Step 0: Load in Raw Data
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")
#'
#' # Step 1: Reading in Raw Data
#' sero_data     <- readSeroData(raw_data = your_raw_data, "magpix")
#' plate_list    <- readPlateLayout(
#'   plate_layout = your_plate_layout,
#'   sero_data = sero_data
#' )
#'
#' # Step 2: Quality Control and MFI to RAU
#' qc_results <- runQC(sero_data, plate_list)
#'
#' # Step 4: Run MFI to RAU (e.g., using ETH beads)
#' mfi_to_rau_output  <- MFItoRAU_Adj(sero_data, plate_list, qc_results)
#'
#' # Step 5: Render classification table
#' renderClassificationTable(
#'   mfi_to_rau_output = mfi_to_rau_output,
#'   algorithm_type = "antibody_model",
#'   qc_results = qc_results
#' )
#'
#'}
#'
renderClassificationTable <- function(mfi_to_rau_output, algorithm_type, qc_results){
  # Load All sens_spec possibilities to cycle through
  sens_spec_all <- c("balanced", "85% sensitivity", "90% sensitivity", "95% sensitivity",
                     "85% specificity", "90% specificity", "95% specificity")

  # Run classify_final_results
  all_classifications <- purrr::map_dfr(sens_spec_all, ~{
    classifyResults(
      mfi_to_rau_output = mfi_to_rau_output,
      algorithm_type = algorithm_type,
      sens_spec = .x,
      qc_results = qc_results
    ) %>%
      as.data.frame() %>%  # Ensure it's a data frame
      dplyr::mutate(sens_spec = .x)  # Add the sens_spec column
  })
  # Return the combined data frame
  all_classifications %>%
    dplyr::group_by(sens_spec, pred_class_max) %>%
    dplyr::summarise(n = n()) %>%
    tidyr::pivot_wider(names_from = pred_class_max, values_from = n) %>%
    dplyr::select(`Sensitivity/Specificity` = sens_spec,
                  Seropositive = seropositive,
                  Seronegative = seronegative)
}
