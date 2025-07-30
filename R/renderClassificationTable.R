#' All Classification Data
#'
#' This function runs the classification algorithm for all possible sensitivity
#' and specificity options.
#'
#' @param mfi_to_rau_output Output from `MFItoRAU_PNG()` or `MFItoRAU_ETH()`
#' (reactive).
#' @param algorithm_type User-selected algorithm choice:
#' - "antibody_model" (PvSeroTaT model; default), or
#' - "antibody_model_excLF016" (PvSeroTat excluding LF016).
#' @param counts_QC_output Output from `getCountsQC()` (reactive).
#' @return A table of all classification outputs.
#' @export
#' @importFrom dplyr group_by summarise select mutate
#' @importFrom tidyr pivot_wider
#' @author Dionne Argyropoulos
renderClassificationTable <- function(mfi_to_rau_output, algorithm_type, counts_QC_output){
  # Load All sens_spec possibilities to cycle through
  sens_spec_all <- c("maximised", "85% sensitivity", "90% sensitivity", "95% sensitivity",
                     "85% specificity", "90% specificity", "95% specificity")
  # Run classify_final_results
  all_classifications <- purrr::map_dfr(sens_spec_all, ~{
    classifyResults(
      mfi_to_rau_output = mfi_to_rau_output,
      algorithm_type = algorithm_type,
      sens_spec = .x,
      counts_QC_output = counts_QC_output
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
