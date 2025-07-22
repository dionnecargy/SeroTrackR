#' Random Forest Classification
#'
#' This function classifies unknown samples as recently exposed or not
#' (Note: MFItoRAU_PNG() or MFItoRAU_ETH() needs to be run first to convert to
#' RAU).
#'
#' @param mfi_to_rau_output Output from `MFItoRAU_PNG()` or `MFItoRAU_ETH()`
#' (reactive).
#' @param algorithm_type User-selected algorithm choice:
#' - "antibody_model" (PvSeroTaT model; default), or
#' - "antibody_model_excLF016" (PvSeroTat excluding LF016).
#' @param sens_spec User-selected Sensitivity/Specificity threshold:
#' - "maximised" (default),
#' - "85\% sensitivity",
#' - "90\% sensitivity",
#' - "95\% sensitivity",
#' - "85\% specificity",
#' - "90\% specificity".
#' - "95\% specificity".
#' @param counts_QC_output Output from `getCountsQC()` (reactive).
#' @return
#' - Data frame with exposure status for every sample.
#' - Summary table with positive/negative results for each threshold.
#' @export
#' @import workflows parsnip
#' @importFrom dplyr select mutate rename_with ends_with bind_cols ungroup inner_join
#' @importFrom stringr str_replace
#' @author Lauren Smith, Dionne Argyropoulos
classifyResults <- function(mfi_to_rau_output, algorithm_type, sens_spec, counts_QC_output) {

  #############################################################################
  # Data wrangling
  #############################################################################

  rau_data <- mfi_to_rau_output[[1]]
  rau_data <- rau_data %>%
    dplyr::select(SampleID, Plate, Location.2, ends_with("_Dilution")) %>%
    dplyr::mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
    dplyr::rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix

  #############################################################################
  # Load files from package
  #############################################################################
  antibody_model <- system.file("extdata", "PvSeroTaTmodel.rds", package = "pvsero")
  antibody_model_excLF016 <- system.file("extdata", "random_forest_excludingLF016.rds", package = "pvsero")
  threshold_values <- system.file("extdata", "threshold_values.csv", package = "pvsero")
  excluding_LF016_threshold_values <- system.file("extdata", "excluding_LF016_threshold_values.csv", package = "pvsero")

  #############################################################################
  # Model-specific functions
  #############################################################################

  # Step 1. Reads in serostatus using the trained random forest

  antibody_model <- readRDS(antibody_model) # Model 1: All top 8
  antibody_model_excLF016 <- readRDS(antibody_model_excLF016) # Model 2: w/o LF016

  # Step 2: Read in the random forest votes threshold values
  threshold_table <- if(algorithm_type == "antibody_model"){
    read.csv(threshold_values)
  } else if (algorithm_type == "antibody_model_excLF016"){
    read.csv(excluding_LF016_threshold_values)
  } else {
    stop("Invalid model provided")
  }

  # Step 3: Determine random forest votes threshold based on the algorithm_type string
  threshold <- if (sens_spec == "maximised") {
    threshold_table %>% filter(sens_spec == "max_sens_spec") %>% pull(threshold)
  } else if (sens_spec == "85% sensitivity") {
    threshold_table %>% filter(sens_spec == "85_sens") %>% pull(threshold)
  } else if (sens_spec == "90% sensitivity") {
    threshold_table %>% filter(sens_spec == "90_sens") %>% pull(threshold)
  } else if (sens_spec == "95% sensitivity") {
    threshold_table %>% filter(sens_spec == "95_sens") %>% pull(threshold)
  } else if (sens_spec == "85% specificity") {
    threshold_table %>% filter(sens_spec == "85_spec") %>% pull(threshold)
  } else if (sens_spec == "90% specificity") {
    threshold_table %>% filter(sens_spec == "90_spec") %>% pull(threshold)
  } else if (sens_spec == "95% specificity") {
    threshold_table %>% filter(sens_spec == "95_spec") %>% pull(threshold)
  } else {
    stop("Invalid sensitivity/specificity type provided.")
  }

  # Step 4: Run the model
  # Retrieve the model based on the algorithm_type string
  model <- get(algorithm_type)

  #############################################################################
  # Model outputs
  #############################################################################

  # Classify rau_data using the specified model and determine seropositive / seronegative based on selected threshold
  sero_status <- predict(model, new_data = rau_data, type = "prob") |>
    dplyr::mutate(
      .keep = "none",
      pred_class_max = ifelse(.pred_new > threshold, "seropositive", "seronegative"),
      pred_class_max = as.factor(pred_class_max)
    )

  final_results <- rau_data %>%
    dplyr::bind_cols(sero_status)

  #############################################################################
  # Return the table of prediction classes and QC pass/fail
  #############################################################################

  final_classification_qc <- counts_QC_output %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Plate, Location.2 = Location, QC_total) %>%
    dplyr::inner_join(final_results, by = c("SampleID", "Plate", "Location.2")) %>%
    dplyr::select(-Location.2)

  return(final_classification_qc)
}
