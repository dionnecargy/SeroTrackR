#' Random Forest Classification
#'
#' This function classifies unknown samples as recently exposed or not
#' (Note: MFItoRAU() or MFItoRAU_Adj() needs to be run first to convert to
#' RAU).
#'
#' @param mfi_to_rau_output Output from `MFItoRAU()` or `MFItoRAU_Adj()`.
#' @param algorithm_type Algorithm: "antibody_model" (PvSEM algorithm; default)
#' @param sens_spec User-selected Sensitivity/Specificity threshold: "balanced"
#' (default) or "90\% specificity".
#' @param qc_results Output from `runQC()`.
#' @param project Default = NULL. Only write "pkpfpv" if using Pk/Pf/Pv pipeline.
#'
#' @return
#' - Data frame with exposure status for every sample.
#' - Summary table with positive/negative results for each threshold.
#' @export
#'
#' @import workflows parsnip ranger
#' @importFrom dplyr select mutate rename_with ends_with bind_cols ungroup inner_join
#' @importFrom stringr str_replace
#'
#' @author Lauren Smith, Dionne Argyropoulos
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
#' qc_results  <- runQC(sero_data, plate_list)
#'
#' # Step 3: Convert MFI to RAU using ETH beads
#' mfi_to_rau <- MFItoRAU_Adj(
#'   sero_data   = sero_data,
#'   plate_list  = plate_list,
#'   qc_results  = qc_results
#' )
#'
#' # Step 4: Perform Pv classification
#' pv_classified <- classifyResults(
#'   mfi_to_rau_output = mfi_to_rau,
#'   algorithm_type    = "antibody_model",
#'   sens_spec         = "balanced",
#'   qc_results        = qc_results
#' )
#' }
classifyResults <- function(
    mfi_to_rau_output,
    algorithm_type = "antibody_model",
    sens_spec = "balanced",
    qc_results,
    project = NULL
  ) {

  #############################################################################
  # Data wrangling
  #############################################################################
  rau_data <- mfi_to_rau_output[[1]]

  if (is.null(project)) {
    rau_data <- rau_data %>%
      dplyr::select(SampleID, Plate, Location.2, ends_with("_Dilution")) %>%
      dplyr::mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
      dplyr::rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix
  } else if (project == "pkpfpv") {
    rau_data <- rau_data %>%
      dplyr::select(SampleID, Plate, Location.2, ends_with("_Adjloglog_Dilution")) %>%
      dplyr::mutate(across(ends_with("_Adjloglog_Dilution"), as.numeric)) %>%    # Convert only "_Adjloglog_Dilution" columns to numeric
      dplyr::rename_with(~ str_replace(., "_Adjloglog_Dilution$", ""), ends_with("_Adjloglog_Dilution")) # Remove the "_Adjloglog_Dilution" suffix
  } else {
    rau_data <- rau_data %>%
      dplyr::select(SampleID, Plate, Location.2, ends_with("_Dilution")) %>%
      dplyr::mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
      dplyr::rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix
  }

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19", "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")
  new_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  name_lookup <- setNames(new_names, old_names)

  rau_data <- rau_data %>%
    rename_with(
      ~ name_lookup[.x],
      .cols = any_of(old_names)
    )

  counts_QC_output <- qc_results$getCountsQC_output

  #############################################################################
  # Load files from package
  #############################################################################
  # Check if local file exists
  local_file <- system.file("extdata", "PvSeroTaTmodel.rds", package = "SeroTrackR")

  # Check if this is a CRAN submission
  # Step 1. Reads in serostatus using the trained random forest
  if (file.exists(local_file)) {
    # Master version: use local file
    antibody_model    <- readRDS(system.file("extdata", "PvSeroTaTmodel.rds", package = "SeroTrackR"))
  } else {
    # CRAN: use URL (requires internet)
    antibody_model    <- readRDS(url("https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/PvSeroTaTmodel.rds"))
  }

  # Step 2: Read in the random forest votes threshold values
  threshold_table   <- read.csv(system.file("extdata", "threshold_values.csv", package = "SeroTrackR"))

  #############################################################################
  # Model-specific functions
  #############################################################################

  # Step 3: Determine random forest votes threshold based on the algorithm_type string
  threshold <- if (sens_spec == "balanced") {
    threshold_table %>% filter(sens_spec == "max_sens_spec") %>% pull(threshold)
  } else if (sens_spec == "90% specificity") {
    threshold_table %>% filter(sens_spec == "90_spec") %>% pull(threshold)
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
  name_lookup_2 <- setNames(old_names, new_names)

  final_classification_qc <- counts_QC_output %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Plate, Location.2 = Location, QC_total) %>%
    dplyr::inner_join(final_results, by = c("SampleID", "Plate", "Location.2")) %>%
    dplyr::select(-Location.2) %>%
      rename_with(
        ~ name_lookup_2[.x],
        .cols = any_of(new_names)
      )

  return(final_classification_qc)
}
