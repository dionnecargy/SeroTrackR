#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion
#'
#' This function fits a 5-parameter logistic standard curve to the dilutions
#' of the positive controls for each protein and converts the MFI values
#' into relative antibody units (RAU) written by Connie Li Wai Suen.
#'
#' @param sero_data Output from `readSeroData()`.
#' @param plate_list Output from `readPlateLayout()`.
#' @param qc_results Output from `runQC()`.
#' @return  A list of three data frames:
#' 1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#' 2. Plot information for `plotModel` function
#' 3. Data frame of RAU data for random forest classification use.
#' @export
#' @import drc
#' @importFrom dplyr distinct select inner_join bind_rows
#' @importFrom tidyselect matches
#' @importFrom purrr imap_dfr
#' @author Connie Li Wai Suen, Dionne Argyropoulos
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
#' # Step 3: Convert MFI to RAU using PNG beads
#' mfi_to_rau <- MFItoRAU(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   qc_results = qc_results
#' )
#'
#' }
MFItoRAU <- function(sero_data, plate_list, qc_results){

  # Idea: parameter for pf/pv ifelse
  # Idea: add 5-point standard curve reference as per MFItoRAU_plasmo

  master_file <- sero_data
  L <- master_file$results
  layout <- plate_list
  counts_QC_output <- qc_results$counts_QC_output

  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]

  L$type.letter <- substr(L$Sample, start=1, stop=1)
  dilution <- c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)
  dilution.scaled <- dilution*25600; dilution.scaled
  dilution.plot <- c("1/50", "1/100", "1/200", "1/400", "1/800", "1/1600", "1/3200", "1/6400", "1/12800", "1/25600")

  ##########################################################################################################
  #### LOG-LOG MODEL
  ##########################################################################################################

  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    # Fetch the corresponding layout data frame
    current_layout <- layout[[plate_level]] ######## when the plate tab name == the plate level defined in the plate column from the file name

    # Initialize storage for results
    results.df.wide <- NULL
    model_list <- list()

    # Iterate over antigens
    for (i in antigens){
      results.df <- NULL
      ## Taking the mean of duplicates for each standard and storing in object std in the following order: S1, S2, S3, ..., S9, S10.
      std <- NULL
      b <- c <- d <- e <- NULL
      # Process standards
      for (r in 1:nrow(subset_data)){
        if (subset_data$type.letter[r]=="S"){
          std <- c(std, as.numeric(subset_data[r,i]))
          std <- ifelse(is.na(std) | std == 0, 1, std)
        }
      }

      log.std <- log(as.numeric(std))
      model1 <- drc::drm(log.std ~ dilution, fct = LL.5(names = c("b", "c", "d", "e", "f")))
      summary(model1)
      model_list[[i]] <- model1

      b <- coef(summary(model1))[1]; b  ## slope
      c <- coef(summary(model1))[2]; c  ## lower asymptote
      d <- coef(summary(model1))[3]; d  ## upper asymptote
      e <- coef(summary(model1))[4]; e  ## ED50
      f <- coef(summary(model1))[5]; f  ## asymmetry parameter (f=1 for 4PL curves)

      ##########################################################################################################
      #### MFI TO RAU CONVERSION
      ##########################################################################################################

      # Process unknowns
      for (r in 1:nrow(subset_data)) {
        results <- NULL
        if (subset_data$type.letter[r] == "U" | subset_data$type.letter[r] == "X") { ##### Unknown works for MAGPIX and X works for BioPlex
          mfi.X <- as.numeric(subset_data[r, i])
          y <- log(mfi.X)

          if (y > max(log.std)) {
            dil.X <- max(dilution)
          } else {
            dil.X <- e*(( ((d-c)/(y-c))^(1/f) - 1 )^(1/b) )
          }
          dil.X <- ifelse(dil.X > 0.02, 0.02, dil.X)
          dil.X <- ifelse((is.na(dil.X) & y>log.std[2]), 0.02, dil.X)       ## Setting observations with very high MFI to 1/50.
          dil.X <- ifelse(dil.X < 1/51200, 1/51200, dil.X)
          dil.X <- ifelse((is.na(dil.X) & y<max(log.std)), 1/51200, dil.X)  ## Setting observations with very low MFI to 1/51200.

          location.X  <- subset_data[r, "Location"]
          sample.X    <- subset_data[r, "Sample"]
          Plate.X     <- subset_data[r, "Plate"]
          results     <- cbind(
            Location = location.X, Sample = sample.X, Plate = Plate.X,
            MFI = mfi.X, Dilution = dil.X, DilutionReciprocal = 1 / dil.X,
            MinStd = min(std), MaxDilution = min(dilution),
            MaxStd = max(std), MinDilution = max(dilution)
          )

          results.colnames <- c(
            "Location", "Sample", "Plate",
            paste0(i, "_", c("MFI", "Dilution", "DilutionReciprocal", "MinStd", "MaxDilution", "MaxStd", "MinDilution"))
          )
          colnames(results) <- results.colnames
        }
        results.df <- rbind(results.df, results)
      }

      # Merge results into wide format
      if (is.null(results.df.wide)) {
        results.df.wide <- results.df
      } else {
        results.df.wide <- merge(results.df.wide, results.df, by = c("Location", "Sample", "Plate"))
      }
    }

    ##########################################################################################################
    #### MODEL RESULTS AND PLOTS
    ##########################################################################################################

    # Plot models with plate in the title
    model_results <- list()
    for (i in names(model_list)) {
      title <- paste("Plate:", plate_level, "- Protein:", i)  # Combine plate and protein name
      model_results[[i]] <- plot(model_list[[i]], main = title)
    }

    ##########################################################################################################
    #### MERGE DATA
    ##########################################################################################################
    # Bind to location
    results.df.wide <- results.df.wide %>%
      as.data.frame() %>%
      # Clean up the new Location.2 (remove trailing space and last character)
      dplyr::mutate(
        Location.2 = stringr::str_split_fixed(as.character(Location), ",", 2)[, 2],
        Location.2 = stringr::str_trim(Location.2),
        Location.2 = stringr::str_sub(Location.2, 1, -2)
      ) %>%
      dplyr::select(Location.2, everything())

    plate_layout_current <- layout[[plate_level]] %>% dplyr::rename(Plate = 1)  # rename first column
    plate_layout_current <- plate_layout_current %>%
      dplyr::mutate(dplyr::across(tidyselect::matches("^[0-9]+$"), as.character)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::matches("^[.x]*[0-9]+$"),
        names_to = "numeric",
        values_to = "SampleID"
      ) %>%
      dplyr::rename(alpha = Plate) %>%
      tidyr::unite("Location.2", alpha:numeric, sep="", na.rm = T)

    # Match SampleID from plate layout to corresponding sample
    results.df.wide <- results.df.wide %>%
      dplyr::left_join(plate_layout_current, by = "Location.2") %>%
      # Keep only needed columns, distinct, and remove NA
      dplyr::distinct(SampleID, Location.2, .keep_all = TRUE) %>%
      tidyr::drop_na() %>%
      # Move SampleID to first column
      dplyr::select(SampleID, everything())

    # Define column names to remain as characters
    character_columns <- c("SampleID", "Location", "Location.2", "Sample", "Plate")

    # Convert specified columns to character
    results.df.wide <- results.df.wide %>%
      dplyr::mutate(
        # Convert specified columns to character
        across(all_of(character_columns), as.character),
        # Convert all other columns to numeric
        across(!all_of(character_columns), as.numeric)
      )

    ##########################################################################################################
    #### Output
    ##########################################################################################################

    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Plate|_MFI|\\_Dilution$", colnames(results.df.wide))
    MFI_RAU_results <- results.df.wide[, col_selection]

    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- results.df.wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results
  }

  #############################################################################
  # Return the final results tables with QC pass/fail
  #############################################################################

  counts_data <- counts_QC_output %>%
    ungroup() %>%
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

  final_results <- dplyr::bind_rows(results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Plate", "Location.2"))

  final_model_results_all <- purrr::imap_dfr(
    model_results_all,
    ~ purrr::imap_dfr(.x, ~ dplyr::mutate(.x, Antigen = .y), .id = "Antigen"),
    .id = "Plate"
  )

  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Plate"))

  # Output
  return(list(final_results, final_MFI_RAU_results, final_model_results_all))

}
