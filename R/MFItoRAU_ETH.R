#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion based on ETH standard
#'
#' This function fits a 5-parameter logistic standard curve to the dilutions
#' of the positive controls for each protein and converts the MFI values
#' into relative antibody units (RAU) written by Eamon Conway.
#'
#' @param serodata_output Output from `readSeroData()` (reactive).
#' @param plate_list Output from `readPlateLayout()` (reactive).
#' @param counts_QC_output Output from `getCountsQC()` (reactive).
#' @return  A list of three data frames:
#' 1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#' 2. Plot information for `plotModel` function.
#' 3. Data frame of RAU data for random forest classification use.
#' @export
#' @importFrom dplyr group_by mutate across inner_join rowwise summarise right_join select left_join rename_with all_of ungroup
#' @importFrom tidyr nest unnest pivot_wider
#' @importFrom tidyselect matches
#' @importFrom purrr map
#' @author Eamon Conway, Dionne Argyropoulos
MFItoRAU_ETH <- function(serodata_output, plate_list, counts_QC_output){

  master_file <- serodata_output$results
  L <- master_file %>% dplyr::mutate(dplyr::across(-c(Location, Sample, Plate), as.numeric))
  layout <- plate_list

  ##########################################################################################################
  #### Reference Fit
  ##########################################################################################################

  png_eth_stds <- system.file("extdata", "png_eth_stds.csv", package = "SeroTrackR")
  refs <- read.csv(png_eth_stds)
  # MAGIC PARAMETERS FOR THIS SECTION
  s1_concentration <- 1/50
  current_min_relative_dilution <- 2.0^-10
  # END MAGIC PARAMETER DEFINITIONS

  control = list(maxit = 10000,
                 abstol = 1e-8,
                 reltol = 1e-6)

  initial_solution = c(-1.0, 0.0, 10, 0.0, 0.0)

  ref_fit <- refs %>%
    dplyr::group_by(.data$std_plate, .data$antigen) %>%
    tidyr::nest()  %>%
    dplyr::mutate(
      .keep = "none",
      eth_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$eth_mfi, .x$dilution, control)
      }),
      png_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$png_mfi, .x$dilution, control)
      })
    )

  reference_antigens = unique(ref_fit$antigen)

  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]

  L$type.letter <- substr(L$Sample, start=1, stop=1) # Categorises into "B" = "Blank", "S" = "Standards", "U" or "X" = "Samples"

  ##########################################################################################################
  #### Initialise outputs and prepare function by plate
  ##########################################################################################################

  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    ##########################################################################################################
    #### Apply conversion
    ##########################################################################################################

    eth_qa_sc <- subset_data %>%
      dplyr::filter(type.letter == "S") %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>%
      dplyr::mutate(dilution = 2 ^ (-as.numeric(gsub( # 2 = dilution factor
        "\\D", "", .data$`Sample`
      )) + 1))  %>%
      dplyr::group_by(.data$antigen) %>%
      tidyr::nest()

    eth_qa_mfi <- subset_data %>%
      dplyr::filter(type.letter == "U" | type.letter == "X") %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>%
      dplyr::group_by(.data$antigen) %>%
      tidyr::nest()

    qa_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$mfi, .x$dilution, control)
      }))

    # We have the fit for each antigen.
    eth_converted = dplyr::inner_join(ref_fit, qa_fit) %>%
      dplyr::inner_join(eth_qa_mfi) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          mfi = .data$mfi,
          Sample = .data$Sample,
          dilution = convert_mfi_to_dilution_no_bounds(mfi,new_fit, 0.0), # We do not want the initial conversion to have any bounds. There are some required due to asymptotes in the function however. (Eamon)
          ref_mfi = convert_dilution_to_mfi(dilution,eth_fit),
          dilution = convert_mfi_to_dilution(ref_mfi,png_fit, current_min_relative_dilution)
        )
      )) %>%
      tidyr::unnest(cols = data)

    # Take MEAN of these 10 repeats
    estimate_eth <- eth_converted %>%
      dplyr::group_by(antigen, Sample) %>%
      dplyr::summarise(dilution = mean(dilution) * s1_concentration,
                       mfi = mean(mfi))

    ##########################################################################################################
    #### MODEL RESULTS AND PLOTS
    ##########################################################################################################

    sc_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$mfi, .x$dilution, control)
      }))

    qa_converted <- dplyr::inner_join(sc_fit, eth_qa_sc) |>
      dplyr::rowwise() |>
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          Sample = .data$Sample,
          dilution = .data$dilution,
          mfi = .data$mfi,
          mfi_pred = convert_dilution_to_mfi(.data$dilution, new_fit)
        )
      )) |>
      tidyr::unnest(cols = data)

    model_results <- qa_converted

    ##########################################################################################################
    #### MERGE DATA: Relabel Sample Names with Plate Layout
    ##########################################################################################################
    # Bind plate-subset data with RAU-converted data
    eth_converted_locations <- subset_data %>%
      dplyr::select(Location, Sample, Plate) %>%
      dplyr::right_join(estimate_eth, by = "Sample")

    # Pivot wider
    eth_converted_wide.1 <- eth_converted_locations %>%
      # Pivot wider: All MFI values
      dplyr::select(-dilution) %>%
      tidyr::pivot_wider(names_from = "antigen", values_from = "mfi") %>%
      dplyr::rename_with(~paste0(.x, "_MFI"), -c(Location, Sample, Plate))
    eth_converted_wide.2 <- eth_converted_locations %>%
      # Pivot wider: All Dilution values
      dplyr::select(-mfi) %>%
      tidyr::pivot_wider(names_from = "antigen", values_from = "dilution") %>%
      dplyr::rename_with(~paste0(.x, "_Dilution"), -c(Location, Sample, Plate))

    eth_converted_wide <- dplyr::left_join(
      eth_converted_wide.1,
      eth_converted_wide.2,
      by = c("Location", "Sample", "Plate")
    )

    # Add positional information
    eth_converted_wide <- eth_converted_wide %>%
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
        cols = `1`:`12`,
        names_to = "numeric",
        values_to = "SampleID"
      ) %>%
      dplyr::rename(alpha = Plate) %>%
      tidyr::unite("Location.2", alpha:numeric, sep="", na.rm = T)

    # Match SampleID from plate layout to corresponding sample
    eth_converted_wide <- eth_converted_wide %>%
      dplyr::left_join(plate_layout_current, by = "Location.2") %>%
      # Keep only needed columns, distinct, and remove NA
      dplyr::distinct(SampleID, Location.2, .keep_all = TRUE) %>%
      tidyr::drop_na() %>%
      # Move SampleID to first column
      dplyr::select(SampleID, everything())

    # Define column names to remain as characters
    character_columns <- c("SampleID", "Location", "Location.2", "Sample", "Plate")

    # Convert specified columns to character
    eth_converted_wide <- eth_converted_wide %>%
      dplyr::mutate(
        # Convert specified columns to character
        across(all_of(character_columns), as.character),
        # Convert all other columns to numeric
        across(!all_of(character_columns), as.numeric)
      )

    ##########################################################################################################
    #### Create output dataframes
    ##########################################################################################################
    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Location.2|Plate|_MFI|\\_Dilution$", colnames(eth_converted_wide))
    MFI_RAU_results <- eth_converted_wide[, col_selection]

    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- eth_converted_wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results

  }

  ##########################################################################################################
  #### Joining all plate data
  ##########################################################################################################

  counts_data <- counts_QC_output %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

  final_results <- dplyr::bind_rows(results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))

  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))

  #############################################################################
  # Re-arrange data for final outputs
  #############################################################################

  # Get all base marker names by stripping _Count
  marker_bases <- names(final_results) %>%
    grep("_MFI$", ., value = TRUE) %>%
    sub("_MFI$", "", .)

  # Create the desired column order
  final_results_order <- c(
    "SampleID", "Location.2", "Location", "Sample", "Plate", "QC_total",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_Dilution"))))
  )
  final_MFI_RAU_order <- c(
    "SampleID", "Plate", "QC_total",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_Dilution"))))
  )

  # Reordered data frame
  final_results <- final_results %>%
    dplyr::select(all_of(final_results_order))

  final_MFI_RAU_results <- final_MFI_RAU_results %>%
    dplyr::select(all_of(final_MFI_RAU_order))

  return(list(final_results, final_MFI_RAU_results, model_results_all))
}
