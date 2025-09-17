#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for Pk proteins
#'
#' This function is utilised in the master function `MFItoRAU_Plasmo()`.
#'
#' @param processed_Pk  df$Pk of output `processPkPfPv()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param counts_QC_output  Output from `getCountsQC()`
#'
#' @return Data frame with MFI data, converted RAU data and matched SampleID's.
#' @export
#'
#' @importFrom dplyr filter mutate  select  group_split arrange rename inner_join
#' @importFrom tidyr pivot_longer as_tibble pivot_wider
#' @importFrom drc drm
#' @importFrom purrr  reduce  map
#'
#' @author Dionne Argyropoulos, Caitlin Bourke
MFItoRAU_Pk <- function(processed_Pk, plate_list, std_point, counts_QC_output){

  L <- processed_Pk %>% dplyr::mutate(dplyr::across(-c(Location, Sample, Plate), as.numeric))
  layout <- plate_list

  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]
  L$type.letter <- substr(L$Sample, start=1, stop=1) # Categorises into "B" = "Blank", "S" = "Standards", "U" or "X" = "Samples"

  ##########################################################################################################
  #### Magic Parameters for 5-point and 10-point standard curve
  ##########################################################################################################

  if(std_point == 5){

    dilution = c(1/50, 1/(50*5^1), 1/(50*5^2), 1/(50*5^3), 1/(50*5^4))
    dilution_scaled = dilution*(50*5^4)
    dilution_factor = 5
    current_min_relative_dilution = 5.0^-5
    s1_concentration = 1/50
    s_final_concentration = 1/51200 # is what is written in the original function ...but do we want it to be S6 = 1/(50*5^5) ?

  } else if(std_point == 10){

    dilution = c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)
    dilution_scaled = dilution*25600
    dilution_factor = 2
    current_min_relative_dilution = 2.0^-10
    s1_concentration = 1/50
    s_final_concentration = 1/51200

  } else {
    print("No standard curve points provided.")
  }

  ##########################################################################################################
  #### LOG-LOG MODEL
  ##########################################################################################################

  # Iterate over each level in L$Plate and corresponding layout data frame
  stds_mod <- list()
  model_catch <- list()
  antigens_split_rau <- list()

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    # Fetch the corresponding layout data frame
    current_layout <- layout[[plate_level]] ######## when the plate tab name == the plate level defined in the plate column from the file name

    # Get standard curves for this plate
    stds <- subset_data %>%
      dplyr::filter(type.letter == "S") %>%
      dplyr::mutate(standard_type = as.factor(sub(".*?(\\d+).*", "\\1", Sample))) %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter, standard_type), names_to = "antigens", values_to = "mfi") %>%
      dplyr::mutate(log_mfi = log(mfi)) %>%
      dplyr::group_split(antigens)

    # Get sample data for all antigens
    # Creating an additional list of dfs is mfi reading for each of the antigens - and adding another column for log_mfi which is needed by the 5PL
    antigens_split <- subset_data %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigens", values_to = "mfi") %>%
      dplyr::mutate(log_mfi = log(mfi)) %>%
      dplyr::group_split(antigens)

    # Extract the number of antigens
    nprot <- length(antigens_split)
    # Extract the names of the antigens from the list of df
    named_prot <- NULL
    for (i in 1:length(antigens_split)) {
      named_prot[[i]] <- unique(antigens_split[[i]]$antigens)
    }
    named_prot <- unlist(named_prot)

    # Fit models per standard_type/antigens
    stds_mod_plate <- list()
    model_catch_plate <- list()

    suppressWarnings({
      for (i in 1:length(stds)) {
        stds_mod[[i]] <- stds[[i]] %>%
          tidyr::as_tibble() %>%
          dplyr::arrange(standard_type) %>%
          dplyr::mutate(
            dilution = dilution,
            dilution_scaled = dilution_scaled
          )
        model_catch[[i]] <- drc::drm(
          stds_mod[[i]]$log_mfi ~ stds_mod[[i]]$dilution,
          fct = LL.5(names = c("slope", "low_asym", "upp_asym", "ED50", "asym_par"))
        )
      }
    })
    names(model_catch) <- named_prot
    names(stds_mod) <- named_prot

    for (i in 1:length(antigens_split)) {
      df <- antigens_split[[i]]
      antigens <- unique(df$antigens)

      std_tbl <- stds_mod[[antigens]]
      mdl     <- model_catch[[antigens]]

      rau_df <- df %>%
        dplyr::mutate(
          max_s1 = std_tbl[std_tbl$standard_type=="1", ]$log_mfi,
          max_dil = std_tbl[std_tbl$standard_type=="1", ]$dilution,
          slope = mdl$fit$par[1],
          low_asym = mdl$fit$par[2],
          upp_asym = mdl$fit$par[3],
          ed50 = mdl$fit$par[4],
          asym_par = mdl$fit$par[5]
        ) %>%
        dplyr::mutate(rau = case_when(
          log_mfi>=max_s1 ~ max_dil,
          log_mfi<max_s1 ~ ed50*((((upp_asym-low_asym)/(log_mfi-low_asym))^(1/asym_par) - 1 )^(1/slope))
        )
        ) %>%
        dplyr::mutate(rau = ifelse(is.na(rau), s_final_concentration, rau)) %>%
        dplyr::mutate(rau = case_when(
          rau<s_final_concentration   ~ s_final_concentration,
          rau>s1_concentration        ~ s1_concentration,
          TRUE ~ rau
        )
        ) %>%
        dplyr::rename(MFI = mfi, Dilution = rau)

      rau_df_wide <- rau_df %>%
        tidyr::pivot_wider(
          id_cols = c(Location, Sample, Plate, type.letter),
          names_from = antigens,
          values_from = c(
            MFI, log_mfi, max_s1, max_dil, slope,
            low_asym, upp_asym, ed50, asym_par,
            Dilution
          ),
          names_glue = "{antigens}_{.value}"
        ) %>%
        dplyr::filter(!(type.letter == "S" | type.letter == "B")) %>%
        dplyr::select(-type.letter) %>%
        dplyr::mutate(Location.2 = stringr::str_extract(Location, "(?<=,)\\w+\\d+(?=\\))"))

      # Join to plate layout names
      names(current_layout)[1] <- "col" # Relabel first column to be "Plate"
      current_layout_final <- current_layout %>%
        tidyr::pivot_longer(-1) %>%
        dplyr::select(col, row = name, SampleID = value) %>%
        dplyr::mutate(Location.2 = paste0(col, row)) %>%
        dplyr::select(-c(col, row))

      rau_df_final <- rau_df_wide %>%
        dplyr::left_join(current_layout_final, by = "Location.2") %>%
        dplyr::select(SampleID, Location, Location.2, Sample, Plate, everything())

      # Save as list
      antigens_split_rau[[paste0(plate_level, "_", antigens)]] <- rau_df_final

    }
  }

  ###############################################################################
  # Step 4: Final Results
  ###############################################################################
  # Define ID columns (common to all dfs)
  id_cols <- c("SampleID", "Location", "Location.2", "Sample", "Plate")
  # Split by prefix before "_"
  by_prefix <- split(antigens_split_rau, sub("_.*", "", names(antigens_split_rau)))
  # Left join within each prefix group
  joined_list <- map(by_prefix, ~ reduce(.x, left_join, by = id_cols))
  # Bind lists
  rau_combined <- bind_rows(joined_list)
  # Join to counts_QC_output
  counts_data <- counts_QC_output %>%
    ungroup() %>%
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

  final_results <- rau_combined %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Plate", "Location.2"))

  return(final_results)
}
