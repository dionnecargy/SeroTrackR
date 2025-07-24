#' Read Serological Data and 5-point Standard Curve
#'
#' A function for generating RAU values with the two curves currently being used in the Pk analysis assay.
#' This is reliant on a 5-fold 5-point serial dilution covering S1 to S5 for poth Pk pool and ETH pool.
#'
#' @param raw_data  String with the raw data path.
#' @param platform  "magpix" or "bioplex".
#' @param plate_layout  File is labelled with S1-5 with ETH as a suffix, S1 to S5 with PK suffix, Blanks and NC.
#' @param date  String with today's date
#'
#' @return A data frame containing all samples and their RAU values.
#' @export
#'
#' @import drc dplyr tidyr
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom meltr melt_csv2 melt_csv
#' @importFrom janitor row_to_names
#'
#' @author Dionne Argyropoulos, Caitlin Bourke
runPlasmoSero5point <- function(raw_data, platform, plate_layout, date){

  ###############################################################################
  # Step 1: Read Raw Serological Data and Plate Layouts
  ###############################################################################

  # Read Serological Data: `pvsero::readSeroData()`
  df_sero <- readSeroData(
    raw_data = raw_data,
    platform = platform
  )
  # Read Plate Layout Data `pvsero::readPlateLayout()`
  plate_list <- readPlateLayout(
    plate_layout = plate_layout,
    antigen_output = df_sero
  )

  ###############################################################################
  # Step 2: Data wrangling
  ###############################################################################

  results <- df_sero[[2]] %>%
    janitor::clean_names() %>%
    tidyr::separate(location, into = c("delete", "well"),sep = ",") %>%
    dplyr::mutate(well = gsub(")", "", well)) %>%
    tidyr::extract(well, into = c("col", "row"), regex = "([A-Z])([0-9]+)", remove = F) %>%
    dplyr::select(-delete)
  counts <- df_sero[[3]] %>%
    janitor::clean_names() %>%
    dplyr::mutate(well = location) %>%
    tidyr::separate(well, into = c("delete", "well"),sep = ",") %>%
    dplyr::mutate(well = gsub(")", "", well)) %>%
    dplyr::select(-c(delete, location, sample)) %>%
    tidyr::pivot_longer(-c(well, plate)) %>%
    dplyr::rename(bead_count = value, protein = name)

  # Join all plates together into one list
  plate_list_long <- plate_list %>%
    purrr::imap_dfr(~ {
      .x %>%
        tidyr::pivot_longer(-1) %>%
        dplyr::mutate(plate = .y) %>% # .y is the name of the list element (e.g. "plate1", "plate2", etc.)
        dplyr::select(col = X1, row = name, sample_id = value, plate) # Overrides specificity of writing "Plate" in first ID of plate layout
    })

  # Join plates list to results
  results_named <- results %>%
    dplyr::left_join(plate_list_long, by = c("col", "row", "plate")) %>%
    dplyr::select(well, col, row, sample_id, plate, everything()) %>%
    dplyr::select(-sample)

  results_named_long <- results_named %>%
    tidyr::pivot_longer(-c(well, row, col, sample_id, plate), names_to = "protein", values_to = "mfi") %>%
    dplyr::mutate(
      mfi = as.numeric(mfi),
      date = date
    ) %>%
    tidyr::drop_na(sample_id) %>%
    dplyr::left_join(counts, by = c("well", "plate", "protein")) %>%
    dplyr::mutate(
      cat = case_when(
        grepl("Blank|^B", sample_id, ignore.case = TRUE) ~ "Blank",
        grepl("NC|naive", sample_id, ignore.case = TRUE) ~ "Naive",
        grepl("ETH", sample_id, ignore.case = TRUE) ~ "ETH Standard",
        grepl("PK", sample_id, ignore.case = TRUE) ~ "PK Standard",
        TRUE ~ "Unknown"
      ),
      cat = factor(cat, levels = c("ETH Standard", "PK Standard", "Blank", "Naive", "Unknown")),
      standard_cat = case_when(cat=="ETH Standard"|cat=="PK Standard" ~ sample_id),
      species = case_when(
        str_detect(protein, "lf")~"vivax",
        str_detect(protein, "pk")~"knowlesi",
        str_detect(protein, "hsp40ag1")~"falciparum",
        str_detect(protein, "pf")~"falciparum",
        str_detect(protein, "pv")~"vivax",
        str_detect(protein, "ptex")~"vivax",
        str_detect(protein, "p87")~"vivax", .default = protein)
    ) %>%
    tidyr::separate(standard_cat, into = c("standard_dil", "standard_type"), sep = "[-_ ]", extra = "merge", fill = "right") %>%
    mutate(standard_type = as.factor(str_replace_all(standard_type, "\\s+", ""))) %>% # Remove whitespace and make factor
    dplyr::distinct()

  ###############################################################################
  # Step 3: Add RAU conversions for Multiple Standard Curves
  ###############################################################################

  if (exists("results_named_long")==TRUE) {
    message("PASS: Proceeding with RAU conversion.")
  } else {
    message("ERROR: Please check the plate processing.")
  }
  # Create list of df for each Std Curve type: One per standard type and one per protein (i.e. - if 2 std types 2 x nprot dfs)
  # The ordering is all stds for 'standard_type' 1 followed by all 'standard_type' 2
  # Subsequent code assumes proteins are in order for each standard and not reverse (i.e. ordered by protein)

  stds <- results_named_long %>%
    dplyr::filter(cat =="ETH Standard" |cat== "PK Standard") %>%
    dplyr::mutate(log_mfi = log(mfi)) %>%
    dplyr::group_split(standard_type, protein)

  # Creating an additional list of dfs is mfi reading for each of the proteins - and adding another column for log_mfi which is needed by the 5PL
  protein_split <- results_named_long %>%
    dplyr::mutate(log_mfi = log(mfi)) %>%
    dplyr::group_split(protein)

  # Extract the number of proteins
  nprot <- length(protein_split)

  # Extract the names of the proteins from the list of df
  named_prot <- NULL
  for (i in 1:length(protein_split)) {
    named_prot[[i]] <- unique(protein_split[[i]]$protein)
  }
  named_prot <- unlist(named_prot)

  stds_mod <- NULL
  model_catch <- NULL
  protein_split_rau <- NULL

  suppressWarnings({
    for (i in 1:length(stds)) {
      stds_mod[[i]] <- stds[[i]] %>%
        tidyr::as_tibble() %>%
        dplyr::arrange(standard_dil) %>%
        dplyr::mutate(
          dilution = c(1/50, 1/(50*5^1), 1/(50*5^2), 1/(50*5^3), 1/(50*5^4)),
          dilution_scaled = dilution*(50*5^4)
        )
      model_catch[[i]] <- drc::drm(
        stds_mod[[i]]$log_mfi ~ stds_mod[[i]]$dilution,
        fct = LL.5(names = c("slope", "low_asym", "upp_asym", "ED50", "asym_par"))
      )
    }
  })
  names(model_catch) <- rep(named_prot,times = 2)

  for (i in 1:length(protein_split)) {
    protein_split_rau[[i]] <- protein_split[[i]] %>%
      dplyr::mutate(
        max_s1_std1 =stds_mod[[i]][stds_mod[[1]]$standard_dil=="S1",]$log_mfi,
        max_dil_std1 = stds_mod[[i]][stds_mod[[1]]$standard_dil=="S1",]$dilution,
        slope_std1 = model_catch[[i]]$fit$par[1],
        low_asym_std1 = model_catch[[i]]$fit$par[2],
        upp_asym_std1 = model_catch[[i]]$fit$par[3],
        ed50_std1 = model_catch[[i]]$fit$par[4],
        asym_par_std1 = model_catch[[i]]$fit$par[5]
      ) %>%
      dplyr::mutate(
        rau_std1 = case_when(
          log_mfi>=max_s1_std1 ~ max_dil_std1,
          log_mfi<max_s1_std1 ~ ed50_std1*((((upp_asym_std1-low_asym_std1)/(log_mfi-low_asym_std1))^(1/asym_par_std1) - 1 )^(1/slope_std1)),
          log_mfi<1/51200 ~ 1/51200
        )) %>%
      dplyr::mutate(
        max_s1_std2 =stds_mod[[i+nprot]][stds_mod[[1+nprot]]$standard_dil=="S1",]$log_mfi,
        max_dil_std2 = stds_mod[[i+nprot]][stds_mod[[1+nprot]]$standard_dil=="S1",]$dilution,
        slope_std2 = model_catch[[i+nprot]]$fit$par[1],
        low_asym_std2 = model_catch[[i+nprot]]$fit$par[2],
        upp_asym_std2 = model_catch[[i+nprot]]$fit$par[3],
        ed50_std2 = model_catch[[i+nprot]]$fit$par[4],
        asym_par_std2 = model_catch[[i+nprot]]$fit$par[5]
      ) %>%
      dplyr::mutate(rau_std2 = case_when(
        log_mfi>=max_s1_std2 ~ max_dil_std2,
        log_mfi<max_s1_std2 ~ ed50_std2*((((upp_asym_std2-low_asym_std2)/(log_mfi-low_asym_std2))^(1/asym_par_std2) - 1 )^(1/slope_std2) )
      )) %>%
      # The two rau values that we use! Anything lower than 1/51200 is made 1/51200 (1.95e-05) or above the S1 value is maxed at 0.02 (1/50)
      dplyr::mutate(
        rau_std1_restricted = case_when(
          rau_std1<1/51200 ~ 1/51200,
          rau_std1>1/50 ~ 1/50,
          is.na(rau_std1)~1/51200,
          TRUE ~ rau_std1
        ),
        rau_std2_restricted = case_when(
          rau_std2<1/51200 ~ 1/51200,
          rau_std2>1/50 ~ 1/50,
          is.na(rau_std2)~1/51200,
          TRUE ~ rau_std2
        )
      ) %>%
      # Make sure the assignment of species is correct based on the protein labelling
      mutate(species_specific_RAU = case_when(
        species=="vivax" ~ rau_std1_restricted,
        species=="falciparum" ~ rau_std1_restricted,
        species=="knowlesi" ~ rau_std2_restricted
      ))
  }

  ###############################################################################
  # Step 4: Final Results
  ###############################################################################
  # All results are in a long list depending on how many proteins there are - we make a long dataframe of this and return this!
  rau_combined <- bind_rows(protein_split_rau) %>% dplyr::select(-c(well, col, row))

  return(rau_combined)


}
