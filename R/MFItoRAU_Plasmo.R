#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for Pk/Pf/Pv Master Function
#'
#' This function leverages `MFItoRAU_Pk()` and `MFItoRAU_PfPv()` to create a final MFI to RAU
#' output for Pk/Pf/Pv analyses.
#'
#' @param sero_data   Output of `readserodata_output()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1".
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param counts_QC_output Output from `getCountsQC()`.
#'
#' @return A list of three data frames:
#' 1. Data frame with MFI data, converted RAU data, matched SampleID's, all intermediate dilution conversion factors
#' 2. Data frame with only SampleID's, MFI and RAU data
#' 3. Data frame #2 in long-format
#'
#' @importFrom dplyr select rename_with left_join ends_with right_join
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_replace
#' @importFrom utils read.csv
#'
#' @export
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' # Example demonstrating multi-plate 5-standard processing workflow.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data_5std <- c(
#'   system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
#' )
#' your_plate_layout_5std <- system.file(
#'   "extdata", "example_platelayout_pk_5std.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Read in raw MAGPIX data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data_5std,
#'   platform = "magpix"
#' )
#'
#' # Read matching plate layout
#' plate_list <- readPlateLayout(
#'   plate_layout = your_plate_layout_5std,
#'   sero_data = sero_data
#' )
#' # Quality control
#' processCounts_output      <- processCounts(sero_data)
#' getCounts_output          <- getCounts(processCounts_output)
#' sampleid_output           <- getSampleID(processCounts_output, plate_list)
#' getAntigenCounts_output   <- getAntigenCounts(processCounts_output, plate_list)
#' getCountsQC_output        <- getCountsQC(getAntigenCounts_output, getCounts_output)
#'
#' # Run MFI to RAU conversion
#' mfi_outputs               <- MFItoRAU_Plasmo(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   panel = "panel1",
#'   std_point = 5,
#'   counts_QC_output = getCountsQC_output
#' )
#'
#' # View All Outputs
#' mfi_outputs
#' }
MFItoRAU_Plasmo <- function(sero_data, plate_list, panel = "panel1", std_point, counts_QC_output){

  processed_master    <- processPkPfPv(sero_data, plate_list, panel = "panel1")
  processed_PfPv      <- processed_master$PfPv
  processed_Pk        <- processed_master$Pk

  #############################################################################
  # Pfk MFI to RAU processing pipeline
  #############################################################################
  Pk_Final            <- MFItoRAU_Pk(processed_Pk, plate_list, std_point, counts_QC_output)

  #############################################################################
  # Pf/Pv MFI to RAU processing pipeline
  #############################################################################
  PfPv_Final          <- suppressWarnings(MFItoRAU_PfPv(processed_PfPv, plate_list, std_point, "PNG", counts_QC_output))
  PfPv_ETH_Final      <- suppressMessages(MFItoRAU_PfPv(processed_PfPv, plate_list, std_point, "ETH", counts_QC_output))

  #############################################################################
  # Join Dataframes Together
  #############################################################################
  pk_final_results            <- Pk_Final
  pfpv_final_results      <- PfPv_Final[[1]]
  pfpv_ETH_final_results      <- PfPv_ETH_Final[[1]]

  PkPfPv_Final <- suppressWarnings(
    pk_final_results %>%
      left_join(pfpv_final_results, by = c("SampleID", "Location.2", "Location", "Sample", "Plate", "QC_total")) %>%
      left_join(pfpv_ETH_final_results))
  PkPfPv_Final_MFI_RAU <- PkPfPv_Final %>%
    dplyr::select(SampleID, Plate, ends_with("_MFI", ignore.case = FALSE), ends_with("_Dilution", ignore.case = FALSE))

  #############################################################################
  # Create long df for downstream analyses (clean)
  #############################################################################
  PkPfPv_Panel_1 <- read.csv(url("https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/PkPfPv_Panel_1.csv"))

  PkPfPv_long_mfi <- PkPfPv_Final_MFI_RAU %>%
    dplyr::select(-ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigens", values_to = "MFI") %>%
    dplyr::left_join(PkPfPv_Panel_1, by = "Antigens")
  PkPfPv_long_rau <- suppressWarnings(
    PkPfPv_Final_MFI_RAU %>%
      dplyr::select(-ends_with("_MFI")) %>%
      dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
      tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigens", values_to = "RAU") %>%
      tidyr::separate(Antigens, c("Antigens", "Beads"), "_") %>%
      dplyr::left_join(PkPfPv_Panel_1, by = "Antigens")) %>%
    dplyr::mutate(RAU_Method = case_when(
      Beads == "loglog" ~ "loglog",
      Beads == "ETHtoPNGloglog" ~ "ETHtoPNGloglog",
      .default = "loglog")
    ) %>% dplyr::select(-Beads)
  PkPfPv_long_mfi_rau <- suppressWarnings(PkPfPv_long_mfi %>%
                                            right_join(PkPfPv_long_rau, by = c("SampleID", "Plate", "Antigens", "Species")))%>%
    dplyr::select(SampleID, Plate, Antigens, Species, MFI, RAU, RAU_Method)

  return(list(All_Results = PkPfPv_Final, MFI_RAU = PkPfPv_Final_MFI_RAU, MFI_RAU_long = PkPfPv_long_mfi_rau))

}
#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for either PNG or ETH standard
#'
#' This function fits a 5-parameter logistic standard curve to the dilutions
#' of the positive controls for each protein and converts the MFI values
#' into relative antibody units (RAU). This function is utilised in the master
#' function `MFItoRAU_Plasmo()`.
#'
#' @param processed_PfPv  df$PfPv of output `processPkPfPv()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is an integer.
#' @param location "PNG" or "ETH" to filter WEHI standard curve data (reactive).
#' @param counts_QC_output  Output from `getCountsQC()`
#'
#' @return A list of three data frames:
#' 1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#' 2. Plot information for `plotModel` function.
#' 3. Data frame of RAU data for random forest classification use.
#' @export
#'
#' @importFrom dplyr group_by mutate across inner_join rowwise summarise right_join select left_join rename_with all_of distinct bind_rows ungroup
#' @importFrom tidyr nest unnest pivot_wider
#' @importFrom purrr map imap_dfr
#' @importFrom grDevices dev.off png recordPlot
#' @import drc
#'
#' @author Dionne Argyropoulos, Connie Li Wai Suen, Eamon Conway
MFItoRAU_PfPv <- function(processed_PfPv, plate_list, std_point, location, counts_QC_output){

  L <- processed_PfPv %>% dplyr::mutate(dplyr::across(-c(Location, Sample, Plate), as.numeric))
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
    s_final_concentration = 1/51200

  } else if(std_point == 10){

    dilution = c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)
    dilution_scaled = dilution*25600
    dilution_factor = 2
    current_min_relative_dilution = 2.0^-10
    s1_concentration = 1/50
    s_final_concentration = 1/51200

  } else {
    message("No standard curve points provided.")
  }

  ##########################################################################################################
  #### ETH or PNG pool MFI to RAU conversion
  ##########################################################################################################

  if(location == "ETH"){

    ##########################################################################################################
    #### Reference Fit
    ##########################################################################################################

    refs <- read.csv(url("https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/png_eth_stds.csv"))

    # MAGIC PARAMETERS FOR THIS SECTION
    s1_concentration <- s1_concentration
    current_min_relative_dilution <- current_min_relative_dilution
    # END MAGIC PARAMETER DEFINITIONS

    control = list(maxit = 10000, abstol = 1e-8, reltol = 1e-6)
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
        dplyr::mutate(dilution = dilution_factor ^ (-as.numeric(gsub("\\D", "", .data$`Sample`)) + 1))  %>%
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
        dplyr::summarise(
          dilution = mean(dilution) * s1_concentration,
          mfi = mean(mfi)
        )

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
        dplyr::mutate(
          # Setting observations with very high MFI to 1/50.
          dilution = ifelse(dilution > 0.02, 0.02, dilution),
          # Setting observations with very low MFI to 1/51200.
          dilution = ifelse(dilution < 1/51200, 1/51200, dilution)
        ) %>%
        tidyr::pivot_wider(names_from = "antigen", values_from = "dilution") %>%
        dplyr::rename_with(~paste0(.x, "_ETHtoPNGloglog_Dilution"), -c(Location, Sample, Plate))

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
      col_selection <- grepl("SampleID|Location.2|Plate|_MFI|\\_ETHtoPNGloglog_Dilution$", colnames(eth_converted_wide))
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
      ungroup() %>%
      dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

    final_results <- dplyr::bind_rows(results_all) %>%
      dplyr::inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))

    final_model_results_all <- dplyr::bind_rows(model_results_all)

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
      unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_ETHtoPNGloglog_Dilution"))))
    )
    final_MFI_RAU_order <- c(
      "SampleID", "Plate", "QC_total",
      unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_ETHtoPNGloglog_Dilution"))))
    )

    # Reordered data frame
    final_results <- final_results %>%
      dplyr::select(all_of(final_results_order))

    final_MFI_RAU_results <- final_MFI_RAU_results %>%
      dplyr::select(all_of(final_MFI_RAU_order))

    return(list(final_results, final_MFI_RAU_results, model_results_all))

  } else if (location == "PNG"){

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
            dil.X <- ifelse(dil.X > s1_concentration, s1_concentration, dil.X)
            dil.X <- ifelse((is.na(dil.X) & y>log.std[2]), s1_concentration, dil.X)       ## Setting observations with very high MFI to s1_concentration.
            dil.X <- ifelse(dil.X < s_final_concentration, s_final_concentration, dil.X)
            dil.X <- ifelse((is.na(dil.X) & y<max(log.std)), s_final_concentration, dil.X)  ## Setting observations with very low MFI to s_final_concentration

            location.X <- subset_data[r, "Location"]
            sample.X <- subset_data[r, "Sample"]
            Plate.X <- subset_data[r, "Plate"]
            results <- cbind(Location = location.X, Sample = sample.X, Plate = Plate.X,
                             MFI = mfi.X, loglog_Dilution = dil.X, DilutionReciprocal = 1 / dil.X,
                             MinStd = min(std), MaxDilution = min(dilution),
                             MaxStd = max(std), MinDilution = max(dilution))

            results.colnames <- c("Location", "Sample", "Plate",
                                  paste0(i, "_", c("MFI", "loglog_Dilution", "DilutionReciprocal",
                                                   "MinStd", "MaxDilution", "MaxStd",
                                                   "MinDilution")))
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
        title <- paste("Plate:", plate_level, "- antigens:", i)

        # Open a null device to prevent plotting on screen
        png(filename = tempfile())   # or pdf(tempfile())
        plot(model_list[[i]], main = title)
        model_results[[i]] <- recordPlot()  # save the plot object
        dev.off()
      }

      # Replay a saved plot later:
      model_results[[1]]  # will display the plot when called

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
        tidyr::pivot_longer(
          cols = `1`:`12`,
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
      col_selection <- grepl("SampleID|Plate|_MFI|\\_loglog_Dilution$", colnames(results.df.wide))
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
      dplyr::ungroup() %>%
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

}
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
    s_final_concentration = 1/51200

  } else if(std_point == 10){

    dilution = c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)
    dilution_scaled = dilution*25600
    dilution_factor = 2
    current_min_relative_dilution = 2.0^-10
    s1_concentration = 1/50
    s_final_concentration = 1/51200

  } else {
    message("No standard curve points provided.")
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
