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
#' @importFrom dplyr group_by mutate across inner_join rowwise summarise right_join select left_join rename_with all_of distinct bind_rows
#' @importFrom tidyr nest unnest pivot_wider
#' @importFrom purrr map
#' @importFrom plyr join
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
  #### ETH or PNG pool MFI to RAU conversion
  ##########################################################################################################

  if(location == "ETH"){

    ##########################################################################################################
    #### Reference Fit
    ##########################################################################################################

    png_eth_stds <- system.file("extdata", "png_eth_stds.csv", package = "SeroTrackR")
    refs <- read.csv(png_eth_stds)

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

      # Bind to location
      eth_converted_locations <- subset_data %>%
        dplyr::select(Location, Sample, Plate) %>%
        dplyr::right_join(estimate_eth, by = "Sample")

      results.location <- matrix(unlist(strsplit(as.character(eth_converted_locations$Location), ",")), ncol = 2, byrow = TRUE)[, 2]
      results.location <- substr(results.location, 1, nchar(results.location) - 1)
      eth_converted_locations <- cbind(Location.2 = results.location, eth_converted_locations)

      ## Matching SampleID from plate layout to corresponding sample.
      location.1 <- matrix(unlist(strsplit(subset_data$Location, ",")), ncol=2, byrow=T)[,2]
      location.1 <- substr(location.1, 1, nchar(location.1)-1)
      location.2 <- data.frame(Location.2=location.1, alpha=gsub("[[:digit:]]", "", location.1), numeric=gsub("[^[:digit:]]", "", location.1), SampleID=NA, stringsAsFactors = FALSE)
      for (i in location.2[, "Location.2"]){
        plate_layout_current <- layout[[plate_level]]
        names(plate_layout_current)[1] <- "Plate" # Relabel first column to be "Plate"
        location.2[location.2$Location.2==i, "SampleID"] <- plate_layout_current[
          plate_layout_current$Plate == unique(location.2[location.2$Location.2 == i, "alpha"]),
          colnames(plate_layout_current) == unique(location.2[location.2$Location.2 == i, "numeric"])
        ]
      }
      row_to_match <- location.2[,c("Location.2", "SampleID")]
      row_to_match <- row_to_match %>% dplyr::distinct(SampleID, Location.2, .keep_all = T) %>% na.omit()

      ## Using join() from plyr package to add SampleID information to results.df.wide. (default or given folder location and unique name)
      eth_converted_locations <- plyr::join(eth_converted_locations, row_to_match, by="Location.2", type="left")

      ## Move SampleID to first column
      eth_converted_locations <- eth_converted_locations[, c("SampleID", colnames(eth_converted_locations)[!(colnames(eth_converted_locations) %in% "SampleID")])]

      # Define column names to remain as characters
      character_columns <- c("SampleID", "Location", "Location.2", "Sample", "antigen", "Plate")

      # Convert specified columns to character
      eth_converted_locations[character_columns] <- lapply(eth_converted_locations[character_columns], as.character)

      # Convert all other columns (not in the specified list) to numeric
      numeric_columns <- setdiff(names(eth_converted_locations), character_columns)
      eth_converted_locations[numeric_columns] <- lapply(eth_converted_locations[numeric_columns], as.numeric)

      # Make long data frame wide
      eth_converted_locations_mfi <-eth_converted_locations %>%
        dplyr::select(-dilution) %>%
        tidyr::pivot_wider(names_from = "antigen", values_from = "mfi") %>%
        dplyr::rename_with(~paste0(.x, "_MFI"), -c(SampleID, Location.2, Location, Sample, Plate))
      eth_converted_locations_dilutions <- eth_converted_locations %>%
        dplyr::select(-mfi) %>%
        tidyr::pivot_wider(names_from = "antigen", values_from = "dilution") %>%
        dplyr::rename_with(~paste0(.x, "_ETHtoPNGloglog_Dilution"), -c(SampleID, Location.2, Location, Sample, Plate)) ########## Relabel with "_ETHtoPNGloglog_Dilution" instead of "_Dilution"
      eth_converted_wide <- eth_converted_locations_mfi %>%
        dplyr::left_join(eth_converted_locations_dilutions, by = c("SampleID", "Location.2", "Location", "Sample", "Plate"))

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
      results.df.wide <- as.data.frame(results.df.wide)
      results.location <- matrix(unlist(strsplit(as.character(results.df.wide$Location), ",")), ncol = 2, byrow = TRUE)[, 2]
      results.location <- substr(results.location, 1, nchar(results.location) - 1)
      results.df.wide <- cbind(Location.2 = results.location, results.df.wide)

      ## Matching SampleID from plate layout to corresponding sample.
      location.1 <- matrix(unlist(strsplit(L$Location, ",")), ncol=2, byrow=T)[,2]
      location.1 <- substr(location.1, 1, nchar(location.1)-1)
      location.2 <- data.frame(Location.2=location.1, alpha=gsub("[[:digit:]]", "", location.1), numeric=gsub("[^[:digit:]]", "", location.1), SampleID=NA, stringsAsFactors = FALSE)
      for (i in location.2[, "Location.2"]){
        plate_layout_current <- layout[[plate_level]]
        names(plate_layout_current)[1] <- "Plate" # Relabel first column to be "Plate"
        location.2[location.2$Location.2==i, "SampleID"] <- plate_layout_current[
          plate_layout_current$Plate == unique(location.2[location.2$Location.2 == i, "alpha"]),
          colnames(plate_layout_current) == unique(location.2[location.2$Location.2 == i, "numeric"])
        ]
      }
      row_to_match <- location.2[,c("Location.2", "SampleID")]
      row_to_match <- row_to_match %>% dplyr::distinct(SampleID, Location.2, .keep_all = T) %>% na.omit()

      ## Using join() from plyr package to add SampleID information to results.df.wide. (default or given folder location and unique name)
      results.df.wide <- plyr::join(results.df.wide, row_to_match, by="Location.2", type="left")

      ## Move SampleID to first column
      results.df.wide <- results.df.wide[, c("SampleID", colnames(results.df.wide)[!(colnames(results.df.wide) %in% "SampleID")])]

      # Define column names to remain as characters
      character_columns <- c("SampleID", "Location", "Location.2", "Sample", "Plate")

      # Convert specified columns to character
      results.df.wide[character_columns] <- lapply(results.df.wide[character_columns], as.character)

      # Convert all other columns (not in the specified list) to numeric
      numeric_columns <- setdiff(names(results.df.wide), character_columns)
      results.df.wide[numeric_columns] <- lapply(results.df.wide[numeric_columns], as.numeric)

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
      ungroup() %>%
      dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

    final_results <- dplyr::bind_rows(results_all) %>%
      dplyr::inner_join(counts_data, by = c("SampleID", "Plate", "Location.2"))

    final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>%
      dplyr::inner_join(counts_data, by = c("SampleID", "Plate"))

    # Output
    return(list(final_results, final_MFI_RAU_results, model_results_all))
  }

}
