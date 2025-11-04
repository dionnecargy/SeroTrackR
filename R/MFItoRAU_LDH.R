#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for LDH
#'
#' This function fits a 5-parameter logistic standard curve to the dilutions
#' of the positive controls for each protein and converts the MFI values
#' into relative antibody units (RAU).
#'
#' @param serodata_output Output from `readSeroData()` or `readSeroData()` (reactive).
#' @param plate_list Output from `readPlateLayout()` (reactive).
#' @param file_path A file path to write the .csv final file. Default: Current working directory.
#' @param dilution  A list of numbers ranging from S1 to S10. Default: 1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81.
#'
#' @return A data frame containing the MFI and RAU Dilution values for each sample
#' @export
#'
#' @import drc
#' @import dplyr
#' @importFrom tidyr pivot_wider  drop_na
#' @importFrom stringr str_split  str_sub str_remove
#' @importFrom purrr map  map_chr
#' @importFrom here here
#' @importFrom utils write.csv
#'
#' @author Connie Li Wai Suen, Caitlin Bourke, Dionne Argyropoulos
MFItoRAU_LDH <- function(
    serodata_output,
    plate_list,
    dilution = c(1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81),
    file_path = NULL
){

  #######################################################################
  # Step 1: Read raw serology data and plate layout
  #######################################################################

  master_file   <- serodata_output
  L             <- master_file$results
  layout        <- plate_list
  dilution      <- dilution

  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]

  L$type.letter <- substr(L$Sample, start=1, stop=1)

  results.df.wide <- NULL

  ##########################################################################################################
  #### Step 2: Perform LOG-LOG MODEL
  ##########################################################################################################
  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    # Fetch the corresponding layout data frame
    current_layout <- layout[[plate_level]] ######## when the plate tab name == the plate level defined in the plate column from the file name

    for (i in antigens) {
      results.df <- NULL
      ## Taking the mean of duplicates for each standard and storing in object std in order: S1-S10.
      std <- NULL
      b <- c <- d <- e <- NULL
      for (r in 1:nrow(subset_data)) {
        if (subset_data$type.letter[r] == "S") {
          std <- c(std, as.numeric(subset_data[r, i]))
        }
      }

      ## Log-log model to obtain a more linear relationship & make it easier to interpolate around the lower asymptote.
      log.std <- log((std))

      ## Five-parameter logistic function is given by the expression: f(x) = c + \frac{d-c}{(1+\exp(b(\log(x)-\log(e))))^f}
      model1 <- drm(log.std ~ dilution, fct = LL.5(names = c("b", "c", "d", "e", "f")))
      # summary(model1)

      # Sys.sleep(0.1)  ## Suspends execution for 0.1 second to prevent RStudio errors when plotting within the loop.
      # plot(model1, main = i)

      ## http://psg.hitachi-solutions.com/masterplex/blog/the-4-parameter-logistic-4pl-nonlinear-regression-model
      ## F(x) = ((A-D)/(1+((x/C)^B))) + D    ## where A=minimum asymptote, B=Hill slope, C=ED50, D=Maximum asymptote
      ## x = C*(((A-D)/(F(x)-D))-1)^(1/B) = e*(((c-d)/(log(mfi.X)(1/f)))-1)^(1/b)
      b <- coef(summary(model1))[1] ## slope
      c <- coef(summary(model1))[2] ## lower asymptote
      d <- coef(summary(model1))[3] ## upper asymptote
      e <- coef(summary(model1))[4] ## ED50
      f <- coef(summary(model1))[5] ## asymmetry parameter (f=1 for 4PL curves)

      ##########################################################################################################
      #### Step 3: MFI TO RAU CONVERSION: Processing Unknowns
      ##########################################################################################################

      for (r in 1:nrow(subset_data)) {
        results <- NULL
        if (subset_data$type.letter[r] == "X" | subset_data$type.letter[r] == "U") {
          mfi.X <- subset_data[r, i]
          mfi.X = as.numeric(mfi.X)
          y <- log(mfi.X)

          ## Extrapolating slightly (use maximum dilution of 16.94 pg/ml instead of 50 pg/ml)
          ## so that MFI values corresponding to dilutions between 1/25600 and 1/51200
          ## are given a dilution instead of being set to 1/25600.

          if (y > max(log.std)) {
            dil.X <- max(dilution)
          } else {
            dil.X <- e * ((((d - c) / (y - c)) ^ (1 / f) - 1) ^ (1 / b))
          }
          dil.X <- ifelse(dil.X > 1000000, 1000000, dil.X)
          dil.X <- ifelse((is.na(dil.X) & y > log.std[2]), 1000000, dil.X)  # Set observations with very high MFI to 1,000,000 pg/ml.
          dil.X <- ifelse(dil.X < 16.94, 16.94, dil.X)
          dil.X <- ifelse((is.na(dil.X) & y < max(log.std)), 16.94, dil.X) # Set observations with very low MFI to 16.94 pg/ml.

          location.X  <- subset_data[r, "Location"]
          sample.X    <- subset_data[r, "Sample"]
          plate.X     <- subset_data[r, "Plate"]
          results     <- cbind(Location = location.X, Sample = sample.X, Plate = plate.X,
                               MFI = mfi.X, Dilution = dil.X, DilutionReciprocal = 1 / dil.X,
                               MinStd = min(std), MaxDilution = min(dilution),
                               MaxStd = max(std), MinDilution = max(dilution))
          results.colnames <- c("Location", "Sample", "Plate",
                                paste0(i, "_", c("MFI", "Dilution", "DilutionReciprocal", "MinStd", "MaxDilution", "MaxStd", "MinDilution")))
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
    #### Step 4: MERGE DATA with plate layout
    ##########################################################################################################
    # Step 1: Create "Location.2" Variable which has the row and column ID only
    results.df.wide <- results.df.wide %>%
      as.data.frame() %>%
      dplyr::mutate(
        Location.2 = Location %>%
          as.character() %>%
          str_split(",") %>%                  # split on ","
          map_chr(~ .x[2]) %>%                # take the 2nd element
          str_sub(1, -2)                      # drop the last character
      ) %>%
      dplyr::relocate(Location.2, .before = 1)       # put Location.2 in front

    # Step 2: Create plate layout to bind to "results.df.wide"
    # 1. Parse L$Location into well IDs
    location.2 <- tibble(Location = subset_data$Location) %>%
      dplyr::mutate(
        Location.2 = str_split(Location, ",", simplify = TRUE)[,2] %>%
          str_sub(1, -2),
        alpha   = str_remove(Location.2, "[0-9]+"),
        numeric = str_remove(Location.2, "[^0-9]")
      )
    # 2. Get current plate layout, standardise colnames
    plate_layout_current <- layout[[plate_level]] %>% dplyr::rename(Plate = 1)  # relabel first column to "Plate"
    # 3. Reshape layout to long form (row = Plate, column = numeric, value = SampleID)
    plate_layout_long <- plate_layout_current %>%
      tidyr::pivot_longer(
        -Plate,
        names_to = "numeric",
        values_to = "SampleID"
      )
    # 4. Join Location info with plate layout to attach SampleIDs
    row_to_match <- location.2 %>%
      dplyr::left_join(plate_layout_long, by = c("alpha" = "Plate", "numeric" = "numeric")) %>%
      dplyr::select(Location.2, SampleID) %>%
      dplyr::distinct(SampleID, Location.2, .keep_all = TRUE) %>%
      tidyr::drop_na()

    # Step 3: Using left_join() to add SampleID information to results.df.wide
    # Define column names to remain as characters
    character_columns <- c("SampleID", "Location", "Location.2", "Sample", "Plate")
    # Join
    results.df.wide <- results.df.wide %>%
      dplyr::left_join(row_to_match, by = "Location.2") %>%
      dplyr::select(SampleID, everything()) %>%                               # Move SampleID to first column
      dplyr::mutate(across(all_of(character_columns), as.character)) %>%      # Convert specified columns to character
      dplyr::mutate(mutate(across(-all_of(character_columns), as.numeric)))   # Convert all other columns to numeric

    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- results.df.wide
  }

  final_results <- dplyr::bind_rows(results_all)

  # Write the results as a file
  write.csv(final_results, file = paste0(here::here(file_path), "LDH_MFI_RAU.csv"), row.names = F)

  # Return final file
  return(final_results)

}
