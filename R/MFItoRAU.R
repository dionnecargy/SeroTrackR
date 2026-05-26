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
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#' @param project Default = NULL. Only write "pkpfpv" if using Pk/Pf/Pv pipeline.
#'
#' @return  A list of three data frames:
#' 1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#' 2. Plot information for `plotModel` function
#' 3. Data frame of RAU data for random forest classification use.
#'
#' @export
#'
#' @import drc
#' @importFrom dplyr distinct select inner_join bind_rows
#' @importFrom tidyselect matches
#' @importFrom purrr imap_dfr
#' @author Dionne Argyropoulos, Connie Li Wai Suen
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
#' # Step 3: Convert MFI to RAU
#' mfi_to_rau <- MFItoRAU(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   qc_results = qc_results
#' )
#'
#' }
MFItoRAU <- function(
    sero_data,
    plate_list,
    qc_results,
    std_point = 10,
    project = NULL
){

  if (is.null(project)) {
    df <- sero_data$results
  } else if (project == "pkpfpv") {
    df <- sero_data
  } else {
    df <- sero_data$results
  }

  setup <- .setup_mfitorau_inputs(
    df = df,
    plate_list = plate_list,
    std_point = std_point
  )

  L        <- setup$L
  layout   <- setup$layout
  antigens <- setup$antigens

  # unpack params
  dilution                      <- setup$params$dilution
  dilution_scaled               <- setup$params$dilution_scaled
  dilution_factor               <- setup$params$dilution_factor
  current_min_relative_dilution <- setup$params$current_min_relative_dilution
  s1_concentration              <- setup$params$s1_concentration
  s_final_concentration         <- setup$params$s_final_concentration

  counts_QC_output              <- qc_results$getCountsQC_output

  # LOG-LOG MODEL
  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    # Fetch the corresponding layout data frame
    current_layout <- layout[[plate_level]]

    # Initialize storage for results
    results.df.wide <- NULL
    model_list <- list()

    # Iterate over antigens
    for (i in antigens) {

      out <- .process_antigen_loglog(
        subset_data            = subset_data,
        antigen                = i,
        dilution               = dilution,
        s1_concentration       = s1_concentration,
        s_final_concentration  = s_final_concentration
      )

      model_list[[i]] <- out$model

      if (is.null(results.df.wide)) {
        results.df.wide <- out$results
      } else {
        results.df.wide <- merge(
          results.df.wide,
          out$results,
          by = c("Location", "Sample", "Plate")
        )
      }
    }

    # MODEL RESULTS AND PLOTS
    # # Plot models with plate in the title
    # model_results <- list()
    # for (i in names(model_list)) {
    #   title <- paste("Plate:", plate_level, "- Protein:", i)  # Combine plate and protein name
    #   model_results[[i]] <- plot(model_list[[i]], main = title)
    # }

    model_results <- list()

    for (i in names(model_list)) {

      model   <- model_list[[i]]
      rng     <- range(model$data$dilution, na.rm = TRUE)
      newx    <- exp(seq(log(rng[1]), log(rng[2]), length.out = 100))
      pred    <- suppressWarnings(as.numeric(predict(model, newdata = data.frame(dilution = newx))))

      model_results[[i]] <- data.frame(
        dilution = newx,
        log.std = pred
      )
    }

    # MERGE DATA
    results.df.wide <- .merge_mfitorau(
      df = results.df.wide,
      layout = layout,
      plate_level = plate_level
    )

    # OUTPUT
    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Plate|_MFI|\\_Dilution$", colnames(results.df.wide))
    MFI_RAU_results <- results.df.wide[, col_selection]

    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- results.df.wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results
  }

  # Return the final results tables with QC pass/fail
  counts_data <- counts_QC_output %>%
    ungroup() %>%
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")
  name_lookup <- setNames(new_names, old_names)

  # Join all results together and relabel antigen names for export
  final_results <- dplyr::bind_rows(results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Plate", "Location.2")) %>%
    dplyr::rename_with(function(col_names) {
      for (old in names(name_lookup)) {
        col_names <- str_replace(
          col_names,
          paste0("^", old, "(?=_)"),  # match only prefix before underscore
          name_lookup[[old]]
        )
      }
      col_names
    })

  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Plate")) %>%
    dplyr::rename_with(function(col_names) {
      for (old in names(name_lookup)) {
        col_names <- str_replace(
          col_names,
          paste0("^", old, "(?=_)"),  # match only prefix before underscore
          name_lookup[[old]]
        )
      }
      col_names
    })

  final_model_results_all <- purrr::imap_dfr(
    model_results_all,
    ~ purrr::imap_dfr(.x, ~ dplyr::mutate(.x, Antigen = .y), .id = "Antigen"),
    .id = "Plate"
  ) %>%
  dplyr::mutate(Antigen = dplyr::recode(Antigen, !!!name_lookup))

  # Output
  return(
    list(
      final_results,
      final_MFI_RAU_results,
      final_model_results_all
    )
  )

}
#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion based on other standard
#'
#' This function fits a 5-parameter logistic standard curve to the dilutions
#' of the positive controls for each protein and converts the MFI values
#' into relative antibody units (RAU) written by Eamon Conway.
#'
#' @param sero_data Output from `readSeroData()`.
#' @param plate_list Output from `readPlateLayout()`.
#' @param qc_results Output from `runQC()`.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#' @param project Default = NULL. Only write "pkpfpv" if using Pk/Pf/Pv pipeline.
#'
#' @return  A list of three data frames:
#' 1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#' 2. Plot information for `plotModel` function.
#' 3. Data frame of RAU data for random forest classification use.
#'
#' @export
#'
#' @importFrom dplyr group_by mutate across inner_join rowwise summarise right_join select left_join rename_with all_of ungroup
#' @importFrom tidyr nest unnest pivot_wider
#' @importFrom tidyselect matches
#' @importFrom purrr map
#' @author Eamon Conway, Dionne Argyropoulos
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
#'   sero_data    = sero_data,
#'   plate_list   = plate_list,
#'   qc_results   = qc_results
#' )
#'
#' }
MFItoRAU_Adj <- function(
    sero_data,
    plate_list,
    qc_results,
    std_point = 10,
    project = NULL
){

  if (is.null(project)) {
    df <- sero_data$results
  } else if (project == "pkpfpv") {
    df <- sero_data
  } else {
    df <- sero_data$results
  }

  setup <- .setup_mfitorau_inputs(
    df = df,
    plate_list = plate_list,
    std_point = std_point
  )

  L        <- setup$L
  layout   <- setup$layout
  antigens <- setup$antigens

  # unpack params
  dilution                      <- setup$params$dilution
  dilution_scaled               <- setup$params$dilution_scaled
  dilution_factor               <- setup$params$dilution_factor
  current_min_relative_dilution <- setup$params$current_min_relative_dilution
  s1_concentration              <- setup$params$s1_concentration
  s_final_concentration         <- setup$params$s_final_concentration

  counts_QC_output              <- qc_results$getCountsQC_output

  # Reference Fit
  refs <-read.csv(system.file("extdata", "png_eth_stds.csv", package = "SeroTrackR"))

  control = list(maxit = 10000, abstol = 1e-8, reltol = 1e-6)
  initial_solution = c(-1.0, 0.0, 10, 0.0, 0.0)

  ref_fit <- refs %>%
    dplyr::group_by(.data$std_plate, .data$antigen) %>%
    tidyr::nest()  %>%
    dplyr::mutate(
      .keep = "none",
      eth_fit = purrr::map(data, ~ {
        suppressMessages(
          suppressWarnings(
            .fit_standard_curve(.x$eth_mfi, .x$dilution, control)
          )
        )
      }),
      png_fit = purrr::map(data, ~ {
        suppressMessages(
          suppressWarnings(
            .fit_standard_curve(.x$png_mfi, .x$dilution, control)
          )
        )
      })
    )

  reference_antigens = unique(ref_fit$antigen)

  # Initialise outputs and prepare function by plate
  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    # Apply conversion
    eth_qa_sc <- subset_data %>%
      dplyr::filter(type.letter == "S") %>%
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>%
      dplyr::mutate(dilution = dilution_factor ^ (-as.numeric(gsub(
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
        .fit_standard_curve(.x$mfi, .x$dilution, control)
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
          dilution = .convert_mfi_to_dilution_no_bounds(mfi,new_fit, 0.0), # We do not want the initial conversion to have any bounds. There are some required due to asymptotes in the function however. (Eamon)
          ref_mfi = .convert_dilution_to_mfi(dilution,eth_fit),
          dilution = .convert_mfi_to_dilution(ref_mfi,png_fit, current_min_relative_dilution)
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

    # MODEL RESULTS AND PLOTS
    sc_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        suppressMessages(
          suppressWarnings(
            .fit_standard_curve(.x$mfi, .x$dilution, control)
          )
        )
      }))

    qa_converted <- dplyr::inner_join(sc_fit, eth_qa_sc) |>
      dplyr::rowwise() |>
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          Sample = .data$Sample,
          dilution = .data$dilution,
          mfi = .data$mfi,
          mfi_pred = .convert_dilution_to_mfi(.data$dilution, new_fit)
        )
      )) |>
      tidyr::unnest(cols = data)

    model_results <- qa_converted

    # MERGE DATA: Relabel Sample Names with Plate Layout
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

    eth_converted_wide <- .merge_mfitorau(
      df = eth_converted_wide,
      layout = layout,
      plate_level = plate_level
    )

    # Create output dataframes
    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Location.2|Plate|_MFI|\\_Dilution$", colnames(eth_converted_wide))
    MFI_RAU_results <- eth_converted_wide[, col_selection]

    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- eth_converted_wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results

  }

  # Joining all plate data
  counts_data <- counts_QC_output %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")
  name_lookup <- setNames(new_names, old_names)

  final_results <- dplyr::bind_rows(results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))

  final_model_results_all <- dplyr::bind_rows(model_results_all, .id = "Plate") %>%
    dplyr::mutate(antigen = dplyr::recode(antigen, !!!name_lookup))

  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>%
    dplyr::inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))

  # Re-arrange data for final outputs
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
    dplyr::select(all_of(final_results_order)) %>%
    dplyr::rename_with(function(col_names) {
      for (old in names(name_lookup)) {
        col_names <- str_replace(
          col_names,
          paste0("^", old, "(?=_)"),  # match only prefix before underscore
          name_lookup[[old]]
        )
      }
      col_names
    })

  final_MFI_RAU_results <- final_MFI_RAU_results %>%
    dplyr::select(all_of(final_MFI_RAU_order)) %>%
    dplyr::rename_with(function(col_names) {
      for (old in names(name_lookup)) {
        col_names <- str_replace(
          col_names,
          paste0("^", old, "(?=_)"),  # match only prefix before underscore
          name_lookup[[old]]
        )
      }
      col_names
    })

  return(
    list(
      final_results,
      final_MFI_RAU_results,
      final_model_results_all
    )
  )
}
#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for LDH
#'
#' This function fits a 5-parameter logistic standard curve to the dilutions
#' of the positive controls for each protein and converts the MFI values
#' into relative antibody units (RAU).
#'
#' @param sero_data Output from `readSeroData()` or `readSeroData()`.
#' @param plate_list Output from `readPlateLayout()`.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = "PvLDH".
#' @param file_path A file path to write the .csv final file. Default: Current working directory.
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
#'
#' @examples
#' \donttest{
#' # Example demonstrating multi-plate processing workflow.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data <- system.file("extdata", "example_BioPlex_PvLDH_plate1.xlsx", package = "SeroTrackR")
#'
#' your_plate_layout <- system.file(
#'   "extdata", "example_platelayout_1.xlsx",
#'   package = "SeroTrackR"
#' )
#'
#' # Read in raw BioPlex data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "bioplex"
#' )
#'
#' # Read matching plate layout
#' plate_list <- readPlateLayout(
#'   plate_layout = your_plate_layout,
#'   sero_data = sero_data
#' )
#'
#' # Run MFI to RAU conversion
#' mfi_outputs <- MFItoRAU_LDH(
#'   sero_data = sero_data,
#'   plate_list = plate_list
#' )
#'
#' # View All Outputs
#' mfi_outputs
#' }
MFItoRAU_LDH <- function(
    sero_data,
    plate_list,
    std_point = "PvLDH",
    file_path = NULL
){

  # Step 1: Read raw serology data and plate layout
  setup <- .setup_mfitorau_inputs(
    df = sero_data$results,
    plate_list = plate_list,
    std_point = "PvLDH"
  )

  L        <- setup$L
  layout   <- setup$layout
  antigens <- setup$antigens

  # unpack params
  dilution                      <- setup$params$dilution
  s1_concentration              <- setup$params$s1_concentration
  s_final_concentration         <- setup$params$s_final_concentration

  results.df.wide <- NULL

  # Step 2: Perform LOG-LOG MODEL
  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates

  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]

    # Fetch the corresponding layout data frame
    current_layout <- layout[[plate_level]]

    results.df.wide <- NULL
    model_list <- list()

    for (i in antigens) {

      out <- .process_antigen_loglog(
        subset_data           = subset_data,
        antigen               = i,
        dilution              = dilution,
        s1_concentration      = s1_concentration,
        s_final_concentration = s_final_concentration,
        unknown_letters       = c("U", "X")
      )

      model_list[[i]] <- out$model

      if (is.null(results.df.wide)) {
        results.df.wide <- out$results
      } else {
        results.df.wide <- merge(
          results.df.wide,
          out$results,
          by = c("Location", "Sample", "Plate")
        )
      }
    }

    # STEP 4: MERGE DATA with plate layout
    # Create "Location.2" Variable which has the row and column ID only
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

    # Create plate layout to bind to "results.df.wide"
    # 1. Parse L$Location into well IDs
    location.2 <- dplyr::tibble(Location = subset_data$Location) %>%
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

    # Using left_join() to add SampleID information to results.df.wide
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

  # Return final file
  return(final_results)

}
#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for Pk/Pf/Pv Master Function
#'
#' This function leverages `MFItoRAU_Pk()` and `MFItoRAU()` to create a final MFI to RAU
#' output for Pk/Pf/Pv analyses.
#'
#' @param sero_data   Output of `readserodata_output()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1" or user provided csv of Antigens and Species.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#' @param qc_results Output from `runQC()`.
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
#' @author Dionne Argyropoulos, Caitlin Bourke
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
#' qc_results  <- runQC(sero_data, plate_list)
#'
#' # Run MFI to RAU conversion
#' mfi_outputs               <- MFItoRAU_Plasmo(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   panel = "panel1",
#'   std_point = 5,
#'   qc_results = qc_results
#' )
#'
#' # View All Outputs
#' mfi_outputs
#' }
MFItoRAU_Plasmo <- function(
    sero_data,
    plate_list,
    panel = "panel1",
    std_point,
    qc_results
  ){

  processed_master    <- processPkPfPv(sero_data, plate_list, panel = panel)
  processed_PfPv      <- processed_master$PfPv
  processed_Pk        <- processed_master$Pk

  counts_QC_output    <- qc_results$getCountsQC_output

  # Pfk MFI to RAU processing pipeline
  Pk_Final            <- MFItoRAU_Pk(
      processed_Pk = processed_Pk,
      plate_list = plate_list,
      std_point = std_point,
      qc_results = qc_results
    )

  # Pf/Pv MFI to RAU processing pipeline
  PfPv_Final          <- suppressWarnings(
    MFItoRAU(
      sero_data = processed_PfPv,
      plate_list = plate_list,
      project = "pkpfpv",
      std_point = std_point,
      qc_results = qc_results
    )
  )
  PfPv_Adj_Final      <- suppressMessages(
    MFItoRAU_Adj(
      sero_data = processed_PfPv,
      plate_list = plate_list,
      project = "pkpfpv",
      std_point = std_point,
      qc_results = qc_results
    )
  )

  # Relabel _Dilution to specify
  PfPv_Final[[1]] <- PfPv_Final[[1]] %>% rename_with(~ gsub("_Dilution$", "_loglog_Dilution", .x))
  PfPv_Final[[2]] <- PfPv_Final[[2]] %>% rename_with(~ gsub("_Dilution$", "_loglog_Dilution", .x))

  PfPv_Adj_Final[[1]] <- PfPv_Adj_Final[[1]] %>% rename_with(~ gsub("_Dilution$", "_Adjloglog_Dilution", .x)) %>%
    dplyr::mutate(across(contains("Adjloglog"), ~if_else(.x<1.95e-05, 1.95e-05, .x))) # catch any values below 1.95e-05 (S11 min)
  PfPv_Adj_Final[[2]] <- PfPv_Adj_Final[[2]] %>% rename_with(~ gsub("_Dilution$", "_Adjloglog_Dilution", .x)) %>%
    dplyr::mutate(across(contains("Adjloglog"), ~if_else(.x<1.95e-05, 1.95e-05, .x)))# catch any values below 1.95e-05 (S11 min)


  # Join Dataframes Together
  pk_final_results            <- Pk_Final
  pfpv_final_results          <- PfPv_Final[[1]]
  pfpv_Adj_final_results      <- PfPv_Adj_Final[[1]]

  PkPfPv_Final <- suppressMessages(
    pk_final_results %>%
      left_join(pfpv_final_results, by = c("SampleID", "Location.2", "Location", "Sample", "Plate", "QC_total")) %>%
      left_join(pfpv_Adj_final_results))
  PkPfPv_Final_MFI_RAU <- PkPfPv_Final %>%
    dplyr::select(SampleID, Plate, ends_with("_MFI", ignore.case = FALSE), ends_with("_Dilution", ignore.case = FALSE))

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")
  name_lookup <- setNames(new_names, old_names)

  names(pfpv_Adj_final_results) <- vapply(
    names(pfpv_Adj_final_results),
    function(col) {
      for (old in names(name_lookup)) {
        col <- sub(paste0("^", old), name_lookup[[old]], col)
      }
      col
    },
    character(1)
  )

  # Add panel
  if(panel == "panel1"){
    panel <-read.csv(system.file("extdata", "PkPfPv_Panel_1.csv", package = "SeroTrackR"))
    panel <- panel %>%
      dplyr::mutate(Antigens = dplyr::recode(Antigens, !!!name_lookup))
  } else {
    panel <- read.csv(panel)
  }

  # Create long df for downstream analyses (clean)
  PkPfPv_long_mfi <- PkPfPv_Final_MFI_RAU %>%
    dplyr::select(-ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigens", values_to = "MFI") %>%
    dplyr::left_join(panel, by = "Antigens")
  PkPfPv_long_rau <- suppressWarnings(
    PkPfPv_Final_MFI_RAU %>%
      dplyr::select(-ends_with("_MFI")) %>%
      dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
      tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigens", values_to = "RAU") %>%
      tidyr::separate(Antigens, c("Antigens", "Beads"), "_") %>%
      dplyr::left_join(panel, by = "Antigens")) %>%
    dplyr::mutate(RAU_Method = case_when(
      Beads == "loglog" ~ "loglog",
      Beads == "Adjloglog" ~ "Adj_loglog",
      .default = "loglog")
    ) %>% dplyr::select(-Beads)

  PkPfPv_long_mfi_rau <- suppressWarnings(
    PkPfPv_long_mfi %>%
      right_join(PkPfPv_long_rau, by = c("SampleID", "Plate", "Antigens", "Species")) %>%
      dplyr::mutate(Species = case_when(
        is.na(Species) & stringr::str_detect(Antigens, "Pv") ~ "Pv",
        is.na(Species) & stringr::str_detect(Antigens, "Pf") ~ "Pf",
        is.na(Species) & stringr::str_detect(Antigens, "Pk") ~ "Pk",
        T ~ Species

      ))
  ) %>%
    dplyr::select(SampleID, Plate, Antigens, Species, MFI, RAU, RAU_Method)

  return(list(All_Results = PkPfPv_Final, MFI_RAU = PkPfPv_Final_MFI_RAU, MFI_RAU_long = PkPfPv_long_mfi_rau))

}
#' Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU)
#' conversion for Pk proteins
#'
#' This function is utilised in the master function `MFItoRAU_Plasmo()`.
#'
#' @param processed_Pk  df$Pk of output `processPkPfPv()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#' @param qc_results Output from `runQC()`.
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
MFItoRAU_Pk <- function(processed_Pk, plate_list, std_point, qc_results){

  setup <- .setup_mfitorau_inputs(
    df = processed_Pk,
    plate_list = plate_list,
    std_point = std_point
  )

  L        <- setup$L
  layout   <- setup$layout
  antigens <- setup$antigens

  # unpack params
  dilution                      <- setup$params$dilution
  dilution_scaled               <- setup$params$dilution_scaled
  dilution_factor               <- setup$params$dilution_factor
  current_min_relative_dilution <- setup$params$current_min_relative_dilution
  s1_concentration              <- setup$params$s1_concentration
  s_final_concentration         <- setup$params$s_final_concentration

  counts_QC_output              <- qc_results$getCountsQC_output

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

  # Step 4: Final Results
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
#' Helper function to set up MFI to RAU function
#'
#' @param df  Output from `readSeroData()`.
#' @param plate_list Output from `readPlateLayout()`.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH" for LDH specific curve. Default = 10. Value is an integer.
#'
#' @returns A list of processed sero_data, processed plate layout, antigen names, and parameters for standard curve.
#' @export
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
#' # Step 2: Setup MFI to RAU
#' setup <- .setup_mfitorau_inputs(
#'   df = sero_data$results,
#'   plate_list = plate_list,
#'   std_point = 10
#' )
#'
#' }
#'
#' @author Dionne Argyropoulos
.setup_mfitorau_inputs <- function(df, plate_list, std_point) {

  # Ensure MFI columns are numeric
  L <- df %>% dplyr::mutate(dplyr::across(-c(Location, Sample, Plate), as.numeric))
  # Load plate list
  layout <- plate_list

  # Identify antigens
  remaining_cols <- setdiff(colnames(L), c("Location", "Sample", "Plate"))
  antigens <- remaining_cols[remaining_cols != ""]

  # Categorises into "B" = "Blank", "S" = "Standards", "U" or "X" = "Samples"
  L$type.letter <- substr(L$Sample, start=1, stop=1)

  # Magic Parameters for 5-point and 10-point standard curve
  params <- switch(
    as.character(std_point),

    "5" = {
      dilution <- c(1/50, 1/(50*5^1), 1/(50*5^2), 1/(50*5^3), 1/(50*5^4))

      list(
        dilution = dilution,
        dilution_scaled = dilution * (50*5^4),
        dilution_factor = 5,
        current_min_relative_dilution = 5.0^-5,
        s1_concentration = 1/50,
        s_final_concentration = 1/51200
      )
    },
    "10" = {
      dilution <- c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)

      list(
        dilution = dilution,
        dilution_scaled = dilution * 25600,
        dilution_factor = 2,
        current_min_relative_dilution = 2.0^-10,
        s1_concentration = 1/50,
        s_final_concentration = 1/51200
      )
    },
    "PvLDH" = {
      dilution <- c(1000000, 333333.33, 111111.11, 37037.04, 12345.68, 4115.23, 1371.74, 457.25, 152.42, 50.81)

      list(
        dilution = dilution,
        dilution_scaled = NULL,
        dilution_factor = NULL,
        current_min_relative_dilution = NULL,
        s1_concentration = 1000000, # Set observations with very high MFI to 1,000,000 pg/ml.
        s_final_concentration = 16.94 # Set observations with very low MFI to 16.94 pg/ml.
      )
    },
    stop("No standard curve points provided.")
  )

  # Return everything
  list(
    L = L,
    layout = layout,
    antigens = antigens,
    params = params
  )
}
#' Helper function to fit a 5-parameter logistic standard curve to dilutions
#'
#' @param subset_data Data for one plate.
#' @param antigen Data for one antigen.
#' @param dilution Set of five or ten.
#' @param s1_concentration  Concentration of highest dilution.
#' @param s_final_concentration Concentration lowest dilution.
#' @param unknown_letters Bioplex, Magpix or Intelliflex known unknown letters (Default = U and X).
#'
#' @returns A list of the model results data frame and model.
#' @export
#'
#' @author Connie Li Wai Suen, Dionne Argyropoulos
.process_antigen_loglog <- function(
    subset_data,
    antigen,
    dilution,
    s1_concentration,
    s_final_concentration,
    unknown_letters = c("U", "X")
) {

  ## Extract standards
  std <- subset_data %>%
    filter(type.letter == "S") %>%
    pull({{ antigen }})

  std <- as.numeric(std)
  std[is.na(std) | std == 0] <- 1
  log.std <- log(std)

  ## Fit 5PL model
  model <- suppressWarnings(drc::drm(
    log.std ~ dilution,
    fct = drc::LL.5(names = c("b", "c", "d", "e", "f"))
  ))

  coefs <- coef(model)
  b <- coefs[1]
  c <- coefs[2]
  d <- coefs[3]
  e <- coefs[4]
  f <- coefs[5]

  ## Process unknowns
  results.df <- NULL

  for (r in seq_len(nrow(subset_data))) {

    if (subset_data$type.letter[r] %in% unknown_letters) {

      mfi.X <- as.numeric(subset_data[r, antigen])
      y <- log(mfi.X)

      dil.X <- if (y > max(log.std)) {
        max(dilution)
      } else {
        e * ((((d - c) / (y - c))^(1 / f) - 1)^(1 / b))
      }

      ## Bounds
      dil.X <- ifelse(dil.X > s1_concentration, s1_concentration, dil.X)
      dil.X <- ifelse(is.na(dil.X) & y > log.std[2], s1_concentration, dil.X)
      dil.X <- ifelse(dil.X < s_final_concentration, s_final_concentration, dil.X)
      dil.X <- ifelse(is.na(dil.X) & y < max(log.std), s_final_concentration, dil.X)

      results <- data.frame(
        Location = subset_data[r, "Location"],
        Sample   = subset_data[r, "Sample"],
        Plate    = subset_data[r, "Plate"],
        MFI      = mfi.X,
        Dilution = dil.X,
        DilutionReciprocal = 1 / dil.X,
        MinStd       = min(std),
        MaxDilution  = min(dilution),
        MaxStd       = max(std),
        MinDilution  = max(dilution),
        stringsAsFactors = FALSE
      )

      colnames(results) <- c(
        "Location", "Sample", "Plate",
        paste0(
          antigen, "_",
          c(
            "MFI", "Dilution", "DilutionReciprocal",
            "MinStd", "MaxDilution", "MaxStd", "MinDilution"
          )
        )
      )

      results.df <- rbind(results.df, results)
    }
  }

  list(
    results = results.df,
    model = model
  )
}
#' Helper function to add Sample IDs to output.
#'
#' @param df Data frame following 5-parameter logistic function applied.
#' @param layout Output from `readPlateLayout()`.
#' @param plate_level Specific plate of interest.
#'
#' @returns Processed data frame with correct Sample IDs.
#' @export
#'
#' @author Dionne Argyropoulos
.merge_mfitorau <- function(df, layout, plate_level) {

  # Add Location.2
  df <- df %>%
    dplyr::mutate(
      Location.2 = stringr::str_split_fixed(as.character(Location), ",", 2)[, 2],
      Location.2 = stringr::str_trim(Location.2),
      Location.2 = stringr::str_sub(Location.2, 1, -2)
    ) %>%
    dplyr::select(Location.2, dplyr::everything())

  # Prepare plate layout
  plate_layout_current <- layout[[plate_level]] %>%
    dplyr::rename(Plate = 1) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::matches("^[0-9]+$"), as.character)
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("^[.x]*[0-9]+$"),
      names_to = "numeric",
      values_to = "SampleID"
    ) %>%
    dplyr::rename(alpha = Plate) %>%
    tidyr::unite("Location.2", alpha:numeric, sep = "", na.rm = TRUE)

  # Join SampleID
  df <- df %>%
    dplyr::left_join(plate_layout_current, by = "Location.2") %>%
    dplyr::distinct(SampleID, Location.2, .keep_all = TRUE) %>%
    tidyr::drop_na() %>%
    dplyr::select(SampleID, dplyr::everything())

  # Coerce column types
  character_columns <- c("SampleID", "Location", "Location.2", "Sample", "Plate")

  df <- df %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(character_columns), as.character),
      dplyr::across(!dplyr::all_of(character_columns), as.numeric)
    )

  return(df)
}
#' Convert known dilution to mfi from fitted standard curve
#' @description
#' Convert dilution to predicted mfi using known standard curve fit.
#'
#' @param dilution Known dilution of samples
#' @param params Known parameters for five parameter logistic fit.
#' @return Returns the predicted mfi of a sample with known dilution.
#' @export
#' @author Eamon Conway
#'
#' @examples
#' # This function is typically called internally by higher-level workflows.
#' # Below is a minimal runnable example using dummy parameters.
#'
#' # Five-parameter logistic model typically expects parameters in the order:
#' # a, b, c, d, e  (e often log-transformed)
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01))
#'
#' # Example dilution value
#' dilution_example <- 0.1
#'
#' # Predict MFI from the dummy standard curve
#' .convert_dilution_to_mfi(dilution_example, dummy_params)
#'
.convert_dilution_to_mfi <- function(dilution, params) {
  if (is.null(dilution) || is.null(params)) {
    error("Require both mfi and params to run.")
  }
  exp(.log_logistic_5p(dilution, params[1], params[2], params[3], params[4], exp(params[5])))
}
#' Convert mfi to dilution using known standard curve fit and no bounds
#' @description
#' Convert mfi to dilution using known standard curve fit and no bounds unless you are below the asymptote of the standard curve.
#' In this situation we set your value to min_relative_dilution. I dunno argue?
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1.
#' @return Returns the dilution of each sample in mfi.
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is generally called inside higher-level analysis workflows.
#' # Below is a minimal self-contained example using dummy values.
#'
#' # Dummy five-parameter logistic fit parameters:
#' # a, b, c, d, e  (with e typically supplied on the log scale)
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01))
#'
#' # Example MFI value
#' mfi_example <- 1500
#'
#' # Minimum relative dilution from the standard curve
#' min_rel_dil <- 1
#'
#' # Convert MFI to dilution without bounds
#' .convert_mfi_to_dilution_no_bounds(mfi_example, dummy_params, min_rel_dil)
.convert_mfi_to_dilution_no_bounds <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  y[y >= (params[2] + params[3])] <- 0.999*(params[2] + params[3])
  result <- .inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y < params[2]] <- min_relative_dilution
  return(result)
}
#' Convert mfi to dilution using known standard curve fit and no lower bound
#' @description
#' Convert mfi to dilution using known standard curve fit and no lower bound unless you are below the asymptote of the standard curve.
#' In this situation we set your value to min_relative_dilution. I dunno argue?
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1.
#' @return Returns the dilution of each sample in mfi.
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is usually called inside higher-level analysis steps.
#' # Below is a minimal runnable example using dummy values.
#'
#' # Dummy five-parameter logistic fit parameters:
#' # a, b, c, d, e  (with e typically on the log scale)
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01), f = 0, g = 5)
#'
#' # Example MFI value
#' mfi_example <- 1500
#'
#' # Minimum relative dilution from the standard curve
#' min_rel_dil <- 1
#'
#' # Convert MFI to dilution without applying a lower bound
#' .convert_mfi_to_dilution_no_lower_bound(mfi_example, dummy_params, min_rel_dil)
.convert_mfi_to_dilution_no_lower_bound <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  result <- .inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y > (params[2] + params[3])] <- 1.0
  result[y < params[2]] <- min_relative_dilution
  result[y > params[7]] <- 1.0
  # I dont think this will happen - Eamon (ask if needed)
  result[result > 1.0] <- 1.0
  return(result)
}
#' Convert mfi to dilution using known standard curve fit.
#' @description
#' Convert mfi to dilution using known standard curve fit.
#'
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1.
#' @return Returns the dilution of each sample in mfi.
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is typically used within larger analysis pipelines.
#' # Below is a minimal runnable example using dummy values.
#'
#' # Dummy five-parameter logistic fit parameters:
#' # a, b, c, d, e  (with e on the log scale)
#' # Additional placeholders (f, g) included so params[6] and params[7] exist.
#' dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01),
#'                   f = -5, g = 5)
#'
#' # Example MFI value
#' mfi_example <- 1500
#'
#' # Minimum relative dilution allowed
#' min_rel_dil <- 1
#'
#' # Convert MFI to dilution
#' .convert_mfi_to_dilution(mfi_example, dummy_params, min_rel_dil)
.convert_mfi_to_dilution <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  result <- .inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y > (params[2] + params[3])] <- 1.0
  result[y < params[2]] <- min_relative_dilution
  result[y < params[6]] <- min_relative_dilution
  result[y > params[7]] <- 1.0
  # I dont think this will happen - Eamon (ask if needed)
  result[result > 1.0] <- 1.0
  return(result)
}
#' Fit a standard curve to known mfi and dilution values.
#' @description
#' We wish to convert the standard curve samples to a five parameter logistic curve.
#' This function takes those values and calls optim to determine the fit.
#'
#' @param mfi Known mfi of samples
#' @param dilution Known dilution of samples
#' @param control Optional list of control parameters for the underlying call to optim.
#'
#' @return standard curve log logistic
#' @export
#' @author Eamon Conway
#' @examples
#' # This function is typically called within data-processing workflows.
#' # Workflow-style example (not run on CRAN)
#'
#' \donttest{
#'
#' # This block demonstrates how .fit_standard_curve() is typically used
#' # inside the MFItoRAU_Adj-conversion pipeline.
#'
#' # Step 1 — Prepare master file (normally from readSeroData)
#' master_file <- data.frame(
#'   Location = c("A1","A2","A3"),
#'   Sample   = c("S1","S2","S3"),
#'   Plate    = c("Plate1","Plate1","Plate1"),
#'   Ag1 = c(12000, 8000, 4000),
#'   Ag2 = c(9000,  5000, 2500)
#' )
#'
#' # Convert antigen columns to numeric
#' L <- master_file |>
#'   dplyr::mutate(dplyr::across(-c(Location, Sample, Plate), as.numeric))
#'
#' # Fake plate layout (normally from readPlateLayout)
#' layout <- list(Plate1 = data.frame(Location = c("A1","A2","A3"), WellType = "STD"))
#'
#'
#' # Step 2 — Load reference standard curve MFI values (dummy data)
#' refs <- data.frame(
#'   std_plate = rep("StdPlate1", 5),
#'   antigen   = rep("Ag1", 5),
#'   dilution  = c(1, 1/2, 1/4, 1/8, 1/16),
#'   eth_mfi   = c(14000, 7000, 3500, 1800, 900),
#'   png_mfi   = c(15000, 7600, 3800, 1900, 950)
#' )
#'
#'
#' # Step 3 — Define optimisation settings
#' control <- list(
#'   maxit  = 10000,
#'   abstol = 1e-8,
#'   reltol = 1e-6
#' )
#'
#'
#' # Step 4 — Fit ETH and PNG curves per standard-plate × antigen
#' ref_fit <- refs |>
#'   dplyr::group_by(.data$std_plate, .data$antigen) |>
#'   tidyr::nest() |>
#'   dplyr::mutate(
#'     eth_fit = purrr::map(data, ~ .fit_standard_curve(.x$eth_mfi, .x$dilution, control)),
#'     png_fit = purrr::map(data, ~ .fit_standard_curve(.x$png_mfi, .x$dilution, control))
#'   )
#'
#' ref_fit
#' }
.fit_standard_curve <- function(mfi, dilution, control = NULL) {
  if (is.null(mfi) | is.null(dilution)) {
    error("Require both mfi and dilution to run.")
  }

  y1 <- log(mfi)
  initial_solution <- c(-1.0, 0.0, max(y1), 0.0, 0.0)

  .error_func <- function(x) {
    f1 <- .log_logistic_5p(dilution, x[1], x[2], x[3], x[4], exp(x[5]))
    sum((y1 - f1)^2.0)
  }

  solution <- optim(par = initial_solution, fn = .error_func, control = control)
  if (solution$convergence != 0) {
    stop("Standard curve failed to converge. Look at data and possibly change control parameters from default.")
  }
  c(solution$par, min(y1), max(y1))
}

.inverse_log_logistic_5p <- function(y,b,c,d,e,f){
  A <- (d/(y-c))^(1/f)-1
  return(exp(-e) *A^(1/b))
}

.log_logistic_5p <- function(x, b, c, d, e, f) {
  return(c + d / (1.0 + exp(b * (log(x) + e)))^f)
}
