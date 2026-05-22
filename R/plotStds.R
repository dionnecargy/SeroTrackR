#' Plot Raw Median Fluorescent Intensity of Standard Curve Data
#'
#' This function gets the standards data and plots the standard curves.
#'
#' @param sero_data Output from `readSeroData()`.
#' @param std_point Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Default = 10. Value is an integer.
#' @param location "PNG" or "ETH" to filter WEHI standard curve data.
#' @param experiment_name User-input experiment name.
#' @return
#' - Dot and line plot of standard curves (S1-S10) with PNG or Ethiopia stds
#' underneath (ggplot).
#' - WEHI-acceptable standard curve data on background of plot with user data.
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr  pivot_longer
#' @import ggplot2
#' @export
#' @author Dionne Argyropoulos, Shazia Ruybal-Pesantez
#'
#' @examples
#' \donttest{
#' # Example demonstrating how to process bead count data.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data <- c(
#'   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' # Read in raw MAGPIX data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
#'
#' # Plot Standards
#' plotStds(
#'   sero_data = sero_data,
#'   location = "ETH",
#'   experiment_name = "experiment1"
#' )
#'
#' }
#'
plotStds <- function(sero_data, std_point = 10, location, experiment_name){

  # stratify data
  master_file <- sero_data
  stds <- master_file$stds

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19", "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  stds_1 <- stds %>%
    dplyr::select(-Location) %>%
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number
      Antigen = dplyr::recode(Antigen, !!!name_lookup),
      MFI = as.numeric(MFI)
    )

  if(std_point == 10){

    stds_1 <- stds_1 %>% dplyr::mutate(Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")))

    location_1 <- ifelse(location == "ETH", "ETH", "PNG")
    wehi_stds <- read.csv(system.file("extdata", "all_stds_MFI.csv", package = "SeroTrackR"))
    wehi_stds <- wehi_stds %>%
      dplyr::filter(Location==location_1) %>%
      mutate(Antigen = dplyr::recode(Antigen, !!!name_lookup))

    suppressMessages(
      suppressWarnings(
        ggplot2::ggplot() +
          ggplot2::geom_point(data = wehi_stds, aes(x = Sample, y = MFI), colour = "grey", alpha = 0.25) +
          ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate,
                                                 text = paste("Sample:", Sample, "<br>MFI:", MFI, "<br>Plate:", Plate))) +
          ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
          ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
          ggplot2::labs(
            x = "Standard Curve",
            y = "MFI",
            title = experiment_name
          ) +
          ggplot2::facet_wrap(~Antigen) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    )

  } else if (std_point == 5){

    stds_1 <- stds_1 %>% dplyr::mutate(Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5")))

    suppressMessages(
      suppressWarnings(
        ggplot2::ggplot() +
        ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate,
                                               text = paste("Sample:", Sample, "<br>MFI:", MFI, "<br>Plate:", Plate))) +
        ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
        ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
        ggplot2::labs(
          x = "Standard Curve",
          y = "MFI",
          title = experiment_name
        ) +
        ggplot2::facet_wrap(~Antigen) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    )

  } else {
    message("Please write a standard point curve.")
  }

}
#' Plot Raw Median Fluorescent Intensity of Standard Curve Data
#'
#' This function gets the standards data and plots the standard curves for any antigens (i.e., non-PvSeroTaT specific).
#'
#' @param sero_data Output from `readSeroData()`.
#' @param experiment_name User-input experiment name.
#' @return
#' - Dot and line plot of standard curves (S1-S10)
#' - WEHI-acceptable standard curve data on background of plot with user data.
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr  pivot_longer
#' @import ggplot2
#' @export
#' @author Shazia Ruybal-Pesantez, Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' # Example demonstrating how to process bead count data.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data <- c(
#'    system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#'    system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#'    system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
#' )
#'
#' # Read in raw MAGPIX data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
#'
#' # Plot Standards
#' plotStds_all(
#'   sero_data = sero_data,
#'   experiment_name = "experiment1"
#' )
#'
#' }
#'
plotStds_all <- function(sero_data, experiment_name){
  master_file <- sero_data
  stds <- master_file$stds

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  stds_1 <- stds %>%
    dplyr::select(-Location) %>%
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number
      Antigen = dplyr::recode(Antigen, !!!name_lookup),
      Sample = factor(Sample, levels = unique(Sample[order(as.numeric(str_extract(Sample, "\\d+")))])), # reorder by standard curve number
      MFI = as.numeric(MFI)
    )


  suppressMessages(
    suppressWarnings(
      ggplot() +
      ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
      ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::labs(
        x = "Standard Curve",
        y = "MFI",
        title = experiment_name
      ) +
      ggplot2::facet_wrap(~Antigen) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  )

}
#' Plot Raw Median Fluorescent Intensity of Pk/Pf/Pv Standard Curve Data
#'
#' This function gets the standards data and plots the standard curves for antigens in the Pk/Pf/Pv panel.
#'
#' @param sero_data Output from `readSeroData()`.
#' @param experiment_name User-input experiment name.
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1" or user provided csv of Antigens and Species.
#'
#' @return
#' - Dot and line plot of standard curves (S1-S10)
#' - WEHI-acceptable standard curve data on background of plot with user data.
#'
#' @import ggplot2
#' @importFrom dplyr select mutate filter left_join across
#' @importFrom tidyr  pivot_longer  separate
#' @importFrom stringr str_detect str_extract
#' @importFrom utils read.csv
#'
#' @export
#' @author Dionne Argyropoulos
#'
#' @examples
#' \donttest{
#' # Example demonstrating how to process bead count data.
#' # These files are included in the SeroTrackR package under inst/extdata.
#'
#' your_raw_data <- c(
#'    system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
#'    system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
#' )
#'
#' # Read in raw MAGPIX data
#' sero_data <- readSeroData(
#'   raw_data = your_raw_data,
#'   platform = "magpix"
#' )
#'
#' # Plot Standards
#' plotStds_PkPfPv(
#'   sero_data = sero_data,
#'   experiment_name = "experiment1",
#'   panel = "panel1"
#' )
#'
#' }
#'
plotStds_PkPfPv <- function(
    sero_data,
    experiment_name,
    panel = "panel1"
   ){

  # Check if shiny.fluent is installed
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required for plotStds_PkPfPv(). Please install it.", call. = FALSE)
  }

  #panel 1 is default

  if(panel == "panel1"){
    panel <-read.csv(system.file("extdata", "PkPfPv_Panel_1.csv", package = "SeroTrackR"))
  } else {
    panel <- read.csv(panel)
  }


  master_file <- sero_data
  stds <- master_file$stds

  # relabel antigen names from lab codes to proper antigen names
  old_names <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  new_names <- c("PvEBP", "Pv-fam-a", "PvMSP5", "PvMSP1-19",  "PvMSP8", "PvRBP2b", "PvPTEX150", "PvCSS")

  name_lookup <- setNames(new_names, old_names)

  stds <- stds %>%
    .relabel_columns()

  stds_1 <- stds %>%
  ################################################################
  # Code to be replaced in future when labelling Stds not an issue
  ################################################################
  dplyr::mutate(
    dplyr::across(-c(Location, Sample, Plate), as.numeric),
    suffix = str_extract(Sample, "\\s*\\([^\\)]+\\)"),
    suffix = zoo::na.locf(suffix, na.rm = FALSE),
    Sample = ifelse(str_detect(Sample, "^S\\d+$"), paste0(Sample, suffix), Sample)
  ) %>%
    dplyr::select(-suffix) %>%
  ################################################################
  # Code to keep
  ################################################################
  dplyr::select(-Location) %>%
    tidyr::separate(Sample, c("Sample", "Beads"), sep = "[ -]") %>%
    tidyr::pivot_longer(-c(Sample, Beads, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number
      Beads = stringr::str_extract(Beads, "[A-Z]+"), # ETH (ETH) may sometimes be entered differently (in brackets or not) - this should just keep the letters PK or ETH that we want
      Sample = factor(Sample, levels = unique(Sample[order(as.numeric(str_extract(Sample, "\\d+")))])), # reorder by standard curve number
      Antigen = dplyr::recode(Antigen, !!!name_lookup),
      MFI = as.numeric(MFI)
    ) %>%
    dplyr::left_join(panel, by = c("Antigen" = "Antigens")) %>%
    dplyr::mutate(Species = case_when(
      is.na(Species) & stringr::str_detect(Antigen, "Pv") ~ "Pv",
      is.na(Species) & stringr::str_detect(Antigen, "Pf") ~ "Pf",
      is.na(Species) & stringr::str_detect(Antigen, "Pk") ~ "Pk",
      T ~ Species
    )) %>%
    dplyr::mutate(stds_to_keep = case_when(
      Species=="Pk" & Beads == "PK" ~ "keep",
      Species=="Pf" & Beads == "ETH" ~ "keep",
      Species=="Pv" & Beads == "ETH" ~ "keep",
      .default = "remove"
    )) %>%
    dplyr::filter(stds_to_keep == "keep") %>%
    dplyr::select(-stds_to_keep)


  suppressMessages(
    suppressWarnings(
      ggplot() +
      ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
      ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::labs(x = "Standard Curve", y = "MFI", title = experiment_name) +
      ggplot2::facet_wrap(~Antigen) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  )

}
