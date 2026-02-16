#' Plot Raw Median Fluorescent Intensity of Pk/Pf/Pv Standard Curve Data
#'
#' This function gets the standards data and plots the standard curves for antigens in the Pk/Pf/Pv panel.
#'
#' @param sero_data Output from `readSeroData()` (reactive).
#' @param experiment_name User-input experiment name (reactive).
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
#'   experiment_name = "experiment1"
#' )
#'
#' }
#'
plotStds_PkPfPv <- function(sero_data, experiment_name){

  # Check if shiny.fluent is installed
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required for plotStds_PkPfPv(). Please install it.", call. = FALSE)
  }

  #panel 1 is default - (future change to extdata?) else provides option for user specified option
  if(panel == "panel1"){
    panel <- read.csv(url("https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/PkPfPv_Panel_1.csv"))

  } else {
    panel <- read.csv(panel)
  }

  master_file <- sero_data
  stds <- master_file$stds

  relabel_columns <- function(df) {
    colnames(df) <- dplyr::case_when(
      stringr::str_detect(colnames(df), regex("EBP", ignore_case = TRUE)) ~ "EBP",
      stringr::str_detect(colnames(df), regex("LF005", ignore_case = TRUE)) ~ "LF005", # Happy to relabel to PvLF005 or Pv-fam-a
      stringr::str_detect(colnames(df), regex("LF010", ignore_case = TRUE)) ~ "LF010", # Happy to relabel to PvLF010 or PvMSP5
      stringr::str_detect(colnames(df), regex("LF016", ignore_case = TRUE)) ~ "LF016", # Happy to relabel to PvLF016 or PvMSP1-19
      stringr::str_detect(colnames(df), regex("(MSP8|L34)", ignore_case = TRUE)) ~ "MSP8",
      stringr::str_detect(colnames(df), regex("(P87|RBP2b-P87|RBP2b)", ignore_case = TRUE)) ~ "RBP2b.P87",
      stringr::str_detect(colnames(df), regex("(PTEX|PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150", # Happy to relabel to PvPTEX150
      stringr::str_detect(colnames(df), regex("CSS", ignore_case = TRUE)) ~ "PvCSS",
      stringr::str_detect(colnames(df), regex("(PfMSP1-19|PfMSP1|PfMSP1.19)", ignore_case = TRUE)) ~ "PfMSP1-19",
      stringr::str_detect(colnames(df), regex("PfAMA1", ignore_case = TRUE)) ~ "PfAMA1",
      stringr::str_detect(colnames(df), regex("Pfetramp5Ag1|Pfetramp", ignore_case = TRUE)) ~ "Pfetramp5Ag1",
      stringr::str_detect(colnames(df), regex("PfHSP40Ag1", ignore_case = TRUE)) ~ "PfHSP40Ag1",
      stringr::str_detect(colnames(df), regex("PfGexp18", ignore_case = TRUE)) ~ "PfGexp18",
      stringr::str_detect(colnames(df), regex("PkSSP2", ignore_case = TRUE)) ~ "PkSSP2",
      stringr::str_detect(colnames(df), regex("PkMSP10", ignore_case = TRUE)) ~ "PkMSP10",
      stringr::str_detect(colnames(df), regex("Pk8", ignore_case = TRUE)) ~ "Pk8",
      stringr::str_detect(colnames(df), regex("SERA3Ag2", ignore_case = TRUE)) ~ "PkSERA3Ag2",
      TRUE ~ colnames(df) # Keep unmatched names as-is
    )
    return(df)
  }

  stds <- stds %>% relabel_columns()

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
    tidyr::separate(Sample, c("Sample", "Beads"), sep = " ") %>%
    tidyr::pivot_longer(-c(Sample, Beads, Plate), names_to = "Antigen", values_to = "MFI") %>%
    dplyr::mutate(
      Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number
      Beads = stringr::str_extract(Beads, "(?<=\\().+?(?=\\))"),
      Sample = factor(Sample, levels = unique(Sample[order(as.numeric(str_extract(Sample, "\\d+")))])), # reorder by standard curve number
      MFI = as.numeric(MFI)
    ) %>%
    dplyr::left_join(PkPfPv_Panel_1, by = c("Antigen" = "Antigens")) %>%
    dplyr::mutate(stds_to_keep = case_when(
      Species=="Pk" & Beads == "PK" ~ "keep",
      Species=="Pf" & Beads == "ETH" ~ "keep",
      Species=="Pv" & Beads == "ETH" ~ "keep",
      .default = "remove"
    )) %>%
    dplyr::filter(stds_to_keep == "keep") %>%
    dplyr::select(-stds_to_keep)

  ggplot() +
    ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
    ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) +
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::labs(x = "Standard Curve", y = "MFI", title = experiment_name) +
    ggplot2::facet_wrap(~Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
