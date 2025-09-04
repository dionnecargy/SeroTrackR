#' Processing Serological Data for Pk/Pf/Pv MFI to RAU conversion
#'
#' This is a pre-requisite function before running the `MFItoRAU_Plasmo()` so that the
#' appropriate MFI to RAU conversions can be run for the respective antigens.
#'
#' @param serodata   Output of `readSeroData()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1".
#'
#' @return A list of two data frames:
#' 1. Data frame with Pk antigens
#' 2. Data frame with Pf/Pv antigens
#' @export
#'
#' @importFrom dplyr mutate across  select  pull filter case_when
#' @importFrom stringr str_extract  str_detect  str_detect
#' @importFrom zoo na.locf
#' @importFrom utils read.csv
#'
#' @author Dionne Argyropoulos
processPkPfPv <- function(serodata, plate_list, panel = "panel1"){

  #############################################################
  # Step 1: Collect Data Inputs for Function
  #############################################################
  master_file               <- serodata$results
  layout                    <- plate_list
  chosen_panel              <- panel

  #############################################################
  # Interim Pre-Processing Step for ZOOMAL project
  #############################################################
  L                         <- master_file %>%
    dplyr::mutate(
      dplyr::across(-c(Location, Sample, Plate), as.numeric),
      # Extract suffix (anything inside parentheses)
      suffix = str_extract(Sample, "\\s*\\([^\\)]+\\)"),
      # Carry forward last seen suffix
      suffix = zoo::na.locf(suffix, na.rm = FALSE),
      # Add suffix only to S-samples that don't already have one
      Sample = ifelse(str_detect(Sample, "^S\\d+$"), paste0(Sample, suffix), Sample)
    ) %>%
    dplyr::select(-suffix)

  #############################################################
  # Step 2: Filter for Pv/Pf and Pv Datasets
  #############################################################

  # Extract Reference IDs for Antigens
  if(panel == "panel1"){
    PkPfPv_Panel_1 <- system.file("extdata", "PkPfPv_Panel_1.csv", package = "SeroTrackR")
    pv_antigens <- read.csv(PkPfPv_Panel_1) %>% dplyr::filter(Species == "Pv") %>% dplyr::pull(Antigens)
    pf_antigens <- read.csv(PkPfPv_Panel_1) %>% dplyr::filter(Species == "Pf") %>% dplyr::pull(Antigens)
    pk_antigens <- read.csv(PkPfPv_Panel_1) %>% dplyr::filter(Species == "Pk") %>% dplyr::pull(Antigens)
  } else {
    pv_antigens <- panel %>% dplyr::filter(Species == "Pv") %>% dplyr::pull(Antigens)
    pf_antigens <- panel %>% dplyr::filter(Species == "Pf") %>% dplyr::pull(Antigens)
    pk_antigens <- panel %>% dplyr::filter(Species == "Pk") %>% dplyr::pull(Antigens)
  }

  # Convert User IDs for Antigens (Accounting for spelling errors/differences)
  # Function to relabel column names
  relabel_columns <- function(df) {
    colnames(df) <- dplyr::case_when(
      stringr::str_detect(colnames(df), regex("EBP", ignore_case = TRUE)) ~ "EBP",
      stringr::str_detect(colnames(df), regex("LF005", ignore_case = TRUE)) ~ "LF005", # Happy to relabel to PvLF005 or Pv-fam-a
      stringr::str_detect(colnames(df), regex("LF010", ignore_case = TRUE)) ~ "LF010", # Happy to relabel to PvLF010 or PvMSP5
      stringr::str_detect(colnames(df), regex("LF016", ignore_case = TRUE)) ~ "LF016", # Happy to relabel to PvLF016 or PvMSP1-19
      stringr::str_detect(colnames(df), regex("(MSP8|L34)", ignore_case = TRUE)) ~ "MSP8",
      stringr::str_detect(colnames(df), regex("(P87|RBP2b-P87)", ignore_case = TRUE)) ~ "RBP2b.P87",
      stringr::str_detect(colnames(df), regex("(PTEX|PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150", # Happy to relabel to PvPTEX150
      stringr::str_detect(colnames(df), regex("CSS", ignore_case = TRUE)) ~ "PvCSS",
      stringr::str_detect(colnames(df), regex("(MSP1-19|PfMSP1|MSP1.19)", ignore_case = TRUE)) ~ "PfMSP1-19",
      stringr::str_detect(colnames(df), regex("AMA1", ignore_case = TRUE)) ~ "PfAMA1",
      stringr::str_detect(colnames(df), regex("etramp5Ag1|tramp", ignore_case = TRUE)) ~ "Pfetramp5Ag1",
      stringr::str_detect(colnames(df), regex("HSP40Ag1", ignore_case = TRUE)) ~ "PfHSP40Ag1",
      stringr::str_detect(colnames(df), regex("Gexp18", ignore_case = TRUE)) ~ "PfGexp18",
      stringr::str_detect(colnames(df), regex("SSP2", ignore_case = TRUE)) ~ "PkSSP2",
      stringr::str_detect(colnames(df), regex("PkMSP10", ignore_case = TRUE)) ~ "PkMSP10",
      stringr::str_detect(colnames(df), regex("Pk8", ignore_case = TRUE)) ~ "Pk8",
      stringr::str_detect(colnames(df), regex("SERA3Ag2", ignore_case = TRUE)) ~ "PkSERA3Ag2",
      TRUE ~ colnames(df) # Keep unmatched names as-is
    )
    return(df)
  }

  L <- L %>% relabel_columns()

  PfPv <- L %>%
    # Step 1: Filter for only Pv/Pf-relevant standard curve
    dplyr::filter(!str_detect(Sample, "PK")) %>%
    # Step 2: Filter for only Pv/Pf-relevant antigens
    dplyr::select(Location, Sample, Plate, all_of(pv_antigens), all_of(pf_antigens))

  Pk <- L %>%
    # Step 1: Filter for only Pk-relevant standard curve
    dplyr::filter(!str_detect(Sample, "ETH|PNG|Global")) %>%
    # Step 2: Filter for only Pk-relevant antigens
    dplyr::select(Location, Sample, Plate, all_of(pk_antigens))

  return(list(PfPv = PfPv,  Pk = Pk))

}
