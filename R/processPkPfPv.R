#' Processing Serological Data for Pk/Pf/Pv MFI to RAU conversion
#'
#' This is a pre-requisite function before running the `MFItoRAU_Plasmo()` so that the
#' appropriate MFI to RAU conversions can be run for the respective antigens.
#'
#' @param serodata_output   Output of `readSeroData()`
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
processPkPfPv <- function(serodata_output, plate_list, panel = "panel1"){

  #############################################################
  # Step 1: Collect Data Inputs for Function
  #############################################################
  master_file               <- serodata_output$results
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
