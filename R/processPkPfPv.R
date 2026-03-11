#' Processing Serological Data for Pk/Pf/Pv MFI to RAU conversion
#'
#' This is a pre-requisite function before running the `MFItoRAU_Plasmo()` so that the
#' appropriate MFI to RAU conversions can be run for the respective antigens.
#'
#' @param sero_data   Output of `readSeroData()`
#' @param plate_list  Output of `readPlateLayout()`
#' @param panel Panel of Pk/Pf/Pv antigens. Default = "panel1" or user provided csv of Antigens and Species.
#'
#' @return A list of two data frames:
#' 1. Data frame with Pk antigens
#' 2. Data frame with Pf/Pv antigens
#' @export
#'
#' @importFrom dplyr mutate across  select  pull filter case_when
#' @importFrom stringr str_extract  str_detect  str_detect
#' @importFrom utils read.csv
#'
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
#'
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
#'
#' # Process multi-species panel
#' processed_master <- processPkPfPv(
#'   sero_data = sero_data,
#'   plate_list = plate_list,
#'   panel = "panel1"
#' )
#' }
processPkPfPv <- function(sero_data, plate_list, panel = "panel1"){

  # Check if shiny.fluent is installed
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required for processPkPfPv(). Please install it.", call. = FALSE)
  }

  #############################################################
  # Step 1: Collect Data Inputs for Function
  #############################################################
  master_file               <- sero_data$results
  layout                    <- plate_list
  chosen_panel              <- panel

  #############################################################
  # Interim Pre-Processing Step for Pk project
  #############################################################
  L                         <- master_file %>%
    dplyr::mutate(
      dplyr::across(-c(Location, Sample, Plate), as.numeric),
      # Extract suffix (anything inside parentheses)
      suffix = stringr::str_extract(Sample, "\\s*\\([^\\)]+\\)"),
      # Carry forward last seen suffix
      suffix = zoo::na.locf(suffix, na.rm = FALSE),
      # Add suffix only to S-samples that don't already have one
      Sample = ifelse(stringr::str_detect(Sample, "^S\\d+$"), paste0(Sample, suffix), Sample)
    ) %>%
    dplyr::select(-suffix)

  #############################################################
  # Step 2: Filter for Pv/Pf and Pv Datasets
  #############################################################

  # Extract Reference IDs for Antigens
  if(panel == "panel1"){
    PkPfPv_Panel_1 <- read.csv(url("https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/PkPfPv_Panel_1.csv"))
    pv_antigens <- PkPfPv_Panel_1 %>% dplyr::filter(Species == "Pv") %>% dplyr::pull(Antigens)
    pf_antigens <- PkPfPv_Panel_1 %>% dplyr::filter(Species == "Pf") %>% dplyr::pull(Antigens)
    pk_antigens <- PkPfPv_Panel_1 %>% dplyr::filter(Species == "Pk") %>% dplyr::pull(Antigens)
  } else {
    panel <- read.csv(panel)
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
