#' Example Serological Dataset
#'
#' MFI values from the WEHI standard curves using both Papua New Guinean
#' and Ethiopian IgG coupled beads for standard curves.
#'
#' @format A data frame with 3860 rows and 4 variables:
#' \describe{
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{Location}{Bead coupling location for standards}
#'   \item{MFI}{Median Fluorescent Intensity}
#'   \item{Antigen}{Antigen name}
#' }
#' @source Mueller Lab, WEHI
"all_stds_MFI.csv"

#' Training Data Median Fluorescent Intensity (MFI)
#'
#' Metadata for the MFI of the training dataset across all eight antigens used
#' in the PvSeroTaT method.
#'
#' @format A data frame with 21,080 rows and 3 columns.
#' \describe{
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{MFI}{Median Fluorescent Intensity}
#'   \item{Antigen}{Antigen name}
#' }
#' @source Mueller Lab, WEHI
"longitudinal_MFI.csv"

#' Training Data Relative Antibody Units (RAU)
#'
#' Metadata for the RAU of the training dataset across all eight antigens used
#' in the PvSeroTaT method.
#'
#' @format A data frame with 21,080 rows and 3 columns.
#' \describe{
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{RAU}{Relative Antibody Units}
#'   \item{Antigen}{Antigen name}
#' }
#' @source Mueller Lab, WEHI
"longitudinal_RAU.csv"

#' 96 Well Plate Map
#'
#' 96 well plate map in a long format used for data wrangling.
#'
#' @format A data frame with 96 rows and 2 columns.
#' \describe{
#'   \item{Location}{Location on plate}
#'   \item{Well}{Well on plate}
#' }
"platemap.csv"

#' Plate Metadata
#'
#' MFI values from QA/QC plate with Papua New Guinean and Ethiopian IgG coupled beads
#' for a range of the eight antigens in the PvSeroTaT method.
#'
#' @format A data frame with 710 rows and 6 columns.
#' \describe{
#'   \item{std_plate}{Unique plate with S1-10}
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{antigen}{Antigen name}
#'   \item{eth_mfi}{Median Fluorescent Intensity for Ethiopian IgG coupled beads}
#'   \item{dilution}{Standard serial dilution factor}
#'   \item{png_mfi}{Median Fluorescent Intensity for PNG IgG coupled beads}
#' }
#' @source Mueller Lab, WEHI
"png_eth_stds.csv"

#' 96 Well Plate Example Layout
#'
#' 96 well plate map in a wide format used in the lab. Contains information of actual Sample ID names in each well.
#'
#' @format A data frame with 9 rows and 13 variables:
#' \describe{
#'   \item{Plate}{Contains rows labelled "A" to "H"}
#'   \item{1-12}{Contains columns labelled "1" to "12"}
#' }
"example_platelayout_1.xlsx"

#' Example Serological Dataset: Bioplex Plate 1
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (Bioplex).
#'
#' @format A data frame with 103 rows and 15 columns.
#' @source Randomised data
"example_BioPlex_plate1.xlsx"

#' Example Serological Dataset: Bioplex Plate 2
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (Bioplex).
#'
#' @format A data frame with 103 rows and 15 columns.
#' @source Randomised data
"example_BioPlex_plate2.xlsx"

#' Example Serological Dataset: Bioplex Plate 3
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (Bioplex).
#'
#' @format A data frame with 103 rows and 15 columns.
#' @source Randomised data
"example_BioPlex_plate3.xlsx"

#' Example Serological Dataset: MAGPIX Plate 1
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (MAGPIX).
#'
#' @format A data frame with 614 rows and 17 columns.
#' @source Internal study data
"example_MAGPIX_plate1.csv"

#' Example Serological Dataset: MAGPIX Plate 2
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (MAGPIX).
#'
#' @format A data frame with 614 rows and 17 columns.
#' @source Simulated data
"example_MAGPIX_plate2.csv"

#' Example Serological Dataset: MAGPIX Plate 3
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (MAGPIX).
#'
#' @format A data frame with 614 rows and 17 columns.
#' @source Simulated data
"example_MAGPIX_plate3.csv"

#' Random Forest Threshold Values for PvSeroTaT Model
#'
#' Random forest classification model threshold values for maximised sensitivity and specificity,
#' 85, 90, 95% specificity and 85, 90, 95% sensitivity for the PvSeroTaT model with eight antigens.
#'
#' @format A data frame with 7 rows and 2 columns.
#' @source Mueller Lab, WEHI
"threshold_values.csv"

#' Random Forest Threshold Values for PvSeroTaT Model w/o LF016
#'
#' Random forest classification model threshold values for maximised sensitivity and specificity,
#' 85, 90, 95% specificity and 85, 90, 95% sensitivity for the PvSeroTaT model with seven antigens.
#'
#' @format A data frame with 7 rows and 2 columns.
#' @source Mueller Lab, WEHI
"excluding_LF016_threshold_values.csv"

#' Eight-Antigen Random Forest Classification Model
#'
#' RData containing the PvSeroTaT eight antigen random forest classification model with
#' 10-fold cross-validation and 5 repeats, 10,000 trees and trained on the dataset described in
#' Smith et al., in prep.
#'
#' @format An Rdata file containing multiple R objects.
#' @source Mueller Lab, WEHI
"PvSeroTaTmodel.rds"

#' Seven-Antigen Random Forest Classification Model w/o LF016
#'
#' RData containing the PvSeroTaT seven antigen random forest classification model (without LF016) with
#' 10-fold cross-validation and 5 repeats, 10,000 trees and trained on the dataset described in
#' Smith et al., in prep.
#'
#' @format An Rdata file containing multiple R objects.
#' @source Mueller Lab, WEHI
"random_forest_excludingLF016.rds"
