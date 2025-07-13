#' Example Serological Dataset
#'
#' MFI values from the WEHI standard curves using both Papua New Guinean
#' and Ethiopian IgG coupled beads for standard curves.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 3860 rows and 4 variables:
#' \describe{
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{Location}{Bead coupling location for standards}
#'   \item{MFI}{Median Fluorescent Intensity}
#'   \item{Antigen}{Antigen name}
#' }
#' @source Mueller Lab, WEHI
#' @name all_stds_MFI_csv
NULL

#' Training Data Median Fluorescent Intensity (MFI)
#'
#' Metadata for the MFI of the training dataset across all eight antigens used
#' in the PvSeroTaT method.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 21,080 rows and 3 columns.
#' \describe{
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{MFI}{Median Fluorescent Intensity}
#'   \item{Antigen}{Antigen name}
#' }
#' @source Mueller Lab, WEHI
#' @name longitudinal_MFI_csv
NULL

#' Training Data Relative Antibody Units (RAU)
#'
#' Metadata for the RAU of the training dataset across all eight antigens used
#' in the PvSeroTaT method.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 21,080 rows and 3 columns.
#' \describe{
#'   \item{Sample}{Unique identifier for the sample}
#'   \item{RAU}{Relative Antibody Units}
#'   \item{Antigen}{Antigen name}
#' }
#' @source Mueller Lab, WEHI
#' @name longitudinal_RAU_csv
NULL

#' 96 Well Plate Map
#'
#' 96 well plate map in a long format used for data wrangling.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 96 rows and 2 columns.
#' \describe{
#'   \item{Location}{Location on plate}
#'   \item{Well}{Well on plate}
#' }
#' @name platemap_csv
NULL

#' Plate Metadata
#'
#' MFI values from QA/QC plate with Papua New Guinean and Ethiopian IgG coupled beads
#' for a range of the eight antigens in the PvSeroTaT method.
#'
#' This file is stored in inst/extdata
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
#' @name png_eth_stds_csv
NULL

#' 96 Well Plate Example Layout
#'
#' 96 well plate map in a wide format used in the lab. Contains information of actual Sample ID names in each well.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 9 rows and 13 variables.
#' \describe{
#'   \item{Plate}{Contains rows labelled "A" to "H"}
#'   \item{1-12}{Contains columns labelled "1" to "12"}
#' }
#' @name example_platelayout_1
NULL

#' Example Serological Dataset: Bioplex Plate 1
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (Bioplex).
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 103 rows and 15 columns.
#' @source Randomised data
#' @name example_BioPlex_plate1
NULL

#' Example Serological Dataset: Bioplex Plate 2
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (Bioplex).
#'
#' @format A data frame with 103 rows and 15 columns.
#' @source Randomised data
#' @name example_BioPlex_plate2
NULL

#' Example Serological Dataset: Bioplex Plate 3
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (Bioplex).
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 103 rows and 15 columns.
#' @source Randomised data
#' @name example_BioPlex_plate2
NULL
"example_BioPlex_plate3.xlsx"

#' Example Serological Dataset: MAGPIX Plate 1

#' A dataset containing raw MFI values and metadata from a sample plate run (MAGPIX).
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 614 rows and 17 columns.
#' @source Randomised data
#' @name example_MAGPIX_plate1
NULL

#' Example Serological Dataset: MAGPIX Plate 2
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (MAGPIX).
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 614 rows and 17 columns.
#' @source Randomised data
#' @name example_MAGPIX_plate2
NULL

#' Example Serological Dataset: MAGPIX Plate 3
#'
#' A dataset containing raw MFI values and metadata from a sample plate run (MAGPIX).
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 614 rows and 17 columns.
#' @source Randomised data
#' @name example_MAGPIX_plate3
NULL

#' Random Forest Threshold Values for PvSeroTaT Model
#'
#' Random forest classification model threshold values for maximised sensitivity and specificity,
#' 85, 90, 95% specificity and 85, 90, 95% sensitivity for the PvSeroTaT model with eight antigens.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 7 rows and 2 columns.
#' @source Mueller Lab, WEHI
#' @name threshold_values_csv
NULL

#' Random Forest Threshold Values for PvSeroTaT Model w/o LF016
#'
#' Random forest classification model threshold values for maximised sensitivity and specificity,
#' 85, 90, 95% specificity and 85, 90, 95% sensitivity for the PvSeroTaT model with seven antigens.
#'
#' This file is stored in inst/extdata
#'
#' @format A data frame with 7 rows and 2 columns.
#' @source Mueller Lab, WEHI
#' @name excluding_LF016_threshold_values_csv
NULL

#' Eight-Antigen Random Forest Classification Model
#'
#' RData containing the PvSeroTaT eight antigen random forest classification model with
#' 10-fold cross-validation and 5 repeats, 10,000 trees and trained on the dataset described in
#' Smith et al., in prep.
#'
#' This file is stored in inst/extdata
#'
#' @format An Rdata file containing multiple R objects.
#' @source Mueller Lab, WEHI
#' @name PvSeroTaTmodel_rds
NULL

#' Seven-Antigen Random Forest Classification Model w/o LF016
#'
#' RData containing the PvSeroTaT seven antigen random forest classification model (without LF016) with
#' 10-fold cross-validation and 5 repeats, 10,000 trees and trained on the dataset described in
#' Smith et al., in prep.
#'
#' This file is stored in inst/extdata
#'
#' @format An Rdata file containing multiple R objects.
#' @source Mueller Lab, WEHI
#' @name random_forest_excludingLF016_rds
NULL
