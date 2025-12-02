## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
setup <- function() {
  needed <- c("knitr", "rmarkdown", "tidyverse", "kableExtra")
  
  lapply(needed, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE)
    }
  })
}

setup()
library(SeroTrackR)

## ----setup 1, eval=FALSE------------------------------------------------------
# library(SeroTrackR)
# library(tidyverse)
# 
# your_raw_data <- system.file("extdata", "example_BioPlex_PvLDH_plate1.csv", package = "SeroTrackR")
# your_plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")

## ----exec=FALSE, eval=FALSE---------------------------------------------------
# your_raw_data <- "PATH/TO/YOUR/FILE/plate1.csv"
# your_plate_layout <- "PATH/TO/YOUR/FILE/plate_layout.xlsx"

## ----example code for LDH, eval=FALSE-----------------------------------------
# runLDHpipeline(
#   raw_data = your_raw_data,
#   plate_layout = your_plate_layout,
#   platform = "bioplex",                               # Defaults to "bioplex" but "magpix" also works
#   file_path = NULL                                    # Defaults to current folder/working directory
# )

