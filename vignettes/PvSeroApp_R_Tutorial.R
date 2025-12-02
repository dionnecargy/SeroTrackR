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

## ----setup 1, eval = FALSE----------------------------------------------------
# library(SeroTrackR)
# library(tidyverse)
# 
# your_raw_data <- c(
#   system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
#   system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
#   system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
# )
# your_plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")

## ----exec=FALSE, eval=FALSE---------------------------------------------------
# your_raw_data <- c(
#   "PATH/TO/YOUR/FILE/plate1.csv",
#   "PATH/TO/YOUR/FILE/plate2.csv",
#   "PATH/TO/YOUR/FILE/plate3.csv"
# )
# your_plate_layout <- "PATH/TO/YOUR/FILE/plate_layout.xlsx"

## ----runPvSeroPipeline with classification, eval = FALSE----------------------
# final_analysis <- runPvSeroPipeline(
#   raw_data = your_raw_data,
#   plate_layout = your_plate_layout,
#   platform = "magpix",
#   location = "ETH",
#   experiment_name = "experiment1",
#   classify = "Yes",
#   algorithm_type = "antibody_model",
#   sens_spec = "maximised"
# )
# 
# final_analysis

