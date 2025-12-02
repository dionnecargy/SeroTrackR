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

## ----setup pk analysis, eval = FALSE------------------------------------------
# library(SeroTrackR)
# library(tidyverse)

## ----eval = FALSE-------------------------------------------------------------
# your_raw_data_5std <- c(
#   system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
#   system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
# )
# your_plate_layout_5std <- system.file("extdata", "example_platelayout_pk_5std.xlsx", package = "SeroTrackR")

## ----eval = FALSE-------------------------------------------------------------
# your_raw_data_10std <- c(
#   system.file("extdata", "example_MAGPIX_pk_10std_plate1.csv", package = "SeroTrackR"),
#   system.file("extdata", "example_MAGPIX_pk_10std_plate2.csv", package = "SeroTrackR")
# )
# your_plate_layout_10std <- system.file("extdata", "example_platelayout_pk_10std.xlsx", package = "SeroTrackR")

## ----eval = FALSE-------------------------------------------------------------
# results <- runPlasmoPipeline(
#   raw_data = your_raw_data_5std,
#   platform = "magpix",
#   plate_layout = your_plate_layout_5std,
#   panel = "panel1",
#   std_point = 5,
#   experiment_name = "5-point standard curve"
# )

## ----eval = FALSE-------------------------------------------------------------
# results <- runPlasmoPipeline(
#   raw_data = your_raw_data_10std,
#   platform = "magpix",
#   plate_layout = your_plate_layout_10std,
#   panel = "panel1",
#   std_point = 10,
#   experiment_name = "10-point standard curve"
# )

