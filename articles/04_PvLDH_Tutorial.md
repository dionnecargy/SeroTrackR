# PvLDH Tutorial

## Data Analysis: LDH

For the LDH data analysis, follow the steps below:

### Using Tutorial Dataset: Load the Data

We will be using the build-in files in the R package for this tutorial,
as shown below.

``` r
library(SeroTrackR)
library(tidyverse)

your_raw_data <- system.file("extdata", "example_BioPlex_PvLDH_plate1.csv", package = "SeroTrackR")
your_plate_layout <- system.file("extdata", "example_platelayout_1.xlsx", package = "SeroTrackR")
```

To run your OWN data, follow the code below and uncomment (i.e., remove
the hashtags):

``` r
your_raw_data <- "PATH/TO/YOUR/FILE/plate1.csv"
your_plate_layout <- "PATH/TO/YOUR/FILE/plate_layout.xlsx"
```

### Run Pipeline

``` r
runLDHpipeline(
  raw_data = your_raw_data,
  plate_layout = your_plate_layout,
  platform = "bioplex",                               # Defaults to "bioplex" but "magpix" also works
  file_path = NULL                                    # Defaults to current folder/working directory
)
```
