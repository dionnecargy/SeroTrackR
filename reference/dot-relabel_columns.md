# Relabel column names to Standardised Naming Convention

This is a helper function to be used inside \`readSeroData()\` to
relabel columns for each plate.

## Usage

``` r
.relabel_columns(df)
```

## Arguments

- df:

  Data frame from \`readSeroData()\` processing.

## Value

A data fame with columns renamed

## Author

Dionne Argyropoulos

## Examples

``` r
# \donttest{
your_raw_data <- system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR")

if (
 requireNamespace("dplyr", quietly = TRUE) &&
 requireNamespace("janitor", quietly = TRUE)
) {

  # Read in raw luminex file
  df <- .read_luminex_file(your_raw_data)

  # Get the start and end rows of the data section: start = "Median", end = "Net MFI"
  row1    <- which(df$xPONENT == "Median")
  row2    <- which(df$xPONENT == "Net MFI")

  # Apply data processing pipeline, including .relabel_columns()
  df |>
    dplyr::slice((row1 + 1):(row2 - 1)) |>
    janitor::row_to_names(row_number = 1) |>
    dplyr::select(dplyr::where(~ !all(is.na(.x)))) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) |>
    dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN", 0, .))) |>
    .relabel_columns()
}
#> Warning: Row 1 does not provide unique names. Consider running clean_names() after row_to_names().
#> Error in dplyr::case_when(stringr::str_detect(colnames(df), regex("EBP",     ignore_case = TRUE)) ~ "EBP", stringr::str_detect(colnames(df),     regex("(LF005|Pv.fam.a|fam.a|Pv-fam-a)", ignore_case = TRUE)) ~     "LF005", stringr::str_detect(colnames(df), regex("(LF010|MSP5)",     ignore_case = TRUE)) ~ "LF010", stringr::str_detect(colnames(df),     regex("(LF016|PvMSP1-19|PvMSP1.19)", ignore_case = TRUE)) ~     "LF016", stringr::str_detect(colnames(df), regex("(MSP8|L34)",     ignore_case = TRUE)) ~ "MSP8", stringr::str_detect(colnames(df),     regex("(P87|RBP2b-P87|RBP2b|PvRBP)", ignore_case = TRUE)) ~     "RBP2b.P87", stringr::str_detect(colnames(df), regex("(PTEX|PTEX150|L18)",     ignore_case = TRUE)) ~ "PTEX150", stringr::str_detect(colnames(df),     regex("PkTRAMPCSS|PkTRAMP-CSS|PkPC", ignore_case = TRUE)) ~     "PkTRAMP-CSS", stringr::str_detect(colnames(df), regex("CSS",     ignore_case = TRUE)) ~ "PvCSS", stringr::str_detect(colnames(df),     regex("(PfMSP1-19|PfMSP1|PfMSP1.19)", ignore_case = TRUE)) ~     "PfMSP1-19", stringr::str_detect(colnames(df), regex("PfAMA1",     ignore_case = TRUE)) ~ "PfAMA1", stringr::str_detect(colnames(df),     regex("Pfetramp5Ag1|Pfetramp", ignore_case = TRUE)) ~ "Pfetramp5Ag1",     stringr::str_detect(colnames(df), regex("HSP40Ag1", ignore_case = TRUE)) ~         "PfHSP40Ag1", stringr::str_detect(colnames(df), regex("PfGexp18",         ignore_case = TRUE)) ~ "PfGexp18", stringr::str_detect(colnames(df),         regex("PkSSP2", ignore_case = TRUE)) ~ "PkSSP2", stringr::str_detect(colnames(df),         regex("PkMSP10", ignore_case = TRUE)) ~ "PkMSP10", stringr::str_detect(colnames(df),         regex("PkRIPR", ignore_case = TRUE)) ~ "PkRIPR", stringr::str_detect(colnames(df),         regex("Sera3ag1", ignore_case = TRUE)) ~ "PkSERA3ag1",     stringr::str_detect(colnames(df), regex("Pk8", ignore_case = TRUE)) ~         "Pk8", stringr::str_detect(colnames(df), regex("SERA3Ag2",         ignore_case = TRUE)) ~ "PkSERA3Ag2", TRUE ~ colnames(df)): Failed to evaluate the left-hand side of formula 1.
#> Caused by error in `dplyr::select()`:
#> ! Names can't be empty.
#> ✖ Empty names found at locations 12, 13, 14, 15, 16, etc.
# }
```
