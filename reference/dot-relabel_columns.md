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
#> # A tibble: 96 × 11
#>    Location  Sample EBP     LF005 LF010   LF016   MSP8   RBP2b.P87 PTEX150 PvCSS
#>    <chr>     <chr>  <chr>   <chr> <chr>   <chr>   <chr>  <chr>     <chr>   <chr>
#>  1 1(1,A1)   Blank1 20      200   20      10      20     10        30      20   
#>  2 2(1,A2)   Blank2 15      291   15      15      15     10        20      15   
#>  3 3(1,A3)   S1     15710.5 11990 22583   21244   24306  7907.5    13207   12427
#>  4 4(1,A4)   S2     13545   8285  16947.5 16146   17172  7186      8550    8034 
#>  5 5(1,A5)   S3     9767    4950  10865   10621.5 11358  4475.5    5329    4990 
#>  6 6(1,A6)   S4     5648.5  2519  6060    5968.5  6237   3508      2671    2446…
#>  7 7(1,A7)   S5     4104.5  1431  3711.5  3738    3883.5 2082      1548    1299 
#>  8 8(1,A8)   S6     2105    676.5 1889    1667    2136   1102      698.5   581  
#>  9 9(1,A9)   S7     1107    328   1106    890     1204   657       333     264  
#> 10 10(1,A10) S8     452.5   141   465     405.5   522    277       156     111  
#> # ℹ 86 more rows
#> # ℹ 1 more variable: `Total Events` <chr>
# }
```
