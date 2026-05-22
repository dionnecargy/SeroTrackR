# Run Quality Control Pipeline

A master function containing each quality control processing step.

## Usage

``` r
runQC(sero_data, plate_list)
```

## Arguments

- sero_data:

  Output from \`readSeroData()\`.

- plate_list:

  Output from \`readPlateLayout()\`.

## Value

processCounts_output, getCounts_output, sampleid_output,
getAntigenCounts_output, getCountsQC_output

## Author

Dionne Argyropoulos

## Examples

``` r

# \donttest{
# Example data supplied with the package
your_raw_data <- c(
  system.file("extdata", "example_MAGPIX_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate2.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_plate3.csv", package = "SeroTrackR")
)

your_plate_layout <- system.file(
  "extdata", "example_platelayout_1.xlsx", package = "SeroTrackR"
)

# Read serology data and plate layout
sero_data  <- readSeroData(your_raw_data,"magpix")
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
plate_list <- readPlateLayout(your_plate_layout, sero_data)
#> Plate layouts correctly identified!

# Run full pipeline including classification
runQC(
  sero_data = sero_data,
  plate_list = plate_list
)
#> $processCounts_output
#> # A tibble: 2,304 × 6
#>    Location Sample Plate  Antigen   Count Warning
#>    <chr>    <chr>  <chr>  <chr>     <dbl>   <dbl>
#>  1 A1       Blank1 plate1 EBP         150       0
#>  2 A1       Blank1 plate1 LF005        88       0
#>  3 A1       Blank1 plate1 LF010       184       0
#>  4 A1       Blank1 plate1 LF016       170       0
#>  5 A1       Blank1 plate1 MSP8        105       0
#>  6 A1       Blank1 plate1 RBP2b.P87   127       0
#>  7 A1       Blank1 plate1 PTEX150      85       0
#>  8 A1       Blank1 plate1 PvCSS       119       0
#>  9 A2       Blank2 plate1 EBP         142       0
#> 10 A2       Blank2 plate1 LF005        97       0
#> # ℹ 2,294 more rows
#> 
#> $getCounts_output
#> # A tibble: 288 × 7
#> # Groups:   Location [96]
#>    Location Plate    Sum Repeat           Row     Col QC_total
#>    <chr>    <chr>  <dbl> <chr>            <chr> <dbl> <chr>   
#>  1 A1       plate1     0 sufficient beads A         1 pass    
#>  2 A1       plate2     1 repeat           A         1 fail    
#>  3 A1       plate3     0 sufficient beads A         1 pass    
#>  4 A10      plate1     0 sufficient beads A        10 pass    
#>  5 A10      plate2     0 sufficient beads A        10 pass    
#>  6 A10      plate3     0 sufficient beads A        10 pass    
#>  7 A11      plate1     0 sufficient beads A        11 pass    
#>  8 A11      plate2     0 sufficient beads A        11 pass    
#>  9 A11      plate3     0 sufficient beads A        11 pass    
#> 10 A12      plate1     0 sufficient beads A        12 pass    
#> # ℹ 278 more rows
#> 
#> $sampleid_output
#> # A tibble: 2,304 × 7
#>    SampleID Location Plate  Sample Antigen   Count Warning
#>    <chr>    <chr>    <chr>  <chr>  <chr>     <dbl>   <dbl>
#>  1 Blank1   A1       plate1 Blank1 EBP         150       0
#>  2 Blank1   A1       plate1 Blank1 LF005        88       0
#>  3 Blank1   A1       plate1 Blank1 LF010       184       0
#>  4 Blank1   A1       plate1 Blank1 LF016       170       0
#>  5 Blank1   A1       plate1 Blank1 MSP8        105       0
#>  6 Blank1   A1       plate1 Blank1 RBP2b.P87   127       0
#>  7 Blank1   A1       plate1 Blank1 PTEX150      85       0
#>  8 Blank1   A1       plate1 Blank1 PvCSS       119       0
#>  9 Blank2   A2       plate1 Blank2 EBP         142       0
#> 10 Blank2   A2       plate1 Blank2 LF005        97       0
#> # ℹ 2,294 more rows
#> 
#> $getAntigenCounts_output
#> # A tibble: 2,304 × 7
#> # Groups:   Location, Antigen, Count [929]
#>    Location Antigen Count Plate  Repeat           QC_antigen SampleID
#>    <chr>    <chr>   <dbl> <chr>  <fct>            <chr>      <chr>   
#>  1 A1       EBP       150 plate1 sufficient beads pass       Blank1  
#>  2 A1       EBP       142 plate2 sufficient beads pass       Blank1  
#>  3 A1       EBP       150 plate3 sufficient beads pass       Blank1  
#>  4 A1       LF005      88 plate1 sufficient beads pass       Blank1  
#>  5 A1       LF005      85 plate2 sufficient beads pass       Blank1  
#>  6 A1       LF005      95 plate3 sufficient beads pass       Blank1  
#>  7 A1       LF010     184 plate1 sufficient beads pass       Blank1  
#>  8 A1       LF010     218 plate2 sufficient beads pass       Blank1  
#>  9 A1       LF010     184 plate3 sufficient beads pass       Blank1  
#> 10 A1       LF016     170 plate1 sufficient beads pass       Blank1  
#> # ℹ 2,294 more rows
#> 
#> $getCountsQC_output
#> # A tibble: 288 × 20
#>    Location SampleID Plate  EBP_Count EBP_QC LF005_Count LF005_QC LF010_Count
#>    <chr>    <chr>    <chr>      <dbl> <chr>        <dbl> <chr>          <dbl>
#>  1 A1       Blank1   plate1       150 pass            88 pass             184
#>  2 A1       Blank1   plate2       142 pass            85 pass             218
#>  3 A1       Blank1   plate3       150 pass            95 pass             184
#>  4 A10      S8       plate1        50 pass            62 pass              52
#>  5 A10      S8       plate2        50 pass            51 pass              78
#>  6 A10      S8       plate3        52 pass            70 pass              51
#>  7 A11      S9       plate1        67 pass            60 pass              78
#>  8 A11      S9       plate2        60 pass            59 pass              63
#>  9 A11      S9       plate3        50 pass            68 pass              80
#> 10 A12      S10      plate1        65 pass            50 pass              80
#> # ℹ 278 more rows
#> # ℹ 12 more variables: LF010_QC <chr>, LF016_Count <dbl>, LF016_QC <chr>,
#> #   MSP8_Count <dbl>, MSP8_QC <chr>, PTEX150_Count <dbl>, PTEX150_QC <chr>,
#> #   PvCSS_Count <dbl>, PvCSS_QC <chr>, RBP2b.P87_Count <dbl>,
#> #   RBP2b.P87_QC <chr>, QC_total <chr>
#> 
# }
```
