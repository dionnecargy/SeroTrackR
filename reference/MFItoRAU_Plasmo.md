# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion for Pk/Pf/Pv Master Function

This function leverages \`MFItoRAU_Pk()\` and \`MFItoRAU()\` to create a
final MFI to RAU output for Pk/Pf/Pv analyses.

## Usage

``` r
MFItoRAU_Plasmo(sero_data, plate_list, panel = "panel1", std_point, qc_results)
```

## Arguments

- sero_data:

  Output of \`readserodata_output()\`

- plate_list:

  Output of \`readPlateLayout()\`

- panel:

  Panel of Pk/Pf/Pv antigens. Default = "panel1" or user provided csv of
  Antigens and Species.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = 10. Value is an integer.

- qc_results:

  Output from \`runQC()\`.

## Value

A list of three data frames: 1. Data frame with MFI data, converted RAU
data, matched SampleID's, all intermediate dilution conversion factors
2. Data frame with only SampleID's, MFI and RAU data 3. Data frame \#2
in long-format

## Author

Dionne Argyropoulos, Caitlin Bourke

## Examples

``` r
# \donttest{
# Example demonstrating multi-plate 5-standard processing workflow.
# These files are included in the SeroTrackR package under inst/extdata.

your_raw_data_5std <- c(
  system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
)
your_plate_layout_5std <- system.file(
  "extdata", "example_platelayout_pk_5std.xlsx",
  package = "SeroTrackR"
)

# Read in raw MAGPIX data
sero_data <- readSeroData(
  raw_data = your_raw_data_5std,
  platform = "magpix"
)
#> PASS: File example_magpix_pk_5std_plate1.csv successfully validated.
#> PASS: File example_magpix_pk_5std_plate2.csv successfully validated.

# Read matching plate layout
plate_list <- readPlateLayout(
  plate_layout = your_plate_layout_5std,
  sero_data = sero_data
)
#> Plate layouts correctly identified!
# Quality control
qc_results  <- runQC(sero_data, plate_list)

# Run MFI to RAU conversion
mfi_outputs               <- MFItoRAU_Plasmo(
  sero_data = sero_data,
  plate_list = plate_list,
  panel = "panel1",
  std_point = 5,
  qc_results = qc_results
)

# View All Outputs
mfi_outputs
#> $All_Results
#> # A tibble: 168 × 145
#>    SampleID Location  Location.2 Sample    Plate  Pk8_MFI Pk8_log_mfi Pk8_max_s1
#>    <chr>    <chr>     <chr>      <chr>     <chr>    <dbl>       <dbl>      <dbl>
#>  1 ABC-0001 13(1,B1)  B1         Unknown13 plate1     601        6.40       8.63
#>  2 ABC-0002 14(1,B2)  B2         Unknown14 plate1     344        5.84       8.63
#>  3 ABC-0003 15(1,B3)  B3         Unknown15 plate1     716        6.57       8.63
#>  4 ABC-0004 16(1,B4)  B4         Unknown16 plate1     357        5.88       8.63
#>  5 ABC-0005 17(1,B5)  B5         Unknown17 plate1    1320        7.19       8.63
#>  6 ABC-0006 18(1,B6)  B6         Unknown18 plate1     899        6.80       8.63
#>  7 ABC-0007 19(1,B7)  B7         Unknown19 plate1     975        6.88       8.63
#>  8 ABC-0008 20(1,B8)  B8         Unknown20 plate1     637        6.46       8.63
#>  9 ABC-0009 21(1,B9)  B9         Unknown21 plate1    1635        7.40       8.63
#> 10 ABC-0010 22(1,B10) B10        Unknown22 plate1     256        5.55       8.63
#> # ℹ 158 more rows
#> # ℹ 137 more variables: Pk8_max_dil <dbl>, Pk8_slope <dbl>, Pk8_low_asym <dbl>,
#> #   Pk8_upp_asym <dbl>, Pk8_ed50 <dbl>, Pk8_asym_par <dbl>, Pk8_Dilution <dbl>,
#> #   PkMSP10_MFI <dbl>, PkMSP10_log_mfi <dbl>, PkMSP10_max_s1 <dbl>,
#> #   PkMSP10_max_dil <dbl>, PkMSP10_slope <dbl>, PkMSP10_low_asym <dbl>,
#> #   PkMSP10_upp_asym <dbl>, PkMSP10_ed50 <dbl>, PkMSP10_asym_par <dbl>,
#> #   PkMSP10_Dilution <dbl>, PkSERA3Ag2_MFI <dbl>, PkSERA3Ag2_log_mfi <dbl>, …
#> 
#> $MFI_RAU
#> # A tibble: 168 × 44
#>    SampleID Plate  Pk8_MFI PkMSP10_MFI PkSERA3Ag2_MFI PkSSP2_MFI `Pv-fam-a_MFI`
#>    <chr>    <chr>    <dbl>       <dbl>          <dbl>      <dbl>          <dbl>
#>  1 ABC-0001 plate1     601        346            858        320            389 
#>  2 ABC-0002 plate1     344        228.           384        112.           217 
#>  3 ABC-0003 plate1     716        584           4397        271            634 
#>  4 ABC-0004 plate1     357        235            438.       144.           238 
#>  5 ABC-0005 plate1    1320        746           1620        778            746 
#>  6 ABC-0006 plate1     899        733           2702.      1015            678.
#>  7 ABC-0007 plate1     975        652           3860        531            716.
#>  8 ABC-0008 plate1     637        228.          1018        459            226.
#>  9 ABC-0009 plate1    1635       1299           1068        795            951 
#> 10 ABC-0010 plate1     256        100            534         38             72 
#> # ℹ 158 more rows
#> # ℹ 37 more variables: PvMSP5_MFI <dbl>, `PvMSP1-19_MFI` <dbl>,
#> #   PvEBP_MFI <dbl>, PvRBP2b_MFI <dbl>, PvCSS_MFI <dbl>, PvPTEX150_MFI <dbl>,
#> #   PvMSP8_MFI <dbl>, `PfMSP1-19_MFI` <dbl>, PfAMA1_MFI <dbl>,
#> #   Pfetramp5Ag1_MFI <dbl>, PfHSP40Ag1_MFI <dbl>, PfGexp18_MFI <dbl>,
#> #   Pk8_Dilution <dbl>, PkMSP10_Dilution <dbl>, PkSERA3Ag2_Dilution <dbl>,
#> #   PkSSP2_Dilution <dbl>, `Pv-fam-a_loglog_Dilution` <dbl>, …
#> 
#> $MFI_RAU_long
#> # A tibble: 4,200 × 7
#>    SampleID Plate  Antigens   Species   MFI       RAU RAU_Method
#>    <chr>    <chr>  <chr>      <chr>   <dbl>     <dbl> <chr>     
#>  1 ABC-0001 plate1 Pk8        Pk        601 0.00171   loglog    
#>  2 ABC-0001 plate1 PkMSP10    Pk        346 0.0000195 loglog    
#>  3 ABC-0001 plate1 PkSERA3Ag2 Pk        858 0.000513  loglog    
#>  4 ABC-0001 plate1 PkSSP2     Pk        320 0.00196   loglog    
#>  5 ABC-0001 plate1 Pv-fam-a   Pv        389 0.0000834 loglog    
#>  6 ABC-0001 plate1 Pv-fam-a   Pv        389 0.0000923 Adj_loglog
#>  7 ABC-0001 plate1 PvMSP5     Pv        392 0.0000856 loglog    
#>  8 ABC-0001 plate1 PvMSP5     Pv        392 0.0000800 Adj_loglog
#>  9 ABC-0001 plate1 PvMSP1-19  Pv        502 0.0000195 loglog    
#> 10 ABC-0001 plate1 PvMSP1-19  Pv        502 0.0000762 Adj_loglog
#> # ℹ 4,190 more rows
#> 
# }
```
