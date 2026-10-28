# Pk/Pv/Pf Serology Tutorial

For all of these analyses you can run as many plates as you wish.

### Visualisation of the Pk/Pv/Pf Pipeline

![](../reference/figures/SeroTrackR_Pk_Pf_Pv.png)

``` r

library(SeroTrackR)
library(tidyverse)
```

### 5-Point Standard Curve

#### Step 1: Load your data!

Firstly, we will be using our example data that’s in-built in the
package. Here replace the
[`system.file()`](https://rdrr.io/r/base/system.file.html) argument with
the file path for your package.

``` r

your_raw_data_5std <- c(
  system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
)
your_plate_layout_5std <- system.file("extdata", "example_platelayout_pk_5std.xlsx", package = "SeroTrackR")
```

#### Step 2: Read your data and process MFI to RAU

This function to (a) process raw Serological data and (b) convert MFI to
RAU. The
[`runPlasmoPipeline()`](https://dionnecargy.github.io/SeroTrackR/reference/runPlasmoPipeline.md)
function will output three data frames:

1.  All_Results: All columns of every MFI to RAU conversion
2.  MFI_RAU: Just the SampleID, Plate, MFI and RAU values per antigen
3.  MFI_RAU_long: SampleID, Plate, MFI, RAU, Antigen, Species
    (long-format df)

``` r

results_5stdcurve <- runPlasmoPipeline(
  raw_data = your_raw_data_5std,
  platform = "magpix",
  plate_layout = your_plate_layout_5std,
  panel = "panel1",
  std_point = 5, 
  experiment_name = "5-point standard curve"
)
#> PASS: File example_magpix_pk_5std_plate1.csv successfully validated.
#> PASS: File example_magpix_pk_5std_plate2.csv successfully validated.
#> Plate layouts correctly identified!
#> QC Processes completed.
#> QC Plotting completed.
#> MFI to RAU conversion completed.
#> Pv classification completed.
```

##### Standard Curve Plot

``` r

results_5stdcurve$std_curve
```

![](Pk_Pv_Pf_Serology_Tutorial_files/figure-html/unnamed-chunk-4-1.png)

##### Bead Counts QC Plot

``` r

results_5stdcurve$bead_counts
```

![](Pk_Pv_Pf_Serology_Tutorial_files/figure-html/unnamed-chunk-5-1.png)

##### Blanks QC Ploat

``` r

results_5stdcurve$blanks
```

![](Pk_Pv_Pf_Serology_Tutorial_files/figure-html/unnamed-chunk-6-1.png)

##### MFI to RAU Tables

All results:

``` r

results_5stdcurve$mfi_outputs$All_Results %>%
  head() %>% 
  kable()
```

| SampleID | Location | Location.2 | Sample | Plate | Pk8_MFI | Pk8_log_mfi | Pk8_max_s1 | Pk8_max_dil | Pk8_slope | Pk8_low_asym | Pk8_upp_asym | Pk8_ed50 | Pk8_asym_par | Pk8_Dilution | PkMSP10_MFI | PkMSP10_log_mfi | PkMSP10_max_s1 | PkMSP10_max_dil | PkMSP10_slope | PkMSP10_low_asym | PkMSP10_upp_asym | PkMSP10_ed50 | PkMSP10_asym_par | PkMSP10_Dilution | PkSERA3Ag2_MFI | PkSERA3Ag2_log_mfi | PkSERA3Ag2_max_s1 | PkSERA3Ag2_max_dil | PkSERA3Ag2_slope | PkSERA3Ag2_low_asym | PkSERA3Ag2_upp_asym | PkSERA3Ag2_ed50 | PkSERA3Ag2_asym_par | PkSERA3Ag2_Dilution | PkSSP2_MFI | PkSSP2_log_mfi | PkSSP2_max_s1 | PkSSP2_max_dil | PkSSP2_slope | PkSSP2_low_asym | PkSSP2_upp_asym | PkSSP2_ed50 | PkSSP2_asym_par | PkSSP2_Dilution | QC_total | Pv-fam-a_MFI | Pv-fam-a_loglog_Dilution | Pv-fam-a_DilutionReciprocal | Pv-fam-a_MinStd | Pv-fam-a_MaxDilution | Pv-fam-a_MaxStd | Pv-fam-a_MinDilution | PvMSP5_MFI | PvMSP5_loglog_Dilution | PvMSP5_DilutionReciprocal | PvMSP5_MinStd | PvMSP5_MaxDilution | PvMSP5_MaxStd | PvMSP5_MinDilution | PvMSP1-19_MFI | PvMSP1-19_loglog_Dilution | PvMSP1-19_DilutionReciprocal | PvMSP1-19_MinStd | PvMSP1-19_MaxDilution | PvMSP1-19_MaxStd | PvMSP1-19_MinDilution | PvEBP_MFI | PvEBP_loglog_Dilution | PvEBP_DilutionReciprocal | PvEBP_MinStd | PvEBP_MaxDilution | PvEBP_MaxStd | PvEBP_MinDilution | PvRBP2b_MFI | PvRBP2b_loglog_Dilution | PvRBP2b_DilutionReciprocal | PvRBP2b_MinStd | PvRBP2b_MaxDilution | PvRBP2b_MaxStd | PvRBP2b_MinDilution | PvCSS_MFI | PvCSS_loglog_Dilution | PvCSS_DilutionReciprocal | PvCSS_MinStd | PvCSS_MaxDilution | PvCSS_MaxStd | PvCSS_MinDilution | PvPTEX150_MFI | PvPTEX150_loglog_Dilution | PvPTEX150_DilutionReciprocal | PvPTEX150_MinStd | PvPTEX150_MaxDilution | PvPTEX150_MaxStd | PvPTEX150_MinDilution | PvMSP8_MFI | PvMSP8_loglog_Dilution | PvMSP8_DilutionReciprocal | PvMSP8_MinStd | PvMSP8_MaxDilution | PvMSP8_MaxStd | PvMSP8_MinDilution | PfMSP1-19_MFI | PfMSP1-19_loglog_Dilution | PfMSP1-19_DilutionReciprocal | PfMSP1-19_MinStd | PfMSP1-19_MaxDilution | PfMSP1-19_MaxStd | PfMSP1-19_MinDilution | PfAMA1_MFI | PfAMA1_loglog_Dilution | PfAMA1_DilutionReciprocal | PfAMA1_MinStd | PfAMA1_MaxDilution | PfAMA1_MaxStd | PfAMA1_MinDilution | Pfetramp5Ag1_MFI | Pfetramp5Ag1_loglog_Dilution | Pfetramp5Ag1_DilutionReciprocal | Pfetramp5Ag1_MinStd | Pfetramp5Ag1_MaxDilution | Pfetramp5Ag1_MaxStd | Pfetramp5Ag1_MinDilution | PfHSP40Ag1_MFI | PfHSP40Ag1_loglog_Dilution | PfHSP40Ag1_DilutionReciprocal | PfHSP40Ag1_MinStd | PfHSP40Ag1_MaxDilution | PfHSP40Ag1_MaxStd | PfHSP40Ag1_MinDilution | PfGexp18_MFI | PfGexp18_loglog_Dilution | PfGexp18_DilutionReciprocal | PfGexp18_MinStd | PfGexp18_MaxDilution | PfGexp18_MaxStd | PfGexp18_MinDilution | PvEBP_Adjloglog_Dilution | Pv-fam-a_Adjloglog_Dilution | PvMSP5_Adjloglog_Dilution | PvMSP1-19_Adjloglog_Dilution | PvMSP8_Adjloglog_Dilution | PvPTEX150_Adjloglog_Dilution | PvCSS_Adjloglog_Dilution | PvRBP2b_Adjloglog_Dilution |
|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| ABC-0001 | 13(1,B1) | B1 | Unknown13 | plate1 | 601 | 6.398595 | 8.631057 | 0.02 | -0.255195 | 1.721846 | 14.73796 | 0.0020262 | 1.430762 | 0.0017050 | 346.0 | 5.846439 | 9.847314 | 0.02 | -0.5988432 | 5.710997 | 10.13687 | 5.39e-05 | 2.526623 | 1.95e-05 | 858.0 | 6.754604 | 9.278139 | 0.02 | -0.1812443 | -7.423291 | 13.38976 | 5.53e-05 | 0.750427 | 0.0005129 | 320.0 | 5.768321 | 7.38399 | 0.02 | -0.3931855 | 2.262344 | 9.049039 | 7.59e-05 | 2.686889 | 0.0019577 | pass | 389.0 | 0.0000834 | 11989.153 | 159 | 3.2e-05 | 17367 | 0.02 | 392 | 0.0000852 | 11734.987 | 174.5 | 3.2e-05 | 12754 | 0.02 | 502 | 1.95e-05 | 51200 | 3558.5 | 3.2e-05 | 26809.5 | 0.02 | 387.0 | 0.0000966 | 10352.513 | 149 | 3.2e-05 | 13445 | 0.02 | 831.5 | 0.0000400 | 25018.250 | 713 | 3.2e-05 | 23660.5 | 0.02 | 438.0 | 0.0007845 | 1274.6183 | 37 | 3.2e-05 | 6152 | 0.02 | 556.0 | 0.0007194 | 1390.0619 | 37 | 3.2e-05 | 7636 | 0.02 | 303.0 | 0.0000233 | 43006.509 | 387 | 3.2e-05 | 23226 | 0.02 | 363 | 0.0004380 | 2283.2281 | 69 | 3.2e-05 | 2751.5 | 0.02 | 359.5 | 0.0011172 | 895.0603 | 37 | 3.2e-05 | 3007 | 0.02 | 467 | 0.0050186 | 199.25926 | 17 | 3.2e-05 | 1459 | 0.02 | 384.0 | 0.0061629 | 162.26245 | 12 | 3.2e-05 | 988 | 0.02 | 1113 | 0.0052569 | 190.22706 | 20 | 3.2e-05 | 2892 | 0.02 | 0.0000258 | 0.0000923 | 0.0000800 | 0.0000762 | 0.0000195 | 0.0001358 | 0.0002014 | 1.95e-05 |
| ABC-0002 | 14(1,B2) | B2 | Unknown14 | plate1 | 344 | 5.840642 | 8.631057 | 0.02 | -0.255195 | 1.721846 | 14.73796 | 0.0020262 | 1.430762 | 0.0008863 | 228.5 | 5.431536 | 9.847314 | 0.02 | -0.5988432 | 5.710997 | 10.13687 | 5.39e-05 | 2.526623 | 1.95e-05 | 384.0 | 5.950643 | 9.278139 | 0.02 | -0.1812443 | -7.423291 | 13.38976 | 5.53e-05 | 0.750427 | 0.0001858 | 112.5 | 4.722953 | 7.38399 | 0.02 | -0.3931855 | 2.262344 | 9.049039 | 7.59e-05 | 2.686889 | 0.0005509 | pass | 217.0 | 0.0000441 | 22679.984 | 159 | 3.2e-05 | 17367 | 0.02 | 242 | 0.0000476 | 21021.638 | 174.5 | 3.2e-05 | 12754 | 0.02 | 337 | 1.95e-05 | 51200 | 3558.5 | 3.2e-05 | 26809.5 | 0.02 | 226.0 | 0.0000518 | 19298.142 | 149 | 3.2e-05 | 13445 | 0.02 | 532.0 | 0.0000199 | 50141.217 | 713 | 3.2e-05 | 23660.5 | 0.02 | 171.0 | 0.0002491 | 4014.0622 | 37 | 3.2e-05 | 6152 | 0.02 | 377.0 | 0.0004554 | 2196.0002 | 37 | 3.2e-05 | 7636 | 0.02 | 483.0 | 0.0000423 | 23632.795 | 387 | 3.2e-05 | 23226 | 0.02 | 234 | 0.0002323 | 4304.3814 | 69 | 3.2e-05 | 2751.5 | 0.02 | 229.0 | 0.0006105 | 1638.0163 | 37 | 3.2e-05 | 3007 | 0.02 | 319 | 0.0031701 | 315.45144 | 17 | 3.2e-05 | 1459 | 0.02 | 256.0 | 0.0038047 | 262.83255 | 12 | 3.2e-05 | 988 | 0.02 | 634 | 0.0026246 | 381.01243 | 20 | 3.2e-05 | 2892 | 0.02 | 0.0000195 | 0.0000384 | 0.0000281 | 0.0000566 | 0.0000195 | 0.0000865 | 0.0000687 | 1.95e-05 |
| ABC-0003 | 15(1,B3) | B3 | Unknown15 | plate1 | 716 | 6.573680 | 8.631057 | 0.02 | -0.255195 | 1.721846 | 14.73796 | 0.0020262 | 1.430762 | 0.0020814 | 584.0 | 6.369901 | 9.847314 | 0.02 | -0.5988432 | 5.710997 | 10.13687 | 5.39e-05 | 2.526623 | 4.43e-05 | 4397.0 | 8.388678 | 9.278139 | 0.02 | -0.1812443 | -7.423291 | 13.38976 | 5.53e-05 | 0.750427 | 0.0049878 | 271.0 | 5.602119 | 7.38399 | 0.02 | -0.3931855 | 2.262344 | 9.049039 | 7.59e-05 | 2.686889 | 0.0015958 | pass | 634.0 | 0.0001442 | 6936.335 | 159 | 3.2e-05 | 17367 | 0.02 | 663 | 0.0001601 | 6246.280 | 174.5 | 3.2e-05 | 12754 | 0.02 | 656 | 1.95e-05 | 51200 | 3558.5 | 3.2e-05 | 26809.5 | 0.02 | 673.5 | 0.0001845 | 5420.041 | 149 | 3.2e-05 | 13445 | 0.02 | 1390.5 | 0.0000777 | 12863.776 | 713 | 3.2e-05 | 23660.5 | 0.02 | 671.0 | 0.0013071 | 765.0369 | 37 | 3.2e-05 | 6152 | 0.02 | 731.5 | 0.0009975 | 1002.5234 | 37 | 3.2e-05 | 7636 | 0.02 | 519.0 | 0.0000463 | 21621.543 | 387 | 3.2e-05 | 23226 | 0.02 | 596 | 0.0009122 | 1096.3018 | 69 | 3.2e-05 | 2751.5 | 0.02 | 569.5 | 0.0020438 | 489.2749 | 37 | 3.2e-05 | 3007 | 0.02 | 913 | 0.0112317 | 89.03411 | 17 | 3.2e-05 | 1459 | 0.02 | 637.5 | 0.0114129 | 87.62002 | 12 | 3.2e-05 | 988 | 0.02 | 2061 | 0.0120404 | 83.05405 | 20 | 3.2e-05 | 2892 | 0.02 | 0.0000784 | 0.0001591 | 0.0001564 | 0.0000969 | 0.0000195 | 0.0001867 | 0.0003170 | 3.09e-05 |
| ABC-0004 | 16(1,B4) | B4 | Unknown16 | plate1 | 357 | 5.877736 | 8.631057 | 0.02 | -0.255195 | 1.721846 | 14.73796 | 0.0020262 | 1.430762 | 0.0009267 | 235.0 | 5.459586 | 9.847314 | 0.02 | -0.5988432 | 5.710997 | 10.13687 | 5.39e-05 | 2.526623 | 1.95e-05 | 437.5 | 6.081077 | 9.278139 | 0.02 | -0.1812443 | -7.423291 | 13.38976 | 5.53e-05 | 0.750427 | 0.0002184 | 143.5 | 4.966335 | 7.38399 | 0.02 | -0.3931855 | 2.262344 | 9.049039 | 7.59e-05 | 2.686889 | 0.0007403 | pass | 238.0 | 0.0000487 | 20520.215 | 159 | 3.2e-05 | 17367 | 0.02 | 283 | 0.0000575 | 17378.322 | 174.5 | 3.2e-05 | 12754 | 0.02 | 318 | 1.95e-05 | 51200 | 3558.5 | 3.2e-05 | 26809.5 | 0.02 | 269.0 | 0.0000634 | 15768.912 | 149 | 3.2e-05 | 13445 | 0.02 | 582.0 | 0.0000233 | 42963.675 | 713 | 3.2e-05 | 23660.5 | 0.02 | 226.0 | 0.0003518 | 2842.7736 | 37 | 3.2e-05 | 6152 | 0.02 | 419.0 | 0.0005154 | 1940.4208 | 37 | 3.2e-05 | 7636 | 0.02 | 205.5 | 0.0000195 | 51200.000 | 387 | 3.2e-05 | 23226 | 0.02 | 266 | 0.0002796 | 3575.9145 | 69 | 3.2e-05 | 2751.5 | 0.02 | 265.0 | 0.0007440 | 1344.0629 | 37 | 3.2e-05 | 3007 | 0.02 | 322 | 0.0032063 | 311.89007 | 17 | 3.2e-05 | 1459 | 0.02 | 261.5 | 0.0039019 | 256.28686 | 12 | 3.2e-05 | 988 | 0.02 | 854 | 0.0037705 | 265.21947 | 20 | 3.2e-05 | 2892 | 0.02 | 0.0000195 | 0.0000484 | 0.0000408 | 0.0000542 | 0.0000195 | 0.0000978 | 0.0000959 | 1.95e-05 |
| ABC-0005 | 17(1,B5) | B5 | Unknown17 | plate1 | 1320 | 7.185387 | 8.631057 | 0.02 | -0.255195 | 1.721846 | 14.73796 | 0.0020262 | 1.430762 | 0.0041184 | 746.0 | 6.614726 | 9.847314 | 0.02 | -0.5988432 | 5.710997 | 10.13687 | 5.39e-05 | 2.526623 | 6.74e-05 | 1620.0 | 7.390181 | 9.278139 | 0.02 | -0.1812443 | -7.423291 | 13.38976 | 5.53e-05 | 0.750427 | 0.0011922 | 778.0 | 6.656727 | 7.38399 | 0.02 | -0.3931855 | 2.262344 | 9.049039 | 7.59e-05 | 2.686889 | 0.0063384 | pass | 746.0 | 0.0001737 | 5757.718 | 159 | 3.2e-05 | 17367 | 0.02 | 892 | 0.0002294 | 4360.066 | 174.5 | 3.2e-05 | 12754 | 0.02 | 1352 | 1.95e-05 | 51200 | 3558.5 | 3.2e-05 | 26809.5 | 0.02 | 918.5 | 0.0002673 | 3740.619 | 149 | 3.2e-05 | 13445 | 0.02 | 1768.0 | 0.0001038 | 9632.643 | 713 | 3.2e-05 | 23660.5 | 0.02 | 666.5 | 0.0012967 | 771.2116 | 37 | 3.2e-05 | 6152 | 0.02 | 1220.5 | 0.0018551 | 539.0675 | 37 | 3.2e-05 | 7636 | 0.02 | 760.5 | 0.0000735 | 13605.973 | 387 | 3.2e-05 | 23226 | 0.02 | 846 | 0.0015869 | 630.1508 | 69 | 3.2e-05 | 2751.5 | 0.02 | 801.0 | 0.0031965 | 312.8449 | 37 | 3.2e-05 | 3007 | 0.02 | 1048 | 0.0132789 | 75.30767 | 17 | 3.2e-05 | 1459 | 0.02 | 852.0 | 0.0164593 | 60.75578 | 12 | 3.2e-05 | 988 | 0.02 | 2191 | 0.0131474 | 76.06085 | 20 | 3.2e-05 | 2892 | 0.02 | 0.0001145 | 0.0001912 | 0.0002249 | 0.0001941 | 0.0000417 | 0.0003388 | 0.0003148 | 5.54e-05 |
| ABC-0006 | 18(1,B6) | B6 | Unknown18 | plate1 | 899 | 6.801283 | 8.631057 | 0.02 | -0.255195 | 1.721846 | 14.73796 | 0.0020262 | 1.430762 | 0.0026892 | 733.0 | 6.597146 | 9.847314 | 0.02 | -0.5988432 | 5.710997 | 10.13687 | 5.39e-05 | 2.526623 | 6.55e-05 | 2702.5 | 7.901933 | 9.278139 | 0.02 | -0.1812443 | -7.423291 | 13.38976 | 5.53e-05 | 0.750427 | 0.0024354 | 1015.0 | 6.922644 | 7.38399 | 0.02 | -0.3931855 | 2.262344 | 9.049039 | 7.59e-05 | 2.686889 | 0.0094357 | pass | 677.5 | 0.0001555 | 6430.683 | 159 | 3.2e-05 | 17367 | 0.02 | 895 | 0.0002303 | 4342.230 | 174.5 | 3.2e-05 | 12754 | 0.02 | 861 | 1.95e-05 | 51200 | 3558.5 | 3.2e-05 | 26809.5 | 0.02 | 888.0 | 0.0002566 | 3896.375 | 149 | 3.2e-05 | 13445 | 0.02 | 1728.0 | 0.0001010 | 9898.976 | 713 | 3.2e-05 | 23660.5 | 0.02 | 579.0 | 0.0010959 | 912.4657 | 37 | 3.2e-05 | 6152 | 0.02 | 967.5 | 0.0013973 | 715.6797 | 37 | 3.2e-05 | 7636 | 0.02 | 4955.0 | 0.0007581 | 1319.146 | 387 | 3.2e-05 | 23226 | 0.02 | 835 | 0.0015532 | 643.8484 | 69 | 3.2e-05 | 2751.5 | 0.02 | 788.5 | 0.0031310 | 319.3872 | 37 | 3.2e-05 | 3007 | 0.02 | 1040 | 0.0131555 | 76.01376 | 17 | 3.2e-05 | 1459 | 0.02 | 829.0 | 0.0158912 | 62.92790 | 12 | 3.2e-05 | 988 | 0.02 | 2080 | 0.0121993 | 81.97173 | 20 | 3.2e-05 | 2892 | 0.02 | 0.0001100 | 0.0001715 | 0.0002259 | 0.0001240 | 0.0006634 | 0.0002583 | 0.0002714 | 5.36e-05 |

MFI and RAU only:

``` r

results_5stdcurve$mfi_outputs$MFI_RAU %>%
  head() %>% 
  kable()
```

| SampleID | Plate | Pk8_MFI | PkMSP10_MFI | PkSERA3Ag2_MFI | PkSSP2_MFI | Pv-fam-a_MFI | PvMSP5_MFI | PvMSP1-19_MFI | PvEBP_MFI | PvRBP2b_MFI | PvCSS_MFI | PvPTEX150_MFI | PvMSP8_MFI | PfMSP1-19_MFI | PfAMA1_MFI | Pfetramp5Ag1_MFI | PfHSP40Ag1_MFI | PfGexp18_MFI | Pk8_Dilution | PkMSP10_Dilution | PkSERA3Ag2_Dilution | PkSSP2_Dilution | Pv-fam-a_loglog_Dilution | PvMSP5_loglog_Dilution | PvMSP1-19_loglog_Dilution | PvEBP_loglog_Dilution | PvRBP2b_loglog_Dilution | PvCSS_loglog_Dilution | PvPTEX150_loglog_Dilution | PvMSP8_loglog_Dilution | PfMSP1-19_loglog_Dilution | PfAMA1_loglog_Dilution | Pfetramp5Ag1_loglog_Dilution | PfHSP40Ag1_loglog_Dilution | PfGexp18_loglog_Dilution | PvEBP_Adjloglog_Dilution | Pv-fam-a_Adjloglog_Dilution | PvMSP5_Adjloglog_Dilution | PvMSP1-19_Adjloglog_Dilution | PvMSP8_Adjloglog_Dilution | PvPTEX150_Adjloglog_Dilution | PvCSS_Adjloglog_Dilution | PvRBP2b_Adjloglog_Dilution |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| ABC-0001 | plate1 | 601 | 346.0 | 858.0 | 320.0 | 389.0 | 392 | 502 | 387.0 | 831.5 | 438.0 | 556.0 | 303.0 | 363 | 359.5 | 467 | 384.0 | 1113 | 0.0017050 | 1.95e-05 | 0.0005129 | 0.0019577 | 0.0000834 | 0.0000852 | 1.95e-05 | 0.0000966 | 0.0000400 | 0.0007845 | 0.0007194 | 0.0000233 | 0.0004380 | 0.0011172 | 0.0050186 | 0.0061629 | 0.0052569 | 0.0000258 | 0.0000923 | 0.0000800 | 0.0000762 | 0.0000195 | 0.0001358 | 0.0002014 | 1.95e-05 |
| ABC-0002 | plate1 | 344 | 228.5 | 384.0 | 112.5 | 217.0 | 242 | 337 | 226.0 | 532.0 | 171.0 | 377.0 | 483.0 | 234 | 229.0 | 319 | 256.0 | 634 | 0.0008863 | 1.95e-05 | 0.0001858 | 0.0005509 | 0.0000441 | 0.0000476 | 1.95e-05 | 0.0000518 | 0.0000199 | 0.0002491 | 0.0004554 | 0.0000423 | 0.0002323 | 0.0006105 | 0.0031701 | 0.0038047 | 0.0026246 | 0.0000195 | 0.0000384 | 0.0000281 | 0.0000566 | 0.0000195 | 0.0000865 | 0.0000687 | 1.95e-05 |
| ABC-0003 | plate1 | 716 | 584.0 | 4397.0 | 271.0 | 634.0 | 663 | 656 | 673.5 | 1390.5 | 671.0 | 731.5 | 519.0 | 596 | 569.5 | 913 | 637.5 | 2061 | 0.0020814 | 4.43e-05 | 0.0049878 | 0.0015958 | 0.0001442 | 0.0001601 | 1.95e-05 | 0.0001845 | 0.0000777 | 0.0013071 | 0.0009975 | 0.0000463 | 0.0009122 | 0.0020438 | 0.0112317 | 0.0114129 | 0.0120404 | 0.0000784 | 0.0001591 | 0.0001564 | 0.0000969 | 0.0000195 | 0.0001867 | 0.0003170 | 3.09e-05 |
| ABC-0004 | plate1 | 357 | 235.0 | 437.5 | 143.5 | 238.0 | 283 | 318 | 269.0 | 582.0 | 226.0 | 419.0 | 205.5 | 266 | 265.0 | 322 | 261.5 | 854 | 0.0009267 | 1.95e-05 | 0.0002184 | 0.0007403 | 0.0000487 | 0.0000575 | 1.95e-05 | 0.0000634 | 0.0000233 | 0.0003518 | 0.0005154 | 0.0000195 | 0.0002796 | 0.0007440 | 0.0032063 | 0.0039019 | 0.0037705 | 0.0000195 | 0.0000484 | 0.0000408 | 0.0000542 | 0.0000195 | 0.0000978 | 0.0000959 | 1.95e-05 |
| ABC-0005 | plate1 | 1320 | 746.0 | 1620.0 | 778.0 | 746.0 | 892 | 1352 | 918.5 | 1768.0 | 666.5 | 1220.5 | 760.5 | 846 | 801.0 | 1048 | 852.0 | 2191 | 0.0041184 | 6.74e-05 | 0.0011922 | 0.0063384 | 0.0001737 | 0.0002294 | 1.95e-05 | 0.0002673 | 0.0001038 | 0.0012967 | 0.0018551 | 0.0000735 | 0.0015869 | 0.0031965 | 0.0132789 | 0.0164593 | 0.0131474 | 0.0001145 | 0.0001912 | 0.0002249 | 0.0001941 | 0.0000417 | 0.0003388 | 0.0003148 | 5.54e-05 |
| ABC-0006 | plate1 | 899 | 733.0 | 2702.5 | 1015.0 | 677.5 | 895 | 861 | 888.0 | 1728.0 | 579.0 | 967.5 | 4955.0 | 835 | 788.5 | 1040 | 829.0 | 2080 | 0.0026892 | 6.55e-05 | 0.0024354 | 0.0094357 | 0.0001555 | 0.0002303 | 1.95e-05 | 0.0002566 | 0.0001010 | 0.0010959 | 0.0013973 | 0.0007581 | 0.0015532 | 0.0031310 | 0.0131555 | 0.0158912 | 0.0121993 | 0.0001100 | 0.0001715 | 0.0002259 | 0.0001240 | 0.0006634 | 0.0002583 | 0.0002714 | 5.36e-05 |

MFI and RAU long table:

``` r

results_5stdcurve$mfi_outputs$MFI_RAU_long %>%
  head() %>% 
  kable()
```

| SampleID | Plate  | Antigens   | Species | MFI |       RAU | RAU_Method |
|:---------|:-------|:-----------|:--------|----:|----------:|:-----------|
| ABC-0001 | plate1 | Pk8        | Pk      | 601 | 0.0017050 | loglog     |
| ABC-0001 | plate1 | PkMSP10    | Pk      | 346 | 0.0000195 | loglog     |
| ABC-0001 | plate1 | PkSERA3Ag2 | Pk      | 858 | 0.0005129 | loglog     |
| ABC-0001 | plate1 | PkSSP2     | Pk      | 320 | 0.0019577 | loglog     |
| ABC-0001 | plate1 | Pv-fam-a   | Pv      | 389 | 0.0000834 | loglog     |
| ABC-0001 | plate1 | Pv-fam-a   | Pv      | 389 | 0.0000923 | Adj_loglog |

### 10-Point Standard Curve

These steps are very similar to the 5-point standard curve, except where
indicated.

#### Step 1: Load your data!

``` r

your_raw_data_10std <- c(
  system.file("extdata", "example_MAGPIX_pk_10std_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_pk_10std_plate2.csv", package = "SeroTrackR")
)
your_plate_layout_10std <- system.file("extdata", "example_platelayout_pk_10std.xlsx", package = "SeroTrackR")
```

#### Step 2: Read your data and process MFI to RAU

``` r

results_10stdcurve <- runPlasmoPipeline(
  raw_data = your_raw_data_10std,
  platform = "magpix",
  plate_layout = your_plate_layout_10std,
  panel = "panel1",
  std_point = 10, ################################### here make sure you write 10! 
  experiment_name = "10-point standard curve"
)
#> PASS: File example_magpix_pk_10std_plate1.csv successfully validated.
#> PASS: File example_magpix_pk_10std_plate2.csv successfully validated.
#> Plate layouts correctly identified!
#> QC Processes completed.
#> QC Plotting completed.
#> MFI to RAU conversion completed.
#> Pv classification completed.
```

##### Standard Curve Plot

``` r

results_10stdcurve$std_curve
```

![](Pk_Pv_Pf_Serology_Tutorial_files/figure-html/unnamed-chunk-12-1.png)

##### Bead Counts QC Plot

``` r

results_10stdcurve$bead_counts
```

![](Pk_Pv_Pf_Serology_Tutorial_files/figure-html/unnamed-chunk-13-1.png)

##### Blanks QC Ploat

``` r

results_10stdcurve$blanks
```

![](Pk_Pv_Pf_Serology_Tutorial_files/figure-html/unnamed-chunk-14-1.png)

##### MFI to RAU Tables

All results:

``` r

results_10stdcurve$mfi_outputs$All_Results %>%
  head() %>% 
  kable()
```

| SampleID | Location | Location.2 | Sample | Plate | Pk8_MFI | Pk8_log_mfi | Pk8_max_s1 | Pk8_max_dil | Pk8_slope | Pk8_low_asym | Pk8_upp_asym | Pk8_ed50 | Pk8_asym_par | Pk8_Dilution | PkMSP10_MFI | PkMSP10_log_mfi | PkMSP10_max_s1 | PkMSP10_max_dil | PkMSP10_slope | PkMSP10_low_asym | PkMSP10_upp_asym | PkMSP10_ed50 | PkMSP10_asym_par | PkMSP10_Dilution | PkSERA3Ag2_MFI | PkSERA3Ag2_log_mfi | PkSERA3Ag2_max_s1 | PkSERA3Ag2_max_dil | PkSERA3Ag2_slope | PkSERA3Ag2_low_asym | PkSERA3Ag2_upp_asym | PkSERA3Ag2_ed50 | PkSERA3Ag2_asym_par | PkSERA3Ag2_Dilution | PkSSP2_MFI | PkSSP2_log_mfi | PkSSP2_max_s1 | PkSSP2_max_dil | PkSSP2_slope | PkSSP2_low_asym | PkSSP2_upp_asym | PkSSP2_ed50 | PkSSP2_asym_par | PkSSP2_Dilution | QC_total | Pv-fam-a_MFI | Pv-fam-a_loglog_Dilution | Pv-fam-a_DilutionReciprocal | Pv-fam-a_MinStd | Pv-fam-a_MaxDilution | Pv-fam-a_MaxStd | Pv-fam-a_MinDilution | PvMSP5_MFI | PvMSP5_loglog_Dilution | PvMSP5_DilutionReciprocal | PvMSP5_MinStd | PvMSP5_MaxDilution | PvMSP5_MaxStd | PvMSP5_MinDilution | PvMSP1-19_MFI | PvMSP1-19_loglog_Dilution | PvMSP1-19_DilutionReciprocal | PvMSP1-19_MinStd | PvMSP1-19_MaxDilution | PvMSP1-19_MaxStd | PvMSP1-19_MinDilution | PvEBP_MFI | PvEBP_loglog_Dilution | PvEBP_DilutionReciprocal | PvEBP_MinStd | PvEBP_MaxDilution | PvEBP_MaxStd | PvEBP_MinDilution | PvRBP2b_MFI | PvRBP2b_loglog_Dilution | PvRBP2b_DilutionReciprocal | PvRBP2b_MinStd | PvRBP2b_MaxDilution | PvRBP2b_MaxStd | PvRBP2b_MinDilution | PvCSS_MFI | PvCSS_loglog_Dilution | PvCSS_DilutionReciprocal | PvCSS_MinStd | PvCSS_MaxDilution | PvCSS_MaxStd | PvCSS_MinDilution | PvPTEX150_MFI | PvPTEX150_loglog_Dilution | PvPTEX150_DilutionReciprocal | PvPTEX150_MinStd | PvPTEX150_MaxDilution | PvPTEX150_MaxStd | PvPTEX150_MinDilution | PvMSP8_MFI | PvMSP8_loglog_Dilution | PvMSP8_DilutionReciprocal | PvMSP8_MinStd | PvMSP8_MaxDilution | PvMSP8_MaxStd | PvMSP8_MinDilution | PfMSP1-19_MFI | PfMSP1-19_loglog_Dilution | PfMSP1-19_DilutionReciprocal | PfMSP1-19_MinStd | PfMSP1-19_MaxDilution | PfMSP1-19_MaxStd | PfMSP1-19_MinDilution | PfAMA1_MFI | PfAMA1_loglog_Dilution | PfAMA1_DilutionReciprocal | PfAMA1_MinStd | PfAMA1_MaxDilution | PfAMA1_MaxStd | PfAMA1_MinDilution | Pfetramp5Ag1_MFI | Pfetramp5Ag1_loglog_Dilution | Pfetramp5Ag1_DilutionReciprocal | Pfetramp5Ag1_MinStd | Pfetramp5Ag1_MaxDilution | Pfetramp5Ag1_MaxStd | Pfetramp5Ag1_MinDilution | PfHSP40Ag1_MFI | PfHSP40Ag1_loglog_Dilution | PfHSP40Ag1_DilutionReciprocal | PfHSP40Ag1_MinStd | PfHSP40Ag1_MaxDilution | PfHSP40Ag1_MaxStd | PfHSP40Ag1_MinDilution | PfGexp18_MFI | PfGexp18_loglog_Dilution | PfGexp18_DilutionReciprocal | PfGexp18_MinStd | PfGexp18_MaxDilution | PfGexp18_MaxStd | PfGexp18_MinDilution | PvEBP_Adjloglog_Dilution | Pv-fam-a_Adjloglog_Dilution | PvMSP5_Adjloglog_Dilution | PvMSP1-19_Adjloglog_Dilution | PvMSP8_Adjloglog_Dilution | PvPTEX150_Adjloglog_Dilution | PvCSS_Adjloglog_Dilution | PvRBP2b_Adjloglog_Dilution |
|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| ABC-0001 | 25(1,C1) | C1 | Unknown25 | plate1 | 1953.0 | 7.577122 | 10.11523 | 0.02 | -4.204756 | -10.30704 | 8.564423 | 0.0006965 | 0.0135688 | 0.0002728 | 1095.0 | 6.998510 | 9.938372 | 0.02 | -3.189255 | 2.67481 | 8.666433 | 0.0005417 | 0.0673257 | 0.0001188 | 5761.0 | 8.658866 | 8.724207 | 0.02 | -3.933689 | 0.0330379 | 6.955764 | 0.0007694 | 0.0511217 | 0.0000195 | 901.5 | 6.804060 | 9.752665 | 0.02 | -3.128358 | 0.1051524 | 8.27011 | 0.0007186 | 0.045718 | 0.0001809 | pass | 1020 | 0.0009250 | 1081.0739 | 44 | 3.91e-05 | 13024 | 0.02 | 1987.0 | 0.0006427 | 1555.8711 | 161 | 3.91e-05 | 24023.5 | 0.02 | 5070 | 0.0019239 | 519.7885 | 121.5 | 3.91e-05 | 23687 | 0.02 | 3140 | 0.0011043 | 905.5270 | 116 | 3.91e-05 | 17664 | 0.02 | 15170.0 | 0.0200000 | 50.0000 | 86 | 3.91e-05 | 9648 | 0.02 | 672.0 | 0.0007168 | 1395.065 | 29 | 3.91e-05 | 13146.5 | 0.02 | 1350 | 0.0011266 | 887.5992 | 52.5 | 3.91e-05 | 14354 | 0.02 | 1751.0 | 0.0004915 | 2034.5996 | 167 | 3.91e-05 | 24512 | 0.02 | 1221 | 0.0200000 | 50.0000 | 26 | 3.91e-05 | 792.5 | 0.02 | 873.5 | 0.0022286 | 448.7084 | 42 | 3.91e-05 | 2658 | 0.02 | 1058.0 | 0.0200000 | 50.0000 | 23.5 | 3.91e-05 | 1009 | 0.02 | 863.0 | 0.0200000 | 50.00000 | 15 | 3.91e-05 | 438 | 0.02 | 2303.0 | 0.0014816 | 674.9243 | 211 | 3.91e-05 | 9475 | 0.02 | 0.0005117 | 0.0010659 | 0.0005896 | 0.0165594 | 0.0004472 | 0.0002156 | 0.0001776 | 0.0181453 |
| ABC-0002 | 26(1,C2) | C2 | Unknown26 | plate1 | 492.5 | 6.199495 | 10.11523 | 0.02 | -4.204756 | -10.30704 | 8.564423 | 0.0006965 | 0.0135688 | 0.0000666 | 296.5 | 5.692047 | 9.938372 | 0.02 | -3.189255 | 2.67481 | 8.666433 | 0.0005417 | 0.0673257 | 0.0000222 | 4583.0 | 8.430109 | 8.724207 | 0.02 | -3.933689 | 0.0330379 | 6.955764 | 0.0007694 | 0.0511217 | 0.0000195 | 214.5 | 5.368310 | 9.752665 | 0.02 | -3.128358 | 0.1051524 | 8.27011 | 0.0007186 | 0.045718 | 0.0000334 | pass | 329 | 0.0003249 | 3077.4498 | 44 | 3.91e-05 | 13024 | 0.02 | 369.5 | 0.0001130 | 8852.8029 | 161 | 3.91e-05 | 24023.5 | 0.02 | 521 | 0.0001926 | 5191.2030 | 121.5 | 3.91e-05 | 23687 | 0.02 | 346 | 0.0001220 | 8195.5655 | 116 | 3.91e-05 | 17664 | 0.02 | 778.0 | 0.0004303 | 2324.0862 | 86 | 3.91e-05 | 9648 | 0.02 | 264.5 | 0.0003275 | 3053.058 | 29 | 3.91e-05 | 13146.5 | 0.02 | 500 | 0.0004606 | 2171.2319 | 52.5 | 3.91e-05 | 14354 | 0.02 | 324.0 | 0.0000843 | 11861.3051 | 167 | 3.91e-05 | 24512 | 0.02 | 364 | 0.0032324 | 309.3656 | 26 | 3.91e-05 | 792.5 | 0.02 | 366.0 | 0.0006612 | 1512.4968 | 42 | 3.91e-05 | 2658 | 0.02 | 462.5 | 0.0037562 | 266.2252 | 23.5 | 3.91e-05 | 1009 | 0.02 | 368.5 | 0.0137004 | 72.99079 | 15 | 3.91e-05 | 438 | 0.02 | 1003.5 | 0.0003592 | 2784.0504 | 211 | 3.91e-05 | 9475 | 0.02 | 0.0000385 | 0.0003910 | 0.0000999 | 0.0019141 | 0.0000467 | 0.0000873 | 0.0000815 | 0.0002659 |
| ABC-0003 | 27(1,C3) | C3 | Unknown27 | plate1 | 136.5 | 4.916325 | 10.11523 | 0.02 | -4.204756 | -10.30704 | 8.564423 | 0.0006965 | 0.0135688 | 0.0000195 | 36.0 | 3.583519 | 9.938372 | 0.02 | -3.189255 | 2.67481 | 8.666433 | 0.0005417 | 0.0673257 | 0.0000195 | 465.0 | 6.142037 | 8.724207 | 0.02 | -3.933689 | 0.0330379 | 6.955764 | 0.0007694 | 0.0511217 | 0.0004228 | 63.0 | 4.143135 | 9.752665 | 0.02 | -3.128358 | 0.1051524 | 8.27011 | 0.0007186 | 0.045718 | 0.0000195 | pass | 64 | 0.0000636 | 15720.7543 | 44 | 3.91e-05 | 13024 | 0.02 | 50.0 | 0.0000195 | 51200.0000 | 161 | 3.91e-05 | 24023.5 | 0.02 | 113 | 0.0000375 | 26646.4247 | 121.5 | 3.91e-05 | 23687 | 0.02 | 45 | 0.0000195 | 51200.0000 | 116 | 3.91e-05 | 17664 | 0.02 | 726.0 | 0.0003985 | 2509.3247 | 86 | 3.91e-05 | 9648 | 0.02 | 101.5 | 0.0001440 | 6945.366 | 29 | 3.91e-05 | 13146.5 | 0.02 | 1124 | 0.0009516 | 1050.9046 | 52.5 | 3.91e-05 | 14354 | 0.02 | 52.0 | 0.0000195 | 51200.0000 | 167 | 3.91e-05 | 24512 | 0.02 | 66 | 0.0002067 | 4838.7924 | 26 | 3.91e-05 | 792.5 | 0.02 | 75.0 | 0.0000871 | 11484.8827 | 42 | 3.91e-05 | 2658 | 0.02 | 64.0 | 0.0001930 | 5180.1406 | 23.5 | 3.91e-05 | 1009 | 0.02 | 60.0 | 0.0003627 | 2756.99591 | 15 | 3.91e-05 | 438 | 0.02 | 137.0 | 0.0000195 | 51200.0000 | 211 | 3.91e-05 | 9475 | 0.02 | 0.0000195 | 0.0000557 | 0.0000195 | 0.0006034 | 0.0000195 | 0.0001827 | 0.0000299 | 0.0002456 |
| ABC-0004 | 28(1,C4) | C4 | Unknown28 | plate1 | 464.0 | 6.139885 | 10.11523 | 0.02 | -4.204756 | -10.30704 | 8.564423 | 0.0006965 | 0.0135688 | 0.0000625 | 688.5 | 6.534515 | 9.938372 | 0.02 | -3.189255 | 2.67481 | 8.666433 | 0.0005417 | 0.0673257 | 0.0000699 | 204.0 | 5.318120 | 8.724207 | 0.02 | -3.933689 | 0.0330379 | 6.955764 | 0.0007694 | 0.0511217 | 0.0002013 | 6687.0 | 8.807921 | 9.752665 | 0.02 | -3.128358 | 0.1051524 | 8.27011 | 0.0007186 | 0.045718 | 0.0000195 | pass | 777 | 0.0007167 | 1395.2938 | 44 | 3.91e-05 | 13024 | 0.02 | 4357.0 | 0.0015093 | 662.5724 | 161 | 3.91e-05 | 24023.5 | 0.02 | 273 | 0.0001013 | 9872.2137 | 121.5 | 3.91e-05 | 23687 | 0.02 | 998 | 0.0003335 | 2998.8421 | 116 | 3.91e-05 | 17664 | 0.02 | 2673.0 | 0.0019523 | 512.2274 | 86 | 3.91e-05 | 9648 | 0.02 | 213.0 | 0.0002731 | 3661.742 | 29 | 3.91e-05 | 13146.5 | 0.02 | 425 | 0.0003985 | 2509.2529 | 52.5 | 3.91e-05 | 14354 | 0.02 | 3718.0 | 0.0011290 | 885.7238 | 167 | 3.91e-05 | 24512 | 0.02 | 251 | 0.0016733 | 597.6085 | 26 | 3.91e-05 | 792.5 | 0.02 | 876.0 | 0.0022385 | 446.7223 | 42 | 3.91e-05 | 2658 | 0.02 | 275.0 | 0.0015646 | 639.1509 | 23.5 | 3.91e-05 | 1009 | 0.02 | 227.0 | 0.0045382 | 220.35135 | 15 | 3.91e-05 | 438 | 0.02 | 499.0 | 0.0001170 | 8546.1083 | 211 | 3.91e-05 | 9475 | 0.02 | 0.0001404 | 0.0008412 | 0.0013207 | 0.0011681 | 0.0010458 | 0.0000750 | 0.0000678 | 0.0011763 |
| ABC-0005 | 29(1,C5) | C5 | Unknown29 | plate1 | 2159.0 | 7.677400 | 10.11523 | 0.02 | -4.204756 | -10.30704 | 8.564423 | 0.0006965 | 0.0135688 | 0.0003016 | 1047.5 | 6.954162 | 9.938372 | 0.02 | -3.189255 | 2.67481 | 8.666433 | 0.0005417 | 0.0673257 | 0.0001132 | 1563.0 | 7.354362 | 8.724207 | 0.02 | -3.933689 | 0.0330379 | 6.955764 | 0.0007694 | 0.0511217 | 0.0000195 | 1002.5 | 6.910252 | 9.752665 | 0.02 | -3.128358 | 0.1051524 | 8.27011 | 0.0007186 | 0.045718 | 0.0002023 | pass | 1434 | 0.0012831 | 779.3828 | 44 | 3.91e-05 | 13024 | 0.02 | 2218.0 | 0.0007210 | 1387.0041 | 161 | 3.91e-05 | 24023.5 | 0.02 | 2318 | 0.0008246 | 1212.6887 | 121.5 | 3.91e-05 | 23687 | 0.02 | 3161 | 0.0011129 | 898.5615 | 116 | 3.91e-05 | 17664 | 0.02 | 14703.5 | 0.0200000 | 50.0000 | 86 | 3.91e-05 | 9648 | 0.02 | 1764.5 | 0.0016960 | 589.621 | 29 | 3.91e-05 | 13146.5 | 0.02 | 1730 | 0.0014235 | 702.4696 | 52.5 | 3.91e-05 | 14354 | 0.02 | 3042.5 | 0.0008983 | 1113.1736 | 167 | 3.91e-05 | 24512 | 0.02 | 903 | 0.0200000 | 50.0000 | 26 | 3.91e-05 | 792.5 | 0.02 | 696.5 | 0.0015869 | 630.1708 | 42 | 3.91e-05 | 2658 | 0.02 | 1259.0 | 0.0200000 | 50.0000 | 23.5 | 3.91e-05 | 1009 | 0.02 | 765.0 | 0.0200000 | 50.00000 | 15 | 3.91e-05 | 438 | 0.02 | 1867.0 | 0.0010253 | 975.2989 | 211 | 3.91e-05 | 9475 | 0.02 | 0.0005156 | 0.0014398 | 0.0006586 | 0.0080539 | 0.0008397 | 0.0002702 | 0.0004059 | 0.0180255 |
| ABC-0006 | 30(1,C6) | C6 | Unknown30 | plate1 | 313.5 | 5.747799 | 10.11523 | 0.02 | -4.204756 | -10.30704 | 8.564423 | 0.0006965 | 0.0135688 | 0.0000410 | 317.0 | 5.758902 | 9.938372 | 0.02 | -3.189255 | 2.67481 | 8.666433 | 0.0005417 | 0.0673257 | 0.0000246 | 4176.5 | 8.337229 | 8.724207 | 0.02 | -3.933689 | 0.0330379 | 6.955764 | 0.0007694 | 0.0511217 | 0.0000195 | 157.0 | 5.056246 | 9.752665 | 0.02 | -3.128358 | 0.1051524 | 8.27011 | 0.0007186 | 0.045718 | 0.0000218 | pass | 173 | 0.0001779 | 5620.5138 | 44 | 3.91e-05 | 13024 | 0.02 | 244.5 | 0.0000706 | 14166.0632 | 161 | 3.91e-05 | 24023.5 | 0.02 | 1211 | 0.0004337 | 2305.8063 | 121.5 | 3.91e-05 | 23687 | 0.02 | 717 | 0.0002429 | 4117.2900 | 116 | 3.91e-05 | 17664 | 0.02 | 1808.0 | 0.0011610 | 861.3537 | 86 | 3.91e-05 | 9648 | 0.02 | 164.0 | 0.0002188 | 4570.580 | 29 | 3.91e-05 | 13146.5 | 0.02 | 242 | 0.0002391 | 4182.3555 | 52.5 | 3.91e-05 | 14354 | 0.02 | 640.0 | 0.0001721 | 5809.2375 | 167 | 3.91e-05 | 24512 | 0.02 | 292 | 0.0021690 | 461.0460 | 26 | 3.91e-05 | 792.5 | 0.02 | 243.0 | 0.0003930 | 2544.3740 | 42 | 3.91e-05 | 2658 | 0.02 | 243.0 | 0.0012934 | 773.1434 | 23.5 | 3.91e-05 | 1009 | 0.02 | 213.5 | 0.0039846 | 250.96902 | 15 | 3.91e-05 | 438 | 0.02 | 410.0 | 0.0000861 | 11607.6908 | 211 | 3.91e-05 | 9475 | 0.02 | 0.0000975 | 0.0002066 | 0.0000592 | 0.0039599 | 0.0001246 | 0.0000393 | 0.0000524 | 0.0007162 |

MFI and RAU only:

``` r

results_10stdcurve$mfi_outputs$MFI_RAU %>%
  head() %>% 
  kable()
```

| SampleID | Plate | Pk8_MFI | PkMSP10_MFI | PkSERA3Ag2_MFI | PkSSP2_MFI | Pv-fam-a_MFI | PvMSP5_MFI | PvMSP1-19_MFI | PvEBP_MFI | PvRBP2b_MFI | PvCSS_MFI | PvPTEX150_MFI | PvMSP8_MFI | PfMSP1-19_MFI | PfAMA1_MFI | Pfetramp5Ag1_MFI | PfHSP40Ag1_MFI | PfGexp18_MFI | Pk8_Dilution | PkMSP10_Dilution | PkSERA3Ag2_Dilution | PkSSP2_Dilution | Pv-fam-a_loglog_Dilution | PvMSP5_loglog_Dilution | PvMSP1-19_loglog_Dilution | PvEBP_loglog_Dilution | PvRBP2b_loglog_Dilution | PvCSS_loglog_Dilution | PvPTEX150_loglog_Dilution | PvMSP8_loglog_Dilution | PfMSP1-19_loglog_Dilution | PfAMA1_loglog_Dilution | Pfetramp5Ag1_loglog_Dilution | PfHSP40Ag1_loglog_Dilution | PfGexp18_loglog_Dilution | PvEBP_Adjloglog_Dilution | Pv-fam-a_Adjloglog_Dilution | PvMSP5_Adjloglog_Dilution | PvMSP1-19_Adjloglog_Dilution | PvMSP8_Adjloglog_Dilution | PvPTEX150_Adjloglog_Dilution | PvCSS_Adjloglog_Dilution | PvRBP2b_Adjloglog_Dilution |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| ABC-0001 | plate1 | 1953.0 | 1095.0 | 5761.0 | 901.5 | 1020 | 1987.0 | 5070 | 3140 | 15170.0 | 672.0 | 1350 | 1751.0 | 1221 | 873.5 | 1058.0 | 863.0 | 2303.0 | 0.0002728 | 0.0001188 | 0.0000195 | 0.0001809 | 0.0009250 | 0.0006427 | 0.0019239 | 0.0011043 | 0.0200000 | 0.0007168 | 0.0011266 | 0.0004915 | 0.0200000 | 0.0022286 | 0.0200000 | 0.0200000 | 0.0014816 | 0.0005117 | 0.0010659 | 0.0005896 | 0.0165594 | 0.0004472 | 0.0002156 | 0.0001776 | 0.0181453 |
| ABC-0002 | plate1 | 492.5 | 296.5 | 4583.0 | 214.5 | 329 | 369.5 | 521 | 346 | 778.0 | 264.5 | 500 | 324.0 | 364 | 366.0 | 462.5 | 368.5 | 1003.5 | 0.0000666 | 0.0000222 | 0.0000195 | 0.0000334 | 0.0003249 | 0.0001130 | 0.0001926 | 0.0001220 | 0.0004303 | 0.0003275 | 0.0004606 | 0.0000843 | 0.0032324 | 0.0006612 | 0.0037562 | 0.0137004 | 0.0003592 | 0.0000385 | 0.0003910 | 0.0000999 | 0.0019141 | 0.0000467 | 0.0000873 | 0.0000815 | 0.0002659 |
| ABC-0003 | plate1 | 136.5 | 36.0 | 465.0 | 63.0 | 64 | 50.0 | 113 | 45 | 726.0 | 101.5 | 1124 | 52.0 | 66 | 75.0 | 64.0 | 60.0 | 137.0 | 0.0000195 | 0.0000195 | 0.0004228 | 0.0000195 | 0.0000636 | 0.0000195 | 0.0000375 | 0.0000195 | 0.0003985 | 0.0001440 | 0.0009516 | 0.0000195 | 0.0002067 | 0.0000871 | 0.0001930 | 0.0003627 | 0.0000195 | 0.0000195 | 0.0000557 | 0.0000195 | 0.0006034 | 0.0000195 | 0.0001827 | 0.0000299 | 0.0002456 |
| ABC-0004 | plate1 | 464.0 | 688.5 | 204.0 | 6687.0 | 777 | 4357.0 | 273 | 998 | 2673.0 | 213.0 | 425 | 3718.0 | 251 | 876.0 | 275.0 | 227.0 | 499.0 | 0.0000625 | 0.0000699 | 0.0002013 | 0.0000195 | 0.0007167 | 0.0015093 | 0.0001013 | 0.0003335 | 0.0019523 | 0.0002731 | 0.0003985 | 0.0011290 | 0.0016733 | 0.0022385 | 0.0015646 | 0.0045382 | 0.0001170 | 0.0001404 | 0.0008412 | 0.0013207 | 0.0011681 | 0.0010458 | 0.0000750 | 0.0000678 | 0.0011763 |
| ABC-0005 | plate1 | 2159.0 | 1047.5 | 1563.0 | 1002.5 | 1434 | 2218.0 | 2318 | 3161 | 14703.5 | 1764.5 | 1730 | 3042.5 | 903 | 696.5 | 1259.0 | 765.0 | 1867.0 | 0.0003016 | 0.0001132 | 0.0000195 | 0.0002023 | 0.0012831 | 0.0007210 | 0.0008246 | 0.0011129 | 0.0200000 | 0.0016960 | 0.0014235 | 0.0008983 | 0.0200000 | 0.0015869 | 0.0200000 | 0.0200000 | 0.0010253 | 0.0005156 | 0.0014398 | 0.0006586 | 0.0080539 | 0.0008397 | 0.0002702 | 0.0004059 | 0.0180255 |
| ABC-0006 | plate1 | 313.5 | 317.0 | 4176.5 | 157.0 | 173 | 244.5 | 1211 | 717 | 1808.0 | 164.0 | 242 | 640.0 | 292 | 243.0 | 243.0 | 213.5 | 410.0 | 0.0000410 | 0.0000246 | 0.0000195 | 0.0000218 | 0.0001779 | 0.0000706 | 0.0004337 | 0.0002429 | 0.0011610 | 0.0002188 | 0.0002391 | 0.0001721 | 0.0021690 | 0.0003930 | 0.0012934 | 0.0039846 | 0.0000861 | 0.0000975 | 0.0002066 | 0.0000592 | 0.0039599 | 0.0001246 | 0.0000393 | 0.0000524 | 0.0007162 |

MFI and RAU long table:

``` r

results_10stdcurve$mfi_outputs$MFI_RAU_long %>%
  head() %>% 
  kable()
```

| SampleID | Plate  | Antigens   | Species |    MFI |       RAU | RAU_Method |
|:---------|:-------|:-----------|:--------|-------:|----------:|:-----------|
| ABC-0001 | plate1 | Pk8        | Pk      | 1953.0 | 0.0002728 | loglog     |
| ABC-0001 | plate1 | PkMSP10    | Pk      | 1095.0 | 0.0001188 | loglog     |
| ABC-0001 | plate1 | PkSERA3Ag2 | Pk      | 5761.0 | 0.0000195 | loglog     |
| ABC-0001 | plate1 | PkSSP2     | Pk      |  901.5 | 0.0001809 | loglog     |
| ABC-0001 | plate1 | Pv-fam-a   | Pv      | 1020.0 | 0.0009250 | loglog     |
| ABC-0001 | plate1 | Pv-fam-a   | Pv      | 1020.0 | 0.0010659 | Adj_loglog |
