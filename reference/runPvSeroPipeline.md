# Run PvSero Pipeline from Start to End

A master function combining the entire PvSeroApp pipeline into one
command to run in R.

## Usage

``` r
runPvSeroPipeline(
  raw_data,
  plate_layout,
  platform,
  location,
  experiment_name,
  classify,
  algorithm_type,
  sens_spec
)
```

## Arguments

- raw_data:

  String with the raw data path.

- plate_layout:

  An ".xlsx" file with sheets labelled plate1, plate2... etc.

- platform:

  "magpix" or "bioplex".

- location:

  "PNG" or "ETH" to filter WEHI standard curve data.

- experiment_name:

  User-input experiment name.

- classify:

  "Yes" or "No" depending on whether you would like classification or
  not.

- algorithm_type:

  User-selected algorithm choice: - "antibody_model" (PvSeroTaT model;
  default), or - "antibody_model_excLF016" (PvSeroTat excluding LF016).

- sens_spec:

  User-selected Sensitivity/Specificity threshold: - "maximised"
  (default), - "85% sensitivity", - "90% sensitivity", - "95%
  sensitivity", - "85% specificity", - "90% specificity". - "95%
  specificity".

## Value

classifyResults_output, stdcurve_plot, plateqc_plot,
check_repeats_output, blanks_plot, model_plot

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

plate_layout <- system.file(
  "extdata", "example_platelayout_1.xlsx", package = "SeroTrackR"
)

# Run full pipeline including classification
runPvSeroPipeline(
  raw_data = your_raw_data,
  plate_layout = plate_layout,
  platform = "magpix",
  location = "PNG",
  experiment_name = "experiment1",
  algorithm_type = "antibody_model",
  sens_spec = "maximised",
  classify = "Yes"
)
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
#> Plate layouts correctly identified!
















#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced








#> [[1]]
#> # A tibble: 252 × 12
#>    SampleID Plate  QC_total      EBP    LF005    LF010   LF016    MSP8 RBP2b.P87
#>    <chr>    <chr>  <chr>       <dbl>    <dbl>    <dbl>   <dbl>   <dbl>     <dbl>
#>  1 ABC013   plate1 pass     0.000818 0.00140  0.000218 1.31e-4 5.44e-5  0.000577
#>  2 ABC097   plate2 pass     0.000935 0.00140  0.000213 1.22e-4 4.47e-5  0.00150 
#>  3 ABC181   plate3 pass     0.000925 0.00148  0.000210 1.25e-4 5.32e-5  0.000309
#>  4 ABC022   plate1 pass     0.02     0.0194   0.000813 3.41e-3 7.00e-4  0.000764
#>  5 ABC106   plate2 pass     0.02     0.0166   0.000821 3.38e-3 6.72e-4  0.02    
#>  6 ABC190   plate3 pass     0.02     0.0189   0.000817 3.83e-3 6.61e-4  0.02    
#>  7 ABC023   plate1 pass     0.000247 0.00668  0.000136 1.11e-4 1.54e-4  0.000882
#>  8 ABC107   plate2 pass     0.000268 0.00639  0.000125 1.01e-4 1.43e-4  0.0105  
#>  9 ABC191   plate3 pass     0.000267 0.00716  0.000131 1.07e-4 1.44e-4  0.02    
#> 10 ABC024   plate1 pass     0.000466 0.000352 0.000286 1.17e-4 8.97e-5  0.000318
#> # ℹ 242 more rows
#> # ℹ 3 more variables: PTEX150 <dbl>, PvCSS <dbl>, pred_class_max <fct>
#> 
#> [[2]]
#> Warning: Removed 250 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> [[3]]

#> 
#> [[4]]
#> # A tibble: 2 × 4
#>   Location SampleID Plate  QC   
#>   <chr>    <chr>    <chr>  <chr>
#> 1 A1       Blank1   plate2 fail 
#> 2 A2       Blank2   plate2 fail 
#> 
#> [[5]]

#> 
#> [[6]]
#> [[6]]$plate1

#> 
#> [[6]]$plate2

#> 
#> [[6]]$plate3

#> 
#> 

# Run processing pipeline only (no classification)
runPvSeroPipeline(
  raw_data = your_raw_data,
  plate_layout = plate_layout,
  platform = "magpix",
  location = "PNG",
  experiment_name = "experiment1",
  algorithm_type = "antibody_model",
  sens_spec = "maximised",
  classify = "No"
)
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
#> Plate layouts correctly identified!
















#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced








#> No Classification Performed
#> [[1]]
#>    SampleID  Plate EBP_MFI EBP_Dilution LF005_MFI LF005_Dilution LF010_MFI
#> 1    ABC013 plate1  2712.0 8.182796e-04    1569.0   1.398668e-03     673.0
#> 2    ABC014 plate1   134.0 5.053441e-05     378.0   3.780142e-04     117.0
#> 3    ABC015 plate1   182.0 6.722592e-05     209.0   2.221241e-04     208.0
#> 4    ABC016 plate1   152.0 5.692040e-05     229.5   2.418157e-04     101.0
#> 5    ABC017 plate1  1135.0 3.417053e-04     236.0   2.480000e-04     299.0
#> 6    ABC018 plate1   174.0 6.451441e-05     395.0   3.931059e-04     175.0
#> 7    ABC019 plate1   421.0 1.412644e-04    2081.5   1.859101e-03     529.0
#> 8    ABC020 plate1    24.0 1.953125e-05      49.0   5.075289e-05      22.0
#> 9    ABC021 plate1    24.0 1.953125e-05      45.0   4.570736e-05      22.0
#> 10   ABC022 plate1 21464.0 2.000000e-02   11789.0   1.944965e-02    2508.5
#> 11   ABC023 plate1   795.0 2.471109e-04    6135.5   6.676371e-03     407.0
#> 12   ABC024 plate1  1574.0 4.663222e-04     348.5   3.516345e-04     892.0
#> 13   ABC025 plate1   146.0 5.481051e-05     408.5   4.050389e-04    1130.5
#> 14   ABC026 plate1  1330.5 3.966820e-04    3036.0   2.787316e-03     965.0
#> 15   ABC027 plate1   358.0 1.226329e-04    4074.5   3.924989e-03     335.5
#> 16   ABC028 plate1   481.0 1.586773e-04    1870.0   1.666296e-03     421.0
#> 17   ABC029 plate1   551.0 1.786959e-04    1282.0   1.149793e-03     331.5
#> 18   ABC030 plate1   943.0 2.882284e-04     584.5   5.576178e-04     235.0
#> 19   ABC031 plate1   532.5 1.734309e-04    1533.0   1.367142e-03     315.0
#> 20   ABC032 plate1   768.0 2.395980e-04   12605.0   2.000000e-02     409.0
#> 21   ABC033 plate1   130.0 4.909114e-05     239.5   2.513190e-04      72.0
#> 22   ABC034 plate1    40.0 1.953125e-05      60.0   6.430349e-05      37.0
#> 23   ABC035 plate1   167.0 6.212092e-05     605.0   5.751386e-04     253.0
#> 24   ABC036 plate1  1879.0 5.559344e-04    4562.0   4.512033e-03     415.0
#> 25   ABC037 plate1   256.0 9.133650e-05    1507.0   1.344431e-03     117.0
#> 26   ABC038 plate1    24.0 1.953125e-05      49.0   5.075289e-05      22.0
#> 27   ABC039 plate1   384.0 1.303727e-04    1708.0   1.521330e-03     326.0
#> 28   ABC040 plate1   210.0 7.653680e-05     512.0   4.953121e-04     221.0
#> 29   ABC041 plate1   351.5 1.206854e-04     312.0   3.186046e-04     165.5
#> 30   ABC042 plate1  1278.0 3.818487e-04    5810.0   6.192024e-03     275.0
#> 31   ABC043 plate1   318.0 1.105589e-04     397.0   3.948765e-04     130.0
#> 32   ABC044 plate1   493.5 1.622727e-04    2980.5   2.730482e-03     401.0
#> 33   ABC045 plate1  9686.0 5.159788e-03     873.0   8.022609e-04     333.0
#> 34   ABC046 plate1   777.5 2.422423e-04     581.0   5.546227e-04     241.5
#> 35   ABC047 plate1   455.0 1.511653e-04     416.5   4.120901e-04     126.0
#> 36   ABC048 plate1   202.0 7.390301e-05    1142.0   1.030151e-03     137.5
#> 37   ABC049 plate1   333.5 1.152638e-04     623.5   5.909205e-04     368.0
#> 38   ABC050 plate1   561.0 1.815352e-04    7984.0   9.852361e-03     441.0
#> 39   ABC051 plate1   506.5 1.660017e-04     340.0   3.439839e-04     152.5
#> 40   ABC052 plate1   254.0 9.070386e-05     367.0   3.682075e-04     392.5
#> 41   ABC053 plate1   888.0 2.729521e-04    1955.0   1.743283e-03     438.0
#> 42   ABC054 plate1   426.0 1.427265e-04    1877.0   1.672611e-03     186.0
#> 43   ABC055 plate1  5733.0 2.096608e-03    1301.5   1.166539e-03    1920.5
#> 44   ABC056 plate1 19092.0 2.000000e-02   14422.0   2.000000e-02    1993.0
#> 45   ABC057 plate1   596.0 1.914395e-04   15139.0   2.000000e-02     371.5
#> 46   ABC058 plate1  1028.0 3.118619e-04    2646.0   2.395742e-03    1486.0
#> 47   ABC059 plate1   512.0 1.675763e-04    4735.0   4.729224e-03    3408.0
#> 48   ABC060 plate1   883.0 2.715635e-04    1953.0   1.741464e-03     755.0
#> 49   ABC061 plate1   977.0 2.976765e-04    8719.0   1.134563e-02     509.0
#> 50   ABC062 plate1   791.0 2.459983e-04    1944.0   1.733283e-03     465.0
#>    LF010_Dilution LF016_MFI LF016_Dilution MSP8_MFI MSP8_Dilution RBP2b.P87_MFI
#> 1    2.181300e-04     327.0   1.305542e-04    182.0  5.436126e-05        1068.0
#> 2    4.187987e-05     197.0   8.290863e-05     58.0  1.953125e-05         465.5
#> 3    7.244372e-05     374.0   1.472006e-04    221.5  6.569336e-05         463.0
#> 4    3.629942e-05      89.0   4.001593e-05     48.0  1.953125e-05         591.0
#> 5    1.018002e-04     507.0   1.933107e-04    209.5  6.226189e-05         671.0
#> 6    6.154529e-05      78.0   3.533402e-05     70.0  2.128036e-05         103.0
#> 7    1.737193e-04     149.0   6.439171e-05    962.0  2.752517e-04        1441.5
#> 8    1.953125e-05      13.0   1.953125e-05     15.5  1.953125e-05          14.0
#> 9    1.953125e-05      13.0   1.953125e-05     16.0  1.953125e-05          13.0
#> 10   8.134573e-04    8001.0   3.414201e-03   2342.0  6.996377e-04        1386.0
#> 11   1.358280e-04     273.0   1.111023e-04    535.5  1.541357e-04        1575.0
#> 12   2.856499e-04     288.0   1.165460e-04    306.0  8.966439e-05         590.0
#> 13   3.597459e-04      83.0   3.747243e-05    100.0  3.034480e-05         301.0
#> 14   3.082439e-04     174.0   7.412102e-05     79.0  2.402206e-05        1090.0
#> 15   1.133742e-04     140.0   6.083676e-05    390.0  1.133016e-04        6704.0
#> 16   1.401983e-04     182.5   7.738537e-05   1777.5  5.191597e-04        2390.0
#> 17   1.121101e-04     165.0   7.064152e-05     77.0  2.341461e-05         244.0
#> 18   8.124600e-05     135.0   5.884846e-05   3017.0  9.289961e-04        1034.0
#> 19   1.068849e-04     210.0   8.781582e-05   2575.5  7.772751e-04        2903.0
#> 20   1.364528e-04    7632.0   3.201596e-03   9860.0  4.221942e-03       13703.0
#> 21   2.594600e-05      64.0   2.924549e-05     37.0  1.953125e-05         156.0
#> 22   1.953125e-05      25.0   1.953125e-05     26.5  1.953125e-05          23.5
#> 23   8.706662e-05     329.0   1.312674e-04   3973.0  1.280320e-03        1426.5
#> 24   1.383262e-04   18287.0   1.359724e-02  11482.5  5.304395e-03       12031.0
#> 25   4.187987e-05     129.0   5.644918e-05    168.0  5.031588e-05         488.0
#> 26   1.953125e-05      12.0   1.953125e-05     15.0  1.953125e-05          14.0
#> 27   1.103703e-04    1172.5   4.164149e-04    986.0  2.821752e-04        1041.0
#> 28   7.669334e-05     191.0   8.062980e-05     89.0  2.704492e-05         203.0
#> 29   5.837395e-05      88.0   3.959365e-05     66.0  2.005477e-05         205.0
#> 30   9.413631e-05     230.5   9.547693e-05    137.0  4.128794e-05        2032.0
#> 31   4.635759e-05     130.5   5.705040e-05     78.0  2.371846e-05         903.0
#> 32   1.339527e-04    5932.5   2.310032e-03   3536.0  1.115788e-03        4699.5
#> 33   1.125842e-04     166.0   7.102935e-05    791.0  2.262954e-04        1093.0
#> 34   8.335195e-05     173.0   7.373561e-05   1479.5  4.278469e-04         899.0
#> 35   4.498479e-05      67.0   3.056359e-05     48.0  1.953125e-05         400.0
#> 36   4.892045e-05     176.0   7.489096e-05    204.5  6.082943e-05        1090.0
#> 37   1.236114e-04   10430.0   5.010026e-03   2340.0  6.989804e-04        4375.0
#> 38   1.464296e-04     128.0   5.604783e-05    141.0  4.245904e-05        1993.0
#> 39   5.400584e-05     147.0   6.360430e-05     90.0  2.734596e-05         600.0
#> 40   1.312936e-04      89.0   4.001593e-05    142.5  4.289769e-05         280.0
#> 41   1.454958e-04     173.0   7.373561e-05    507.0  1.461340e-04        1009.0
#> 42   6.519736e-05    9767.0   4.538200e-03  15945.0  9.031437e-03       10073.0
#> 43   6.135859e-04     220.5   9.175101e-05   4833.0  1.624070e-03        2340.0
#> 44   6.376833e-04   12836.5   6.991252e-03   7426.0  2.831247e-03       11930.0
#> 45   1.247106e-04     229.0   9.491935e-05    535.5  1.541357e-04       14032.0
#> 46   4.721291e-04     292.0   1.179921e-04   9357.0  3.912473e-03        8009.0
#> 47   1.140308e-03     127.5   5.584700e-05    191.0  5.695312e-05        4401.0
#> 48   2.433848e-04     586.0   2.202248e-04    122.0  3.687718e-05         830.0
#> 49   1.675320e-04     187.0   7.910541e-05   1289.0  3.708318e-04       12930.0
#> 50   1.538910e-04     216.5   9.025475e-05  12307.0  5.906439e-03        5029.0
#>    RBP2b.P87_Dilution PTEX150_MFI PTEX150_Dilution PvCSS_MFI PvCSS_Dilution
#> 1        5.770510e-04       936.0     8.125690e-04     223.0   2.842341e-04
#> 2        2.524154e-04       122.0     1.229382e-04      93.0   1.353895e-04
#> 3        2.510989e-04       293.0     2.780605e-04     868.0   8.856529e-04
#> 4        3.183967e-04       109.0     1.103057e-04     110.0   1.567520e-04
#> 5        3.606033e-04      1665.5     1.423688e-03     266.0   3.287867e-04
#> 6        4.861577e-05       294.5     2.793637e-04      92.0   1.341049e-04
#> 7        7.980419e-04       241.0     2.324246e-04     282.0   3.450091e-04
#> 8        1.953125e-05        28.0     2.575707e-05      17.0   2.164514e-05
#> 9        1.953125e-05        29.0     2.688183e-05      17.0   2.164514e-05
#> 10       7.638780e-04      1992.0     1.707851e-03    2927.0   2.805690e-03
#> 11       8.823387e-04      8821.0     1.036477e-02     998.0   1.000753e-03
#> 12       3.178705e-04       315.0     2.971101e-04     366.0   4.277861e-04
#> 13       1.647135e-04       652.0     5.792227e-04     130.0   1.809064e-04
#> 14       5.895294e-04       179.0     1.765117e-04      92.0   1.341049e-04
#> 15       1.065547e-02      8814.0     1.035246e-02    5627.0   6.008259e-03
#> 16       1.474938e-03      2465.0     2.134466e-03     411.0   4.708536e-04
#> 17       1.332709e-04       579.0     5.190072e-04     229.0   2.905443e-04
#> 18       5.578835e-04      4928.0     4.709636e-03    7730.0   9.301583e-03
#> 19       1.934755e-03      2779.0     2.428423e-03    7300.0   8.559487e-03
#> 20       2.000000e-02      5612.0     5.547489e-03    6383.0   7.098724e-03
#> 21       8.220839e-05       204.0     1.992948e-04      49.0   7.478435e-05
#> 22       1.953125e-05        40.0     3.911440e-05      27.0   3.958033e-05
#> 23       7.887589e-04       803.0     7.033066e-04    1670.0   1.600665e-03
#> 24       2.000000e-02      4830.5     4.594927e-03    4217.0   4.214407e-03
#> 25       2.642547e-04       908.0     7.895473e-04      87.0   1.276303e-04
#> 26       1.953125e-05        29.0     2.688183e-05      17.0   2.164514e-05
#> 27       5.618183e-04       299.5     2.837029e-04     149.0   2.030662e-04
#> 28       1.099804e-04       410.0     3.780905e-04     174.5   2.318402e-04
#> 29       1.111337e-04       200.0     1.956732e-04     183.0   2.412229e-04
#> 30       1.196476e-03      1762.0     1.506865e-03    1241.0   1.215924e-03
#> 31       4.852711e-04      1513.5     1.293954e-03     325.0   3.878312e-04
#> 32       4.411958e-03       574.0     5.148730e-04    5389.0   5.684531e-03
#> 33       5.912357e-04       417.5     3.844135e-04     262.0   3.247034e-04
#> 34       4.830827e-04      1504.0     1.285895e-03     260.0   3.226574e-04
#> 35       2.178176e-04       183.0     1.801817e-04      61.0   9.230420e-05
#> 36       5.895294e-04       504.0     4.568121e-04     459.0   5.160639e-04
#> 37       3.829610e-03      1650.0     1.410389e-03    7801.5   9.428736e-03
#> 38       1.167973e-03      2147.0     1.845608e-03    1502.0   1.448918e-03
#> 39       3.231337e-04      2178.0     1.873393e-03     170.0   2.268333e-04
#> 40       1.532301e-04       298.0     2.824019e-04     138.0   1.903211e-04
#> 41       5.438779e-04       709.0     6.261066e-04    2040.0   1.941416e-03
#> 42       2.000000e-02      1585.5     1.355218e-03    1124.0   1.112219e-03
#> 43       1.434126e-03      5329.0     5.193682e-03    2369.0   2.253465e-03
#> 44       2.000000e-02      4826.0     4.589660e-03    3468.5   3.372929e-03
#> 45       2.000000e-02      7928.5     8.863319e-03    1414.5   1.370483e-03
#> 46       2.000000e-02      3736.0     3.382990e-03   14370.5   2.000000e-02
#> 47       3.873514e-03     11567.0     1.593571e-02    2923.5   2.802129e-03
#> 48       4.455731e-04       425.5     3.911488e-04     121.0   1.701532e-04
#> 49       2.000000e-02     13891.0     2.000000e-02    9531.0   1.286498e-02
#> 50       5.088748e-03      4157.0     3.833152e-03     760.0   7.896233e-04
#>    Location.2 QC_total
#> 1          B1     pass
#> 2          B2     pass
#> 3          B3     pass
#> 4          B4     pass
#> 5          B5     pass
#> 6          B6     pass
#> 7          B7     pass
#> 8          B8     pass
#> 9          B9     pass
#> 10        B10     pass
#> 11        B11     pass
#> 12        B12     pass
#> 13         C1     pass
#> 14         C2     pass
#> 15         C3     pass
#> 16         C4     pass
#> 17         C5     pass
#> 18         C6     pass
#> 19         C7     pass
#> 20         C8     pass
#> 21         C9     pass
#> 22        C10     pass
#> 23        C11     pass
#> 24        C12     pass
#> 25         D1     pass
#> 26         D2     pass
#> 27         D3     pass
#> 28         D4     pass
#> 29         D5     pass
#> 30         D6     pass
#> 31         D7     pass
#> 32         D8     pass
#> 33         D9     pass
#> 34        D10     pass
#> 35        D11     pass
#> 36        D12     pass
#> 37         E1     pass
#> 38         E2     pass
#> 39         E3     pass
#> 40         E4     pass
#> 41         E5     pass
#> 42         E6     pass
#> 43         E7     pass
#> 44         E8     pass
#> 45         E9     pass
#> 46        E10     pass
#> 47        E11     pass
#> 48        E12     pass
#> 49         F1     pass
#> 50         F2     pass
#>  [ reached 'max' / getOption("max.print") -- omitted 202 rows ]
#> 
#> [[2]]
#> Warning: Removed 250 rows containing missing values or values outside the scale range
#> (`geom_point()`).

#> 
#> [[3]]

#> 
#> [[4]]
#> # A tibble: 2 × 4
#>   Location SampleID Plate  QC   
#>   <chr>    <chr>    <chr>  <chr>
#> 1 A1       Blank1   plate2 fail 
#> 2 A2       Blank2   plate2 fail 
#> 
#> [[5]]

#> 
#> [[6]]
#> [[6]]$plate1

#> 
#> [[6]]$plate2

#> 
#> [[6]]$plate3

#> 
#> 
# }
```
