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
#>  4 ABC022   plate1 pass     0.02     0.0194   0.000814 3.42e-3 7.00e-4  0.000764
#>  5 ABC106   plate2 pass     0.02     0.0166   0.000822 3.38e-3 6.72e-4  0.02    
#>  6 ABC190   plate3 pass     0.02     0.0189   0.000817 3.83e-3 6.61e-4  0.02    
#>  7 ABC023   plate1 pass     0.000247 0.00668  0.000136 1.11e-4 1.54e-4  0.000882
#>  8 ABC107   plate2 pass     0.000268 0.00639  0.000125 1.01e-4 1.43e-4  0.0105  
#>  9 ABC191   plate3 pass     0.000267 0.00716  0.000131 1.07e-4 1.44e-4  0.02    
#> 10 ABC024   plate1 pass     0.000466 0.000351 0.000285 1.17e-4 8.97e-5  0.000318
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
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced








#> No Classification Performed
#> [[1]]
#>    SampleID  Plate EBP_MFI EBP_Dilution LF005_MFI LF005_Dilution LF010_MFI
#> 1    ABC013 plate1  2712.0 8.182495e-04    1569.0   1.398405e-03     673.0
#> 2    ABC014 plate1   134.0 5.054767e-05     378.0   3.778390e-04     117.0
#> 3    ABC015 plate1   182.0 6.723758e-05     209.0   2.221228e-04     208.0
#> 4    ABC016 plate1   152.0 5.693370e-05     229.5   2.417926e-04     101.0
#> 5    ABC017 plate1  1135.0 3.415984e-04     236.0   2.479699e-04     299.0
#> 6    ABC018 plate1   174.0 6.452668e-05     395.0   3.929151e-04     175.0
#> 7    ABC019 plate1   421.0 1.412406e-04    2081.5   1.859233e-03     529.0
#> 8    ABC020 plate1    24.0 1.953125e-05      49.0   5.076359e-05      22.0
#> 9    ABC021 plate1    24.0 1.953125e-05      45.0   4.570091e-05      22.0
#> 10   ABC022 plate1 21464.0 2.000000e-02   11789.0   1.942067e-02    2508.5
#> 11   ABC023 plate1   795.0 2.470337e-04    6135.5   6.681742e-03     407.0
#> 12   ABC024 plate1  1574.0 4.662028e-04     348.5   3.514874e-04     892.0
#> 13   ABC025 plate1   146.0 5.482390e-05     408.5   4.048361e-04    1130.5
#> 14   ABC026 plate1  1330.5 3.965660e-04    3036.0   2.788578e-03     965.0
#> 15   ABC027 plate1   358.0 1.226195e-04    4074.5   3.927784e-03     335.5
#> 16   ABC028 plate1   481.0 1.586437e-04    1870.0   1.666242e-03     421.0
#> 17   ABC029 plate1   551.0 1.786514e-04    1282.0   1.149400e-03     331.5
#> 18   ABC030 plate1   943.0 2.881360e-04     584.5   5.572872e-04     235.0
#> 19   ABC031 plate1   532.5 1.733892e-04    1533.0   1.366858e-03     315.0
#> 20   ABC032 plate1   768.0 2.395240e-04   12605.0   2.000000e-02     409.0
#> 21   ABC033 plate1   130.0 4.910425e-05     239.5   2.512851e-04      72.0
#> 22   ABC034 plate1    40.0 1.953125e-05      60.0   6.434784e-05      37.0
#> 23   ABC035 plate1   167.0 6.213362e-05     605.0   5.747965e-04     253.0
#> 24   ABC036 plate1  1879.0 5.558223e-04    4562.0   4.515563e-03     415.0
#> 25   ABC037 plate1   256.0 9.133934e-05    1507.0   1.344134e-03     117.0
#> 26   ABC038 plate1    24.0 1.953125e-05      49.0   5.076359e-05      22.0
#> 27   ABC039 plate1   384.0 1.303550e-04    1708.0   1.521155e-03     326.0
#> 28   ABC040 plate1   210.0 7.654569e-05     512.0   4.950277e-04     221.0
#> 29   ABC041 plate1   351.5 1.206731e-04     312.0   3.184940e-04     165.5
#> 30   ABC042 plate1  1278.0 3.817346e-04    5810.0   6.197129e-03     275.0
#> 31   ABC043 plate1   318.0 1.105521e-04     397.0   3.946840e-04     130.0
#> 32   ABC044 plate1   493.5 1.622371e-04    2980.5   2.731668e-03     401.0
#> 33   ABC045 plate1  9686.0 5.162702e-03     873.0   8.018278e-04     333.0
#> 34   ABC046 plate1   777.5 2.421671e-04     581.0   5.542941e-04     241.5
#> 35   ABC047 plate1   455.0 1.511358e-04     416.5   4.118804e-04     126.0
#> 36   ABC048 plate1   202.0 7.391279e-05    1142.0   1.029723e-03     137.5
#> 37   ABC049 plate1   333.5 1.152544e-04     623.5   5.905686e-04     368.0
#> 38   ABC050 plate1   561.0 1.814891e-04    7984.0   9.856898e-03     441.0
#> 39   ABC051 plate1   506.5 1.659640e-04     340.0   3.438451e-04     152.5
#> 40   ABC052 plate1   254.0 9.070700e-05     367.0   3.680426e-04     392.5
#> 41   ABC053 plate1   888.0 2.728650e-04    1955.0   1.743300e-03     438.0
#> 42   ABC054 plate1   426.0 1.427018e-04    1877.0   1.672563e-03     186.0
#> 43   ABC055 plate1  5733.0 2.097550e-03    1301.5   1.166151e-03    1920.5
#> 44   ABC056 plate1 19092.0 2.000000e-02   14422.0   2.000000e-02    1993.0
#> 45   ABC057 plate1   596.0 1.913883e-04   15139.0   2.000000e-02     371.5
#> 46   ABC058 plate1  1028.0 3.117624e-04    2646.0   2.396494e-03    1486.0
#> 47   ABC059 plate1   512.0 1.675378e-04    4735.0   4.733006e-03    3408.0
#> 48   ABC060 plate1   883.0 2.714769e-04    1953.0   1.741479e-03     755.0
#> 49   ABC061 plate1   977.0 2.975812e-04    8719.0   1.134809e-02     509.0
#> 50   ABC062 plate1   791.0 2.459216e-04    1944.0   1.733290e-03     465.0
#>    LF010_Dilution LF016_MFI LF016_Dilution MSP8_MFI MSP8_Dilution RBP2b.P87_MFI
#> 1    2.178084e-04     327.0   1.306707e-04    182.0  5.436126e-05        1068.0
#> 2    4.196174e-05     197.0   8.302613e-05     58.0  1.953125e-05         465.5
#> 3    7.236242e-05     374.0   1.472968e-04    221.5  6.569336e-05         463.0
#> 4    3.641901e-05      89.0   3.999430e-05     48.0  1.953125e-05         591.0
#> 5    1.016155e-04     507.0   1.933215e-04    209.5  6.226189e-05         671.0
#> 6    6.151453e-05      78.0   3.528282e-05     70.0  2.128036e-05         103.0
#> 7    1.734141e-04     149.0   6.447283e-05    962.0  2.752517e-04        1441.5
#> 8    1.953125e-05      13.0   1.953125e-05     15.5  1.953125e-05          14.0
#> 9    1.953125e-05      13.0   1.953125e-05     16.0  1.953125e-05          13.0
#> 10   8.140407e-04    8001.0   3.420227e-03   2342.0  6.996377e-04        1386.0
#> 11   1.355677e-04     273.0   1.112315e-04    535.5  1.541357e-04        1575.0
#> 12   2.853540e-04     288.0   1.166731e-04    306.0  8.966439e-05         590.0
#> 13   3.595234e-04      83.0   3.743520e-05    100.0  3.034480e-05         301.0
#> 14   3.079663e-04     174.0   7.422478e-05     79.0  2.402206e-05        1090.0
#> 15   1.131592e-04     140.0   6.090736e-05    390.0  1.133016e-04        6704.0
#> 16   1.399310e-04     182.5   7.749492e-05   1777.5  5.191597e-04        2390.0
#> 17   1.118981e-04     165.0   7.073815e-05     77.0  2.341461e-05         244.0
#> 18   8.112909e-05     135.0   5.891259e-05   3017.0  9.289961e-04        1034.0
#> 19   1.066862e-04     210.0   8.793862e-05   2575.5  7.772751e-04        2903.0
#> 20   1.361915e-04    7632.0   3.206758e-03   9860.0  4.221942e-03       13703.0
#> 21   2.614266e-05      64.0   2.914986e-05     37.0  1.953125e-05         156.0
#> 22   1.953125e-05      25.0   1.953125e-05     26.5  1.953125e-05          23.5
#> 23   8.692846e-05     329.0   1.313832e-04   3973.0  1.280320e-03        1426.5
#> 24   1.380619e-04   18287.0   1.359981e-02  11482.5  5.304395e-03       12031.0
#> 25   4.196174e-05     129.0   5.650493e-05    168.0  5.031588e-05         488.0
#> 26   1.953125e-05      12.0   1.953125e-05     15.0  1.953125e-05          14.0
#> 27   1.101626e-04    1172.5   4.158674e-04    986.0  2.821752e-04        1041.0
#> 28   7.659431e-05     191.0   8.074429e-05     89.0  2.704492e-05         203.0
#> 29   5.835938e-05      88.0   3.956951e-05     66.0  2.005477e-05         205.0
#> 30   9.397456e-05     230.5   9.560499e-05    137.0  4.128794e-05        2032.0
#> 31   4.641112e-05     130.5   5.710831e-05     78.0  2.371846e-05         903.0
#> 32   1.336957e-04    5932.5   2.311793e-03   3536.0  1.115788e-03        4699.5
#> 33   1.123711e-04     166.0   7.112683e-05    791.0  2.262954e-04        1093.0
#> 34   8.322715e-05     173.0   7.383864e-05   1479.5  4.278469e-04         899.0
#> 35   4.504683e-05      67.0   3.047818e-05     48.0  1.953125e-05         400.0
#> 36   4.895851e-05     176.0   7.499616e-05    204.5  6.082943e-05        1090.0
#> 37   1.233738e-04   10430.0   5.022324e-03   2340.0  6.989804e-04        4375.0
#> 38   1.461532e-04     128.0   5.610212e-05    141.0  4.245904e-05        1993.0
#> 39   5.401475e-05     147.0   6.368320e-05     90.0  2.734596e-05         600.0
#> 40   1.310412e-04      89.0   3.999430e-05    142.5  4.289769e-05         280.0
#> 41   1.452207e-04     173.0   7.383864e-05    507.0  1.461340e-04        1009.0
#> 42   6.514881e-05    9767.0   4.548744e-03  15945.0  9.031437e-03       10073.0
#> 43   6.137798e-04     220.5   9.187695e-05   4833.0  1.624070e-03        2340.0
#> 44   6.379229e-04   12836.5   7.009058e-03   7426.0  2.831247e-03       11930.0
#> 45   1.244707e-04     229.0   9.504715e-05    535.5  1.541357e-04       14032.0
#> 46   4.720703e-04     292.0   1.181184e-04   9357.0  3.912473e-03        8009.0
#> 47   1.141523e-03     127.5   5.590055e-05    191.0  5.695312e-05        4401.0
#> 48   2.430669e-04     586.0   2.201736e-04    122.0  3.687718e-05         830.0
#> 49   1.672319e-04     187.0   7.921768e-05   1289.0  3.708318e-04       12930.0
#> 50   1.536050e-04     216.5   9.037961e-05  12307.0  5.906439e-03        5029.0
#>    RBP2b.P87_Dilution PTEX150_MFI PTEX150_Dilution PvCSS_MFI PvCSS_Dilution
#> 1        5.770203e-04       936.0     8.128171e-04     223.0   2.842248e-04
#> 2        2.524148e-04       122.0     1.229479e-04      93.0   1.354042e-04
#> 3        2.510985e-04       293.0     2.781573e-04     868.0   8.855372e-04
#> 4        3.183903e-04       109.0     1.103090e-04     110.0   1.567646e-04
#> 5        3.605930e-04      1665.5     1.423876e-03     266.0   3.287681e-04
#> 6        4.862604e-05       294.5     2.794612e-04      92.0   1.341197e-04
#> 7        7.979900e-04       241.0     2.324964e-04     282.0   3.449872e-04
#> 8        1.953125e-05        28.0     2.574712e-05      17.0   2.163190e-05
#> 9        1.953125e-05        29.0     2.687120e-05      17.0   2.163190e-05
#> 10       7.638294e-04      1992.0     1.707962e-03    2927.0   2.805598e-03
#> 11       8.822789e-04      8821.0     1.036465e-02     998.0   1.000625e-03
#> 12       3.178642e-04       315.0     2.972169e-04     366.0   4.277468e-04
#> 13       1.647200e-04       652.0     5.794357e-04     130.0   1.809159e-04
#> 14       5.894975e-04       179.0     1.765515e-04      92.0   1.341197e-04
#> 15       1.065393e-02      8814.0     1.035233e-02    5627.0   6.008717e-03
#> 16       1.474821e-03      2465.0     2.134432e-03     411.0   4.708055e-04
#> 17       1.332795e-04       579.0     5.192039e-04     229.0   2.905337e-04
#> 18       5.578546e-04      4928.0     4.708619e-03    7730.0   9.302737e-03
#> 19       1.934594e-03      2779.0     2.428274e-03    7300.0   8.560487e-03
#> 20       2.000000e-02      5612.0     5.546272e-03    6383.0   7.099412e-03
#> 21       8.221907e-05       204.0     1.993478e-04      49.0   7.479745e-05
#> 22       1.953125e-05        40.0     3.909921e-05      27.0   3.958143e-05
#> 23       7.887080e-04       803.0     7.035435e-04    1670.0   1.600506e-03
#> 24       2.000000e-02      4830.5     4.593944e-03    4217.0   4.214517e-03
#> 25       2.642532e-04       908.0     7.897937e-04      87.0   1.276455e-04
#> 26       1.953125e-05        29.0     2.687120e-05      17.0   2.163190e-05
#> 27       5.617890e-04       299.5     2.838027e-04     149.0   2.030722e-04
#> 28       1.099902e-04       410.0     3.782357e-04     174.5   2.318412e-04
#> 29       1.111434e-04       200.0     1.957241e-04     183.0   2.412221e-04
#> 30       1.196387e-03      1762.0     1.507033e-03    1241.0   1.215777e-03
#> 31       4.852491e-04      1513.5     1.294168e-03     325.0   3.878002e-04
#> 32       4.411515e-03       574.0     5.150685e-04    5389.0   5.684922e-03
#> 33       5.912036e-04       417.5     3.845614e-04     262.0   3.246857e-04
#> 34       4.830609e-04      1504.0     1.286110e-03     260.0   3.226402e-04
#> 35       2.178200e-04       183.0     1.802236e-04      61.0   9.231935e-05
#> 36       5.894975e-04       504.0     4.569885e-04     459.0   5.160069e-04
#> 37       3.829240e-03      1650.0     1.410579e-03    7801.5   9.429917e-03
#> 38       1.167886e-03      2147.0     1.845676e-03    1502.0   1.448762e-03
#> 39       3.231269e-04      2178.0     1.873451e-03     170.0   2.268351e-04
#> 40       1.532373e-04       298.0     2.825010e-04     138.0   1.903292e-04
#> 41       5.438503e-04       709.0     6.263302e-04    2040.0   1.941261e-03
#> 42       2.000000e-02      1585.5     1.355420e-03    1124.0   1.112080e-03
#> 43       1.434013e-03      5329.0     5.192538e-03    2369.0   2.253325e-03
#> 44       2.000000e-02      4826.0     4.588679e-03    3468.5   3.372907e-03
#> 45       2.000000e-02      7928.5     8.862400e-03    1414.5   1.370329e-03
#> 46       2.000000e-02      3736.0     3.382447e-03   14370.5   2.000000e-02
#> 47       3.873139e-03     11567.0     1.594242e-02    2923.5   2.802036e-03
#> 48       4.455548e-04       425.5     3.912996e-04     121.0   1.701642e-04
#> 49       2.000000e-02     13891.0     2.000000e-02    9531.0   1.286680e-02
#> 50       5.088214e-03      4157.0     3.832432e-03     760.0   7.895203e-04
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
