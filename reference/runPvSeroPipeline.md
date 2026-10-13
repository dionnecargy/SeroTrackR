# Run PvSero Pipeline from Start to End

A master function combining the entire PvSeroApp pipeline into one
command to run in R.

## Usage

``` r
runPvSeroPipeline(
  raw_data,
  plate_layout,
  platform = "magpix",
  location,
  experiment_name = "experiment1",
  std_point = 10,
  classify = "Yes",
  algorithm_type = "antibody_model",
  sens_spec = "balanced"
)
```

## Arguments

- raw_data:

  String with the raw data path.

- plate_layout:

  An ".xlsx" file with sheets labelled plate1, plate2... etc.

- platform:

  "magpix" or "bioplex". Default = "magpix".

- location:

  "PNG" or "ETH" to filter WEHI standard curve data.

- experiment_name:

  User-input experiment name.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = 10. Value is an integer.

- classify:

  "Yes" or "No" depending on whether you would like classification or
  not. Default = "Yes".

- algorithm_type:

  Algorithm: "antibody_model" (PvSEM algorithm; default)

- sens_spec:

  User-selected Sensitivity/Specificity threshold: "balanced" (default)
  or "90% specificity".

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
  std_point = 10,
  algorithm_type = "antibody_model",
  sens_spec = "balanced",
  classify = "Yes"
)
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
#> Plate layouts correctly identified!
#> QC Processes completed.
#> MFI to RAU conversion completed.
#> Please write a standard point curve.
#> QC Plotting completed.
#> Pv classification completed.
#> [[1]]
#> # A tibble: 252 × 12
#>    SampleID Plate  QC_total    PvEBP `Pv-fam-a`   PvMSP5 `PvMSP1-19`    PvMSP8
#>    <chr>    <chr>  <chr>       <dbl>      <dbl>    <dbl>       <dbl>     <dbl>
#>  1 ABC013   plate1 pass     0.000819   0.00140  0.000218    0.000131 0.0000544
#>  2 ABC097   plate2 pass     0.000935   0.00140  0.000213    0.000122 0.0000447
#>  3 ABC181   plate3 pass     0.000925   0.00148  0.000210    0.000125 0.0000532
#>  4 ABC022   plate1 pass     0.02       0.0194   0.000814    0.00342  0.000700 
#>  5 ABC106   plate2 pass     0.02       0.0166   0.000821    0.00337  0.000672 
#>  6 ABC190   plate3 pass     0.02       0.0189   0.000817    0.00383  0.000661 
#>  7 ABC023   plate1 pass     0.000247   0.00668  0.000136    0.000111 0.000154 
#>  8 ABC107   plate2 pass     0.000268   0.00639  0.000125    0.000101 0.000143 
#>  9 ABC191   plate3 pass     0.000267   0.00716  0.000131    0.000107 0.000144 
#> 10 ABC024   plate1 pass     0.000466   0.000352 0.000285    0.000117 0.0000897
#> # ℹ 242 more rows
#> # ℹ 4 more variables: PvRBP2b <dbl>, PvPTEX150 <dbl>, PvCSS <dbl>,
#> #   pred_class_max <fct>
#> 
#> [[2]]
#> NULL
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
#> [[6]]$`1`

#> 
#> [[6]]$`2`

#> 
#> [[6]]$`3`

#> 
#> 

# Run processing pipeline only (no classification)
runPvSeroPipeline(
  raw_data = your_raw_data,
  plate_layout = plate_layout,
  platform = "magpix",
  location = "PNG",
  experiment_name = "experiment1",
  std_point = 10,
  algorithm_type = "antibody_model",
  sens_spec = "balanced",
  classify = "No"
)
#> PASS: File example_magpix_plate1.csv successfully validated.
#> PASS: File example_magpix_plate2.csv successfully validated.
#> PASS: File example_magpix_plate3.csv successfully validated.
#> Plate layouts correctly identified!
#> QC Processes completed.
#> MFI to RAU conversion completed.
#> Please write a standard point curve.
#> QC Plotting completed.
#> No Classification Performed.
#> [[1]]
#>    SampleID  Plate PvEBP_MFI PvEBP_Dilution Pv-fam-a_MFI Pv-fam-a_Dilution
#> 1    ABC013 plate1    2712.0   8.185533e-04       1569.0      1.398841e-03
#> 2    ABC014 plate1     134.0   5.055885e-05        378.0      3.779156e-04
#> 3    ABC015 plate1     182.0   6.721225e-05        209.0      2.220911e-04
#> 4    ABC016 plate1     152.0   5.692953e-05        229.5      2.417732e-04
#> 5    ABC017 plate1    1135.0   3.415387e-04        236.0      2.479545e-04
#> 6    ABC018 plate1     174.0   6.450652e-05        395.0      3.930025e-04
#> 7    ABC019 plate1     421.0   1.411405e-04       2081.5      1.859620e-03
#> 8    ABC020 plate1      24.0   1.953125e-05         49.0      5.075042e-05
#> 9    ABC021 plate1      24.0   1.953125e-05         45.0      4.569902e-05
#> 10   ABC022 plate1   21464.0   2.000000e-02      11789.0      1.942726e-02
#> 11   ABC023 plate1     795.0   2.469327e-04       6135.5      6.679931e-03
#> 12   ABC024 plate1    1574.0   4.662245e-04        348.5      3.515449e-04
#> 13   ABC025 plate1     146.0   5.482459e-05        408.5      4.049320e-04
#> 14   ABC026 plate1    1330.5   3.965397e-04       3036.0      2.788654e-03
#> 15   ABC027 plate1     358.0   1.225294e-04       4074.5      3.927296e-03
#> 16   ABC028 plate1     481.0   1.585378e-04       1870.0      1.666662e-03
#> 17   ABC029 plate1     551.0   1.785422e-04       1282.0      1.149818e-03
#> 18   ABC030 plate1     943.0   2.880498e-04        584.5      5.574847e-04
#> 19   ABC031 plate1     532.5   1.732805e-04       1533.0      1.367294e-03
#> 20   ABC032 plate1     768.0   2.394209e-04      12605.0      2.000000e-02
#> 21   ABC033 plate1     130.0   4.911917e-05        239.5      2.512719e-04
#> 22   ABC034 plate1      40.0   1.953125e-05         60.0      6.431166e-05
#> 23   ABC035 plate1     167.0   6.211826e-05        605.0      5.750046e-04
#> 24   ABC036 plate1    1879.0   5.559120e-04       4562.0      4.514763e-03
#> 25   ABC037 plate1     256.0   9.127763e-05       1507.0      1.344569e-03
#> 26   ABC038 plate1      24.0   1.953125e-05         49.0      5.075042e-05
#> 27   ABC039 plate1     384.0   1.302602e-04       1708.0      1.521588e-03
#> 28   ABC040 plate1     210.0   7.650440e-05        512.0      4.951856e-04
#> 29   ABC041 plate1     351.5   1.205843e-04        312.0      3.185277e-04
#> 30   ABC042 plate1    1278.0   3.816987e-04       5810.0      6.195511e-03
#> 31   ABC043 plate1     318.0   1.104710e-04        397.0      3.947726e-04
#> 32   ABC044 plate1     493.5   1.621303e-04       2980.5      2.731769e-03
#> 33   ABC045 plate1    9686.0   5.163963e-03        873.0      8.021501e-04
#> 34   ABC046 plate1     777.5   2.420647e-04        581.0      5.544898e-04
#> 35   ABC047 plate1     455.0   1.510321e-04        416.5      4.119813e-04
#> 36   ABC048 plate1     202.0   7.387574e-05       1142.0      1.030118e-03
#> 37   ABC049 plate1     333.5   1.151695e-04        623.5      5.907861e-04
#> 38   ABC050 plate1     561.0   1.813797e-04       7984.0      9.854575e-03
#> 39   ABC051 plate1     506.5   1.658565e-04        340.0      3.438971e-04
#> 40   ABC052 plate1     254.0   9.064605e-05        367.0      3.681121e-04
#> 41   ABC053 plate1     888.0   2.727726e-04       1955.0      1.743708e-03
#> 42   ABC054 plate1     426.0   1.426011e-04       1877.0      1.672982e-03
#> 43   ABC055 plate1    5733.0   2.098717e-03       1301.5      1.166572e-03
#> 44   ABC056 plate1   19092.0   2.000000e-02      14422.0      2.000000e-02
#> 45   ABC057 plate1     596.0   1.912785e-04      15139.0      2.000000e-02
#> 46   ABC058 plate1    1028.0   3.116870e-04       2646.0      2.396727e-03
#> 47   ABC059 plate1     512.0   1.674300e-04       4735.0      4.732092e-03
#> 48   ABC060 plate1     883.0   2.713840e-04       1953.0      1.741888e-03
#> 49   ABC061 plate1     977.0   2.974991e-04       8719.0      1.134606e-02
#> 50   ABC062 plate1     791.0   2.458203e-04       1944.0      1.733700e-03
#>    PvMSP5_MFI PvMSP5_Dilution PvMSP1-19_MFI PvMSP1-19_Dilution PvMSP8_MFI
#> 1       673.0    2.178084e-04         327.0       1.306386e-04      182.0
#> 2       117.0    4.196174e-05         197.0       8.298787e-05       58.0
#> 3       208.0    7.236242e-05         374.0       1.472735e-04      221.5
#> 4       101.0    3.641901e-05          89.0       3.999920e-05       48.0
#> 5       299.0    1.016155e-04         507.0       1.933315e-04      209.5
#> 6       175.0    6.151453e-05          78.0       3.529767e-05       70.0
#> 7       529.0    1.734141e-04         149.0       6.444475e-05      962.0
#> 8        22.0    1.953125e-05          13.0       1.953125e-05       15.5
#> 9        22.0    1.953125e-05          13.0       1.953125e-05       16.0
#> 10     2508.5    8.140407e-04        8001.0       3.417705e-03     2342.0
#> 11      407.0    1.355677e-04         273.0       1.111928e-04      535.5
#> 12      892.0    2.853540e-04         288.0       1.166358e-04      306.0
#> 13     1130.5    3.595234e-04          83.0       3.744533e-05      100.0
#> 14      965.0    3.079663e-04         174.0       7.419012e-05       79.0
#> 15      335.5    1.131592e-04         140.0       6.088248e-05      390.0
#> 16      421.0    1.399310e-04         182.5       7.745868e-05     1777.5
#> 17      331.5    1.118981e-04         165.0       7.070550e-05       77.0
#> 18      235.0    8.112909e-05         135.0       5.888971e-05     3017.0
#> 19      315.0    1.066862e-04         210.0       8.789917e-05     2575.5
#> 20      409.0    1.361915e-04        7632.0       3.204559e-03     9860.0
#> 21       72.0    2.614266e-05          64.0       2.917991e-05       37.0
#> 22       37.0    1.953125e-05          25.0       1.953125e-05       26.5
#> 23      253.0    8.692846e-05         329.0       1.313515e-04     3973.0
#> 24      415.0    1.380619e-04       18287.0       1.360074e-02    11482.5
#> 25      117.0    4.196174e-05         129.0       5.648467e-05      168.0
#> 26       22.0    1.953125e-05          12.0       1.953125e-05       15.0
#> 27      326.0    1.101626e-04        1172.5       4.160713e-04      986.0
#> 28      221.0    7.659431e-05         191.0       8.070677e-05       89.0
#> 29      165.5    5.835938e-05          88.0       3.957525e-05       66.0
#> 30      275.0    9.397456e-05         230.5       9.556472e-05      137.0
#> 31      130.0    4.641112e-05         130.5       5.708737e-05       78.0
#> 32      401.0    1.336957e-04        5932.5       2.310900e-03     3536.0
#> 33      333.0    1.123711e-04         166.0       7.109394e-05      791.0
#> 34      241.5    8.322715e-05         173.0       7.380418e-05     1479.5
#> 35      126.0    4.504683e-05          67.0       3.050471e-05       48.0
#> 36      137.5    4.895851e-05         176.0       7.496111e-05      204.5
#> 37      368.0    1.233738e-04       10430.0       5.017538e-03     2340.0
#> 38      441.0    1.461532e-04         128.0       5.608232e-05      141.0
#> 39      152.5    5.401475e-05         147.0       6.365579e-05       90.0
#> 40      392.5    1.310412e-04          89.0       3.999920e-05      142.5
#> 41      438.0    1.452207e-04         173.0       7.380418e-05      507.0
#> 42      186.0    6.514881e-05        9767.0       4.544575e-03    15945.0
#> 43     1920.5    6.137798e-04         220.5       9.183693e-05     4833.0
#> 44     1993.0    6.379229e-04       12836.5       7.002489e-03     7426.0
#> 45      371.5    1.244707e-04         229.0       9.500689e-05      535.5
#> 46     1486.0    4.720703e-04         292.0       1.180815e-04     9357.0
#> 47     3408.0    1.141523e-03         127.5       5.588098e-05      191.0
#> 48      755.0    2.430669e-04         586.0       2.202066e-04      122.0
#> 49      509.0    1.672319e-04         187.0       7.918073e-05     1289.0
#> 50      465.0    1.536050e-04         216.5       9.033976e-05    12307.0
#>    PvMSP8_Dilution PvRBP2b_MFI PvRBP2b_Dilution PvPTEX150_MFI
#> 1     5.436126e-05      1068.0     5.769583e-04         936.0
#> 2     1.953125e-05       465.5     2.524185e-04         122.0
#> 3     6.569336e-05       463.0     2.511026e-04         293.0
#> 4     1.953125e-05       591.0     3.183724e-04         109.0
#> 5     6.226189e-05       671.0     3.605631e-04        1665.5
#> 6     2.128036e-05       103.0     4.862387e-05         294.5
#> 7     2.752517e-04      1441.5     7.979408e-04         241.0
#> 8     1.953125e-05        14.0     1.953125e-05          28.0
#> 9     1.953125e-05        13.0     1.953125e-05          29.0
#> 10    6.996377e-04      1386.0     7.637756e-04        1992.0
#> 11    1.541357e-04      1575.0     8.822441e-04        8821.0
#> 12    8.966439e-05       590.0     3.178464e-04         315.0
#> 13    3.034480e-05       301.0     1.647529e-04         652.0
#> 14    2.402206e-05      1090.0     5.894351e-04         179.0
#> 15    1.133016e-04      6704.0     1.064906e-02        8814.0
#> 16    5.191597e-04      2390.0     1.474981e-03        2465.0
#> 17    2.341461e-05       244.0     1.333191e-04         579.0
#> 18    9.289961e-04      1034.0     5.577934e-04        4928.0
#> 19    7.772751e-04      2903.0     1.934960e-03        2779.0
#> 20    4.221942e-03     13703.0     2.000000e-02        5612.0
#> 21    1.953125e-05       156.0     8.225314e-05         204.0
#> 22    1.953125e-05        23.5     1.953125e-05          40.0
#> 23    1.280320e-03      1426.5     7.886574e-04         803.0
#> 24    5.304395e-03     12031.0     2.000000e-02        4830.5
#> 25    5.031588e-05       488.0     2.642528e-04         908.0
#> 26    1.953125e-05        14.0     1.953125e-05          29.0
#> 27    2.821752e-04      1041.0     5.617277e-04         299.5
#> 28    2.704492e-05       203.0     1.100311e-04         410.0
#> 29    2.005477e-05       205.0     1.111844e-04         200.0
#> 30    4.128794e-05      2032.0     1.196439e-03        1762.0
#> 31    2.371846e-05       903.0     4.851947e-04        1513.5
#> 32    1.115788e-03      4699.5     4.412649e-03         574.0
#> 33    2.262954e-04      1093.0     5.911412e-04         417.5
#> 34    4.278469e-04       899.0     4.830068e-04        1504.0
#> 35    1.953125e-05       400.0     2.178358e-04         183.0
#> 36    6.082943e-05      1090.0     5.894351e-04         504.0
#> 37    6.989804e-04      4375.0     3.830305e-03        1650.0
#> 38    4.245904e-05      1993.0     1.167928e-03        2147.0
#> 39    2.734596e-05       600.0     3.231075e-04        2178.0
#> 40    4.289769e-05       280.0     1.532732e-04         298.0
#> 41    1.461340e-04      1009.0     5.437901e-04         709.0
#> 42    9.031437e-03     10073.0     2.000000e-02        1585.5
#> 43    1.624070e-03      2340.0     1.434156e-03        5329.0
#> 44    2.831247e-03     11930.0     2.000000e-02        4826.0
#> 45    1.541357e-04     14032.0     2.000000e-02        7928.5
#> 46    3.912473e-03      8009.0     2.000000e-02        3736.0
#> 47    5.695312e-05      4401.0     3.874212e-03       11567.0
#> 48    3.687718e-05       830.0     4.455064e-04         425.5
#> 49    3.708318e-04     12930.0     2.000000e-02       13891.0
#> 50    5.906439e-03      5029.0     5.089311e-03        4157.0
#>    PvPTEX150_Dilution PvCSS_MFI PvCSS_Dilution Location.2 QC_total
#> 1        8.127366e-04     223.0   2.842313e-04         B1     pass
#> 2        1.228915e-04      93.0   1.353894e-04         B2     pass
#> 3        2.780292e-04     868.0   8.856384e-04         B3     pass
#> 4        1.102629e-04     110.0   1.567512e-04         B4     pass
#> 5        1.423983e-03     266.0   3.287835e-04         B5     pass
#> 6        2.793327e-04      92.0   1.341049e-04         B6     pass
#> 7        2.323812e-04     282.0   3.450058e-04         B7     pass
#> 8        2.580000e-05      17.0   2.165720e-05         B8     pass
#> 9        2.692247e-05      17.0   2.165720e-05         B9     pass
#> 10       1.708162e-03    2927.0   2.805547e-03        B10     pass
#> 11       1.036268e-02     998.0   1.000735e-03        B11     pass
#> 12       2.970847e-04     366.0   4.277819e-04        B12     pass
#> 13       5.793065e-04     130.0   1.809051e-04         C1     pass
#> 14       1.764604e-04      92.0   1.341049e-04         C2     pass
#> 15       1.035037e-02    5627.0   6.007799e-03         C3     pass
#> 16       2.134759e-03     411.0   4.708489e-04         C4     pass
#> 17       5.190671e-04     229.0   2.905415e-04         C5     pass
#> 18       4.709240e-03    7730.0   9.300803e-03         C6     pass
#> 19       2.428679e-03    7300.0   8.558776e-03         C7     pass
#> 20       5.546784e-03    6383.0   7.098154e-03         C8     pass
#> 21       1.992456e-04      49.0   7.478784e-05         C9     pass
#> 22       3.913298e-05      27.0   3.958834e-05        C10     pass
#> 23       7.034371e-04    1670.0   1.600615e-03        C11     pass
#> 24       4.594572e-03    4217.0   4.214130e-03        C12     pass
#> 25       7.897074e-04      87.0   1.276305e-04         D1     pass
#> 26       2.692247e-05      17.0   2.165720e-05         D2     pass
#> 27       2.836733e-04     149.0   2.030644e-04         D3     pass
#> 28       3.780943e-04     174.5   2.318380e-04         D4     pass
#> 29       1.956235e-04     183.0   2.412206e-04         D5     pass
#> 30       1.507167e-03    1241.0   1.215895e-03         D6     pass
#> 31       1.294233e-03     325.0   3.878274e-04         D7     pass
#> 32       5.149313e-04    5389.0   5.684104e-03         D8     pass
#> 33       3.844197e-04     262.0   3.247003e-04         D9     pass
#> 34       1.286172e-03     260.0   3.226543e-04        D10     pass
#> 35       1.801306e-04      61.0   9.230630e-05        D11     pass
#> 36       4.568470e-04     459.0   5.160586e-04        D12     pass
#> 37       1.410682e-03    7801.5   9.427946e-03         E1     pass
#> 38       1.845918e-03    1502.0   1.448877e-03         E2     pass
#> 39       1.873702e-03     170.0   2.268311e-04         E3     pass
#> 40       2.823719e-04     138.0   1.903195e-04         E4     pass
#> 41       6.262085e-04    2040.0   1.941343e-03         E5     pass
#> 42       1.355505e-03    1124.0   1.112196e-03         E6     pass
#> 43       5.193108e-03    2369.0   2.253368e-03         E7     pass
#> 44       4.589307e-03    3468.5   3.372733e-03         E8     pass
#> 45       8.861526e-03    1414.5   1.370446e-03         E9     pass
#> 46       3.383032e-03   14370.5   2.000000e-02        E10     pass
#> 47       1.593411e-02    2923.5   2.801986e-03        E11     pass
#> 48       3.911576e-04     121.0   1.701521e-04        E12     pass
#> 49       2.000000e-02    9531.0   1.286392e-02         F1     pass
#> 50       3.833058e-03     760.0   7.896119e-04         F2     pass
#>  [ reached 'max' / getOption("max.print") -- omitted 202 rows ]
#> 
#> [[2]]
#> NULL
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
#> [[6]]$`1`

#> 
#> [[6]]$`2`

#> 
#> [[6]]$`3`

#> 
#> 
# }
```
