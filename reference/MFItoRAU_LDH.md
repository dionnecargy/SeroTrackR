# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion for LDH

This function fits a 5-parameter logistic standard curve to the
dilutions of the positive controls for each protein and converts the MFI
values into relative antibody units (RAU).

## Usage

``` r
MFItoRAU_LDH(sero_data, plate_list, std_point = "PvLDH", file_path = NULL)
```

## Arguments

- sero_data:

  Output from \`readSeroData()\` or \`readSeroData()\`.

- plate_list:

  Output from \`readPlateLayout()\`.

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = "PvLDH".

- file_path:

  A file path to write the .csv final file. Default: Current working
  directory.

## Value

A data frame containing the MFI and RAU Dilution values for each sample

## Author

Connie Li Wai Suen, Caitlin Bourke, Dionne Argyropoulos

## Examples

``` r
# \donttest{
# Example demonstrating multi-plate processing workflow.
# These files are included in the SeroTrackR package under inst/extdata.

your_raw_data <- system.file("extdata", "example_BioPlex_PvLDH_plate1.xlsx", package = "SeroTrackR")

your_plate_layout <- system.file(
  "extdata", "example_platelayout_1.xlsx",
  package = "SeroTrackR"
)

# Read in raw BioPlex data
sero_data <- readSeroData(
  raw_data = your_raw_data,
  platform = "bioplex"
)
#> PASS: File example_bioplex_pvldh_plate1.xlsx successfully validated.

# Read matching plate layout
plate_list <- readPlateLayout(
  plate_layout = your_plate_layout,
  sero_data = sero_data
)
#> Plate layouts correctly identified!

# Run MFI to RAU conversion
mfi_outputs <- MFItoRAU_LDH(
  sero_data = sero_data,
  plate_list = plate_list
)

# View All Outputs
mfi_outputs
#>    SampleID Location.2  Location Sample  Plate PvLDH-ST_MFI PvLDH-ST_Dilution
#> 1    ABC013         B1  13(1,B1)    X13 plate1         54.5         331.54945
#> 2    ABC014         B2  14(1,B2)    X14 plate1         28.5          57.55777
#> 3    ABC015         B3  15(1,B3)    X15 plate1         33.5          99.51571
#> 4    ABC016         B4  16(1,B4)    X16 plate1         32.0          86.11185
#> 5    ABC017         B5  17(1,B5)    X17 plate1         33.0          94.97735
#> 6    ABC018         B6  18(1,B6)    X18 plate1         30.0          69.29260
#> 7    ABC019         B7  19(1,B7)    X19 plate1      15646.5      168548.31301
#> 8    ABC020         B8  20(1,B8)    X20 plate1         40.0         163.93929
#> 9    ABC021         B9  21(1,B9)    X21 plate1         46.5         236.01254
#> 10   ABC022        B10 22(1,B10)    X22 plate1         32.0          86.11185
#> 11   ABC023        B11 23(1,B11)    X23 plate1         33.5          99.51571
#> 12   ABC024        B12 24(1,B12)    X24 plate1        114.0        1110.29378
#> 13   ABC025         C1  25(1,C1)    X25 plate1         29.0          61.38007
#> 14   ABC026         C2  26(1,C2)    X26 plate1         48.5         259.30891
#> 15   ABC027         C3  27(1,C3)    X27 plate1         41.0         174.60587
#> 16   ABC028         C4  28(1,C4)    X28 plate1         52.0         301.06675
#> 17   ABC029         C5  29(1,C5)    X29 plate1         29.0          61.38007
#> 18   ABC030         C6  30(1,C6)    X30 plate1         28.5          57.55777
#> 19   ABC031         C7  31(1,C7)    X31 plate1         38.0         143.15306
#> 20   ABC032         C8  32(1,C8)    X32 plate1         27.5          50.19597
#> 21   ABC033         C9  33(1,C9)    X33 plate1         40.0         163.93929
#> 22   ABC034        C10 34(1,C10)    X34 plate1         23.0          22.38161
#> 23   ABC035        C11 35(1,C11)    X35 plate1         32.0          86.11185
#> 24   ABC036        C12 36(1,C12)    X36 plate1         26.0          39.90749
#> 25   ABC037         D1  37(1,D1)    X37 plate1         32.5          90.50864
#> 26   ABC038         D2  38(1,D2)    X38 plate1         34.0         104.12149
#> 27   ABC039         D3  39(1,D3)    X39 plate1         25.5          36.69232
#> 28   ABC040         D4  40(1,D4)    X40 plate1         35.0         113.52686
#> 29   ABC041         D5  41(1,D5)    X41 plate1         73.5         574.30235
#> 30   ABC042         D6  42(1,D6)    X42 plate1         22.0          17.54900
#> 31   ABC043         D7  43(1,D7)    X43 plate1         28.5          57.55777
#> 32   ABC044         D8  44(1,D8)    X44 plate1         35.5         118.32238
#> 33   ABC045         D9  45(1,D9)    X45 plate1         24.0          27.73842
#> 34   ABC046        D10 46(1,D10)    X46 plate1         98.0         898.50898
#> 35   ABC047        D11 47(1,D11)    X47 plate1         26.0          39.90749
#> 36   ABC048        D12 48(1,D12)    X48 plate1         33.0          94.97735
#> 37   ABC049         E1  49(1,E1)    X49 plate1         25.5          36.69232
#> 38   ABC050         E2  50(1,E2)    X50 plate1         32.0          86.11185
#> 39   ABC051         E3  51(1,E3)    X51 plate1         81.5         679.64565
#> 40   ABC052         E4  52(1,E4)    X52 plate1         29.5          65.29263
#> 41   ABC053         E5  53(1,E5)    X53 plate1         25.0          33.59004
#> 42   ABC054         E6  54(1,E6)    X54 plate1         42.0         185.44010
#> 43   ABC055         E7  55(1,E7)    X55 plate1         46.5         236.01254
#> 44   ABC056         E8  56(1,E8)    X56 plate1         29.0          61.38007
#> 45   ABC057         E9  57(1,E9)    X57 plate1         24.0          27.73842
#> 46   ABC058        E10 58(1,E10)    X58 plate1         37.0         133.05732
#> 47   ABC059        E11 59(1,E11)    X59 plate1         37.5         138.07909
#> 48   ABC060        E12 60(1,E12)    X60 plate1         32.5          90.50864
#> 49   ABC061         F1  61(1,F1)    X61 plate1         25.5          36.69232
#> 50   ABC062         F2  62(1,F2)    X62 plate1         33.5          99.51571
#> 51   ABC063         F3  63(1,F3)    X63 plate1         31.0          77.54362
#> 52   ABC064         F4  64(1,F4)    X64 plate1         36.5         128.08945
#> 53   ABC065         F5  65(1,F5)    X65 plate1         22.0          17.54900
#> 54   ABC066         F6  66(1,F6)    X66 plate1         39.0         153.45117
#> 55   ABC067         F7  67(1,F7)    X67 plate1         37.5         138.07909
#> 56   ABC068         F8  68(1,F8)    X68 plate1         36.0         123.17720
#> 57   ABC069         F9  69(1,F9)    X69 plate1         22.0          17.54900
#> 58   ABC070        F10 70(1,F10)    X70 plate1         30.0          69.29260
#> 59   ABC071        F11 71(1,F11)    X71 plate1         32.0          86.11185
#> 60   ABC072        F12 72(1,F12)    X72 plate1         26.0          39.90749
#> 61   ABC073         G1  73(1,G1)    X73 plate1         25.5          36.69232
#> 62   ABC074         G2  74(1,G2)    X74 plate1         28.5          57.55777
#> 63   ABC075         G3  75(1,G3)    X75 plate1         31.0          77.54362
#> 64   ABC076         G4  76(1,G4)    X76 plate1         30.0          69.29260
#> 65   ABC077         G5  77(1,G5)    X77 plate1         27.0          46.66268
#> 66   ABC078         G6  78(1,G6)    X78 plate1         33.0          94.97735
#> 67   ABC079         G7  79(1,G7)    X79 plate1         27.5          50.19597
#> 68   ABC080         G8  80(1,G8)    X80 plate1         36.0         123.17720
#> 69   ABC081         G9  81(1,G9)    X81 plate1         22.0          17.54900
#> 70   ABC082        G10 82(1,G10)    X82 plate1         30.0          69.29260
#> 71   ABC083        G11 83(1,G11)    X83 plate1         24.5          30.60421
#> 72   ABC084        G12 84(1,G12)    X84 plate1         31.0          77.54362
#> 73   ABC085         H1  85(1,H1)    X85 plate1         26.5          43.23208
#> 74   ABC086         H2  86(1,H2)    X86 plate1         32.0          86.11185
#> 75   ABC087         H3  87(1,H3)    X87 plate1         26.0          39.90749
#> 76   ABC088         H4  88(1,H4)    X88 plate1         35.0         113.52686
#> 77   ABC089         H5  89(1,H5)    X89 plate1         24.0          27.73842
#> 78   ABC090         H6  90(1,H6)    X90 plate1         64.0         451.05003
#> 79   ABC091         H7  91(1,H7)    X91 plate1         33.5          99.51571
#> 80   ABC092         H8  92(1,H8)    X92 plate1         31.5          81.78936
#> 81   ABC093         H9  93(1,H9)    X93 plate1         27.0          46.66268
#> 82   ABC094        H10 94(1,H10)    X94 plate1         24.0          27.73842
#> 83   ABC095        H11 95(1,H11)    X95 plate1         34.5         108.79257
#> 84   ABC096        H12 96(1,H12)    X96 plate1         27.5          50.19597
#>    PvLDH-ST_DilutionReciprocal PvLDH-ST_MinStd PvLDH-ST_MaxDilution
#> 1                 3.016141e-03              30                50.81
#> 2                 1.737385e-02              30                50.81
#> 3                 1.004866e-02              30                50.81
#> 4                 1.161280e-02              30                50.81
#> 5                 1.052883e-02              30                50.81
#> 6                 1.443156e-02              30                50.81
#> 7                 5.933017e-06              30                50.81
#> 8                 6.099819e-03              30                50.81
#> 9                 4.237063e-03              30                50.81
#> 10                1.161280e-02              30                50.81
#> 11                1.004866e-02              30                50.81
#> 12                9.006625e-04              30                50.81
#> 13                1.629193e-02              30                50.81
#> 14                3.856404e-03              30                50.81
#> 15                5.727184e-03              30                50.81
#> 16                3.321523e-03              30                50.81
#> 17                1.629193e-02              30                50.81
#> 18                1.737385e-02              30                50.81
#> 19                6.985530e-03              30                50.81
#> 20                1.992192e-02              30                50.81
#> 21                6.099819e-03              30                50.81
#> 22                4.467954e-02              30                50.81
#> 23                1.161280e-02              30                50.81
#> 24                2.505795e-02              30                50.81
#> 25                1.104867e-02              30                50.81
#> 26                9.604165e-03              30                50.81
#> 27                2.725366e-02              30                50.81
#> 28                8.808488e-03              30                50.81
#> 29                1.741243e-03              30                50.81
#> 30                5.698330e-02              30                50.81
#> 31                1.737385e-02              30                50.81
#> 32                8.451486e-03              30                50.81
#> 33                3.605108e-02              30                50.81
#> 34                1.112955e-03              30                50.81
#> 35                2.505795e-02              30                50.81
#> 36                1.052883e-02              30                50.81
#> 37                2.725366e-02              30                50.81
#> 38                1.161280e-02              30                50.81
#> 39                1.471355e-03              30                50.81
#> 40                1.531566e-02              30                50.81
#> 41                2.977073e-02              30                50.81
#> 42                5.392577e-03              30                50.81
#> 43                4.237063e-03              30                50.81
#> 44                1.629193e-02              30                50.81
#> 45                3.605108e-02              30                50.81
#> 46                7.515558e-03              30                50.81
#> 47                7.242226e-03              30                50.81
#> 48                1.104867e-02              30                50.81
#> 49                2.725366e-02              30                50.81
#> 50                1.004866e-02              30                50.81
#> 51                1.289597e-02              30                50.81
#> 52                7.807044e-03              30                50.81
#> 53                5.698330e-02              30                50.81
#> 54                6.516731e-03              30                50.81
#> 55                7.242226e-03              30                50.81
#> 56                8.118386e-03              30                50.81
#> 57                5.698330e-02              30                50.81
#> 58                1.443156e-02              30                50.81
#> 59                1.161280e-02              30                50.81
#> 60                2.505795e-02              30                50.81
#> 61                2.725366e-02              30                50.81
#> 62                1.737385e-02              30                50.81
#> 63                1.289597e-02              30                50.81
#> 64                1.443156e-02              30                50.81
#> 65                2.143040e-02              30                50.81
#> 66                1.052883e-02              30                50.81
#> 67                1.992192e-02              30                50.81
#> 68                8.118386e-03              30                50.81
#> 69                5.698330e-02              30                50.81
#> 70                1.443156e-02              30                50.81
#> 71                3.267525e-02              30                50.81
#> 72                1.289597e-02              30                50.81
#> 73                2.313097e-02              30                50.81
#> 74                1.161280e-02              30                50.81
#> 75                2.505795e-02              30                50.81
#> 76                8.808488e-03              30                50.81
#> 77                3.605108e-02              30                50.81
#> 78                2.217049e-03              30                50.81
#> 79                1.004866e-02              30                50.81
#> 80                1.222653e-02              30                50.81
#> 81                2.143040e-02              30                50.81
#> 82                3.605108e-02              30                50.81
#> 83                9.191804e-03              30                50.81
#> 84                1.992192e-02              30                50.81
#>    PvLDH-ST_MaxStd PvLDH-ST_MinDilution
#> 1            23757                1e+06
#> 2            23757                1e+06
#> 3            23757                1e+06
#> 4            23757                1e+06
#> 5            23757                1e+06
#> 6            23757                1e+06
#> 7            23757                1e+06
#> 8            23757                1e+06
#> 9            23757                1e+06
#> 10           23757                1e+06
#> 11           23757                1e+06
#> 12           23757                1e+06
#> 13           23757                1e+06
#> 14           23757                1e+06
#> 15           23757                1e+06
#> 16           23757                1e+06
#> 17           23757                1e+06
#> 18           23757                1e+06
#> 19           23757                1e+06
#> 20           23757                1e+06
#> 21           23757                1e+06
#> 22           23757                1e+06
#> 23           23757                1e+06
#> 24           23757                1e+06
#> 25           23757                1e+06
#> 26           23757                1e+06
#> 27           23757                1e+06
#> 28           23757                1e+06
#> 29           23757                1e+06
#> 30           23757                1e+06
#> 31           23757                1e+06
#> 32           23757                1e+06
#> 33           23757                1e+06
#> 34           23757                1e+06
#> 35           23757                1e+06
#> 36           23757                1e+06
#> 37           23757                1e+06
#> 38           23757                1e+06
#> 39           23757                1e+06
#> 40           23757                1e+06
#> 41           23757                1e+06
#> 42           23757                1e+06
#> 43           23757                1e+06
#> 44           23757                1e+06
#> 45           23757                1e+06
#> 46           23757                1e+06
#> 47           23757                1e+06
#> 48           23757                1e+06
#> 49           23757                1e+06
#> 50           23757                1e+06
#> 51           23757                1e+06
#> 52           23757                1e+06
#> 53           23757                1e+06
#> 54           23757                1e+06
#> 55           23757                1e+06
#> 56           23757                1e+06
#> 57           23757                1e+06
#> 58           23757                1e+06
#> 59           23757                1e+06
#> 60           23757                1e+06
#> 61           23757                1e+06
#> 62           23757                1e+06
#> 63           23757                1e+06
#> 64           23757                1e+06
#> 65           23757                1e+06
#> 66           23757                1e+06
#> 67           23757                1e+06
#> 68           23757                1e+06
#> 69           23757                1e+06
#> 70           23757                1e+06
#> 71           23757                1e+06
#> 72           23757                1e+06
#> 73           23757                1e+06
#> 74           23757                1e+06
#> 75           23757                1e+06
#> 76           23757                1e+06
#> 77           23757                1e+06
#> 78           23757                1e+06
#> 79           23757                1e+06
#> 80           23757                1e+06
#> 81           23757                1e+06
#> 82           23757                1e+06
#> 83           23757                1e+06
#> 84           23757                1e+06
# }
```
