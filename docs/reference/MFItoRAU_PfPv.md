# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion for either PNG or ETH standard

This function fits a 5-parameter logistic standard curve to the
dilutions of the positive controls for each protein and converts the MFI
values into relative antibody units (RAU). This function is utilised in
the master function \`MFItoRAU_Plasmo()\`.

## Usage

``` r
MFItoRAU_PfPv(
  processed_PfPv,
  plate_list,
  std_point,
  location,
  counts_QC_output
)
```

## Arguments

- processed_PfPv:

  df\$PfPv of output \`processPkPfPv()\`

- plate_list:

  Output of \`readPlateLayout()\`

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is
  an integer.

- location:

  "PNG" or "ETH" to filter WEHI standard curve data (reactive).

- counts_QC_output:

  Output from \`getCountsQC()\`

## Value

A list of three data frames: 1. Data frame with MFI data, converted RAU
data and matched SampleID's. 2. Plot information for \`plotModel\`
function. 3. Data frame of RAU data for random forest classification
use.

## Author

Dionne Argyropoulos, Connie Li Wai Suen, Eamon Conway
