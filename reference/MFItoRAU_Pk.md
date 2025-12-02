# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion for Pk proteins

This function is utilised in the master function \`MFItoRAU_Plasmo()\`.

## Usage

``` r
MFItoRAU_Pk(processed_Pk, plate_list, std_point, counts_QC_output)
```

## Arguments

- processed_Pk:

  df\$Pk of output \`processPkPfPv()\`

- plate_list:

  Output of \`readPlateLayout()\`

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve. Value is
  an integer.

- counts_QC_output:

  Output from \`getCountsQC()\`

## Value

Data frame with MFI data, converted RAU data and matched SampleID's.

## Author

Dionne Argyropoulos, Caitlin Bourke
