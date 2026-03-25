# Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion for Pk proteins

This function is utilised in the master function \`MFItoRAU_Plasmo()\`.

## Usage

``` r
MFItoRAU_Pk(processed_Pk, plate_list, std_point, qc_results)
```

## Arguments

- processed_Pk:

  df\$Pk of output \`processPkPfPv()\`

- plate_list:

  Output of \`readPlateLayout()\`

- std_point:

  Standard Point Curve: 5 = 5-point curve, 10 = 10-point curve, "PvLDH"
  for LDH specific curve. Default = 10. Value is an integer.

- qc_results:

  Output from \`runQC()\`.

## Value

Data frame with MFI data, converted RAU data and matched SampleID's.

## Author

Dionne Argyropoulos, Caitlin Bourke
