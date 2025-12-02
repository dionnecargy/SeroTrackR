# Random Forest Classification for Pv only in Pk/Pf/Pv analysis

This function classifies unknown samples as recently exposed or not
(Note: MFItoRAU() or MFItoRAU_ETH() needs to be run first to convert to
RAU). This is a slightly modified function for ONLY use in Pk/Pf/Pv
analysis. The only difference is that ETHtoPNGloglog_Dilution is used to
identify the correct columns to then classify.

## Usage

``` r
classifyPv(mfi_to_rau_output, algorithm_type, sens_spec, counts_QC_output)
```

## Arguments

- mfi_to_rau_output:

  Output from \`MFItoRAU()\` or \`MFItoRAU_ETH()\` (reactive).

- algorithm_type:

  User-selected algorithm choice: - "antibody_model" (PvSeroTaT model;
  default), or - "antibody_model_excLF016" (PvSeroTaT excluding LF016).

- sens_spec:

  User-selected Sensitivity/Specificity threshold: - "maximised"
  (default), - "85% sensitivity", - "90% sensitivity", - "95%
  sensitivity", - "85% specificity", - "90% specificity". - "95%
  specificity".

- counts_QC_output:

  Output from \`getCountsQC()\` (reactive).

## Value

\- Data frame with exposure status for every sample. - Summary table
with positive/negative results for each threshold.

## Author

Lauren Smith, Dionne Argyropoulos

## Examples

``` r
# \donttest{

# Step 0: Load example raw data
raw_files_5std <- c(
  system.file("extdata", "example_MAGPIX_pk_5std_plate1.csv", package = "SeroTrackR"),
  system.file("extdata", "example_MAGPIX_pk_5std_plate2.csv", package = "SeroTrackR")
)
plate_layout_5std <- system.file(
  "extdata",
  "example_platelayout_pk_5std.xlsx",
  package = "SeroTrackR"
)

# Step 1: Read in serology data and plate layout
sero_data  <- readSeroData(raw_files_5std, "magpix")
#> PASS: File example_magpix_pk_5std_plate1.csv successfully validated.
#> PASS: File example_magpix_pk_5std_plate2.csv successfully validated.
plate_list <- readPlateLayout(plate_layout_5std, sero_data)
#> Plate layouts correctly identified!

# Step 2: Process counts and QC
counts      <- processCounts(sero_data)
counts_raw  <- getCounts(counts)
sample_ids  <- getSampleID(counts, plate_list)
antigen_cts <- getAntigenCounts(counts, plate_list)
counts_qc   <- getCountsQC(antigen_cts, counts_raw)

# Step 3: Convert MFI to RAU using 5-point standard curve
mfi_results <- MFItoRAU_Plasmo(
  sero_data = sero_data,
  plate_list         = plate_list,
  panel              = "panel1",
  std_point          = 5,
  counts_QC_output   = counts_qc
)
#> Joining with `by = join_by(SampleID, Location, Location.2, Sample, Plate,
#> QC_total, LF005_MFI, LF010_MFI, LF016_MFI, EBP_MFI, RBP2b.P87_MFI, PvCSS_MFI,
#> PTEX150_MFI, MSP8_MFI)`

# Step 4: Classify Pv samples
pv_classified <- classifyPv(
  mfi_to_rau_output= mfi_results,
  algorithm_type   = "antibody_model",
  sens_spec        = "maximised",
  counts_QC_output = counts_qc
)
# }
```
