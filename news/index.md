# Changelog

## SeroTrackR 1.1.0

## SeroTrackR 1.0.1

- Updated
  [`readSeroData()`](https://dionnecargy.github.io/SeroTrackR/reference/readSeroData.md)
  error messaging for
  [`.check_platform()`](https://dionnecargy.github.io/SeroTrackR/reference/dot-check_platform.md)
- Removed algorithm without PvMSP1-19 as it is no longer necessary
- Kept only balanced and 90% specificity threshold options for algorithm

## SeroTrackR 1.0.0

CRAN release: 2026-03-26

- [`runQC()`](https://dionnecargy.github.io/SeroTrackR/reference/runQC.md)
  aims to streamline all the quality control steps into one function
- “maximised” threshold in the Pv classification algorithm has been
  relabeled as “balanced”
- Incorporation of standard curve type: 5-point or 10-point for any MFI
  to RAU conversion
- Capability to input your own Pk/Pf protein panel (relating to Pk/Pf/Pv
  work)

## SeroTrackR 0.5.1

- Updated
  [`readSeroData()`](https://dionnecargy.github.io/SeroTrackR/reference/readSeroData.md)
  to include `version` parameter accounting for version 4.2 and 4.3 of
  the xPONENT software in MAGPIX Luminex machines.

## SeroTrackR 0.5.0

- Initial CRAN submission.
- First publication of pkgdown.
- Created hex sticker.
- Created
  [`getPlateLayout()`](https://dionnecargy.github.io/SeroTrackR/reference/getPlateLayout.md)
  function.
- Updated `renderQCreport()` to save path specified by user.
- Updated package dependencies.
- Added PvLDH analysis pipeline.
- Clearer MFI to RAU converion for pk/pf/pv analysis pipeline.

## SeroTrackR 0.4.0

- Renamed package to `SeroTrackR`.
- Rendered first tutorials to GitHub Pages.
- Added targets visualisations.
- Improved pk/pf/pv analysis pipeline.

## SeroTrackR 0.3.0

- Improved bioplex import functionality.
- Used
  “[`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)” or
  “[`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html)” in
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

## SeroTrackR 0.2.0

- Added basic functions from `PvSeroApp`.

## SeroTrackR 0.1.0

- Initial commit of R package `pvsero`.
