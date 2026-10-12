<!-- badges: start -->
[![DOI](https://zenodo.org/badge/1016494729.svg)](https://doi.org/10.5281/zenodo.23095199)
[![CRAN status](https://www.r-pkg.org/badges/version/SeroTrackR)](https://CRAN.R-project.org/package=SeroTrackR)
[![R-CMD-check-CRAN](https://github.com/dionnecargy/SeroTrackR/actions/workflows/R-CMD-check-CRAN.yaml/badge.svg)](https://github.com/dionnecargy/SeroTrackR/actions/workflows/R-CMD-check-CRAN.yaml)
[![CRAN downloads per month](https://cranlogs.r-pkg.org/badges/SeroTrackR)](https://CRAN.R-project.org/package=SeroTrackR)
[![CRAN downloads total](https://cranlogs.r-pkg.org/badges/grand-total/SeroTrackR)](https://CRAN.R-project.org/package=SeroTrackR)
<!-- badges: end -->

# <img src="man/figures/SeroTrackR_sticker.png" width="25%" height="25%" align="left"/> SeroTrackR

The `{SeroTrackR}` R package compliments the PvSeroApp [see Github here](https://github.com/dionnecargy/PvSeroApp) for experienced users who are comfortable with implementing the code in their own R sessions. 

This package contains helper functions relevant for *Plasmodium* spp. projects for in-house and collaborator needs. 

<p>&nbsp;</p>

## How to install the package:

```{r}
install.packages("SeroTrackR")
library(SeroTrackR)
```

Alternatively, you can download the developmental version on GitHub. 

```{r}
pak::pak("dionnecargy/SeroTrackR") # To download the package
library(SeroTrackR) # To load the package 
```

The GitHub (master) version also bundles the trained classification model file locally, whereas the CRAN version downloads it from this repository at runtime — so the CRAN version requires an internet connection the first time functions such as `classifyResults()` are called.

### Dependencies

Package dependencies are installed automatically by `install.packages()` or `pak::pak()`. If you are missing system libraries needed to build a dependency from source (e.g. `curl`, `openssl`, `xml2` on Linux), `pak::pak()` will usually install the required system requirements for you; otherwise install the appropriate system package for your OS first.

## How can I use the package?

Please see the [tutorial](https://dionnecargy.github.io/SeroTrackR/) or see the vignettes inside the package by running the following commands: 

```{r}
browseVignettes("SeroTrackR") 

# To run a specific vignette: 
vignette("PvSeroApp_R_Tutorial")
vignette("Pk_Pv_Pf_Serology_Tutorial")
```

## For Maintainers

### Branch structure

- **`master`** is the development branch. All changes (code, documentation, large data files in `inst/extdata/`) are made here. Large files such as `PvSeroTaTmodel.rds` (~24 MB) exceed CRAN's 5 MB package size limit and live only on master.
- **`CRAN`** is the submission branch. It is kept in sync with master automatically by the `Sync to CRAN Branch` GitHub Action on every push to master. On this branch, oversized files and pkgdown-related files (`docs/`, `inst/hex/`, `inst/tutorials/`, `_pkgdown.yml`) are removed, and the package code downloads the missing data at runtime from `https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/`.

### Maintainer checklist

- **Do not commit directly to `CRAN`** unless resolving a merge conflict; all changes should flow from master.
- **When adding a new large data file** (approaching CRAN's 5 MB limit) to `inst/extdata/`:
  1. Add its removal to the "Remove pkgdown-related files and oversized data" step of [`.github/workflows/sync-to-cran.yml`](.github/workflows/sync-to-cran.yml) and a matching line to the `.Rbuildignore` step.
  2. Write the loading code to fall back to the raw GitHub URL when the local file is absent (see `classifyResults()` for the pattern).
- **After pushing to master**, check the Actions tab to confirm both workflows succeeded:
  - `Sync to CRAN Branch` merges master into CRAN and re-applies the deletions. If it fails on merge conflicts, resolve manually: `git checkout CRAN && git merge master`, fix conflicts, push.
  - `R-CMD-check-CRAN` then runs `R CMD check --as-cran` on the CRAN branch; its badge above shows the latest result.
- **Before a CRAN submission**, confirm the CRAN branch version number matches master, that the `R-CMD-check-CRAN` badge is passing with no warnings, and that the runtime-fetched URLs still resolve.
