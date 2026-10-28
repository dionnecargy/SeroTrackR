<!-- badges: start -->
[![Preprint DOI](https://img.shields.io/badge/preprint-10.12688%2Fverixiv.3797.2-blue)](https://doi.org/10.12688/verixiv.3797.2)
[![CRAN status](https://www.r-pkg.org/badges/version/SeroTrackR)](https://CRAN.R-project.org/package=SeroTrackR)
[![R-CMD-check-CRAN](https://github.com/dionnecargy/SeroTrackR/actions/workflows/R-CMD-check-CRAN.yaml/badge.svg)](https://github.com/dionnecargy/SeroTrackR/actions/workflows/R-CMD-check-CRAN.yaml)
[![CRAN downloads per month](https://cranlogs.r-pkg.org/badges/SeroTrackR)](https://CRAN.R-project.org/package=SeroTrackR)
[![CRAN downloads total](https://cranlogs.r-pkg.org/badges/grand-total/SeroTrackR)](https://CRAN.R-project.org/package=SeroTrackR)
[![Code archive (Zenodo)](https://img.shields.io/badge/code_archive-Zenodo-lightgrey)](https://doi.org/10.5281/zenodo.23095199)
<!-- badges: end -->

# <img src="man/figures/SeroTrackR_sticker.png" width="25%" height="25%" align="left"/> SeroTrackR

The `{SeroTrackR}` R package compliments the PvSeroApp [see Github here](https://github.com/dionnecargy/PvSeroApp) for experienced users who are comfortable with implementing the code in their own R sessions. 

This package contains helper functions relevant for *Plasmodium* spp. projects for in-house and collaborator needs. 

<p>&nbsp;</p>

## How to install the package:

```{r}
# Download the package available on the Comprehensive R Archive Network (CRAN)
install.packages("SeroTrackR") 
# Load the package in your R session 
library(SeroTrackR)
```

Alternatively, you can download the developmental version on GitHub. 

```{r}
pak::pak("dionnecargy/SeroTrackR") 
library(SeroTrackR) # To load the package 
```

The GitHub (master) version also bundles the trained classification model file locally, whereas the CRAN version downloads it from this repository at runtime, so the CRAN version requires an internet connection the first time functions such as `classifyResults()` are called.

### Dependencies

Package dependencies are installed automatically by `install.packages()` or `pak::pak()`. If you are missing system libraries needed to build a dependency from source (e.g. `curl`, `openssl`, `xml2` on Linux), `pak::pak()` will usually install the required system requirements for you; otherwise install the appropriate system package for your OS first.

## Documentation

Two sites serve different purposes:

- **[ebook](https://vispa-malaria.github.io/sero-analysis/)**: the comprehensive *guide*: end-to-end workflows for the fit-for-purpose tool described in [Argyropoulos et al. 2026 VeriXiv](https://doi.org/10.12688/verixiv.3797.2) and for pathogen-agnostic uses. Start here if you're learning the package.
- **[pkgdown site](https://dionnecargy.github.io/SeroTrackR/)**: the *reference*: function documentation, arguments, and the vignettes as articles. Use this for quick lookups.

The same vignettes are also available offline inside R: 

```{r}
browseVignettes("SeroTrackR") 

# To run a specific vignette: 
vignette("PvSeroApp_R_Tutorial")
vignette("Pk_Pv_Pf_Serology_Tutorial")
```

## Citing SeroTrackR

If you use `SeroTrackR` in your work, please cite **both** the pre-print describing the method and the package itself:

> Argyropoulos D, Conway E, Bourke C, Li-Wai-Suen C, Ruybal-Pesántez S, Smith L, Mueller I, Longley R. *PvSeroApp and SeroTrackR: An automated pipeline for Plasmodium vivax malaria serological analyses and machine learning classification of recent exposure.* VeriXiv 2026; 3:313. <https://doi.org/10.12688/verixiv.3797.2>
> `SeroTrackR` R package (CRAN): <https://doi.org/10.32614/CRAN.package.SeroTrackR>

Run `citation("SeroTrackR")` in R to get both entries in BibTeX format.

The source code of each release is additionally archived on [Zenodo](https://doi.org/10.5281/zenodo.23095199) for long-term preservation; the preferred citations are the two above.

If you are using the `{PvSeroApp}` in your work, please cite: 

> Smith L, Argyropoulos DC, Bareng APN, Lin J, Kiernan-Walker N, Lamont M, Abraham A, Lim PS, Wu KW, William T, Anstey NM, Grigg MJ, Sattabongkot J, Lacerda M, Vahi V, Mazhari R, Mueller I, Longley RJ. *An optimised serological machine learning model enabling targeted test-and-treat for Plasmodium vivax malaria.* medRxiv 2026. <https://doi.org/10.64898/2026.08.09.26355326>

## For Maintainers

### Branch structure

- **`master`** is the development branch. All changes (code, documentation, large data files in `inst/extdata/`) are made here. Large files such as `PvSeroTaTmodel.rds` (~24 MB) exceed CRAN's 5 MB package size limit and live only on master.
- **`CRAN`** is the submission branch. It is kept in sync with master automatically by the `Sync to CRAN Branch` GitHub Action on every push to master. On this branch, oversized files and pkgdown-related files (`docs/`, `inst/hex/`, `inst/tutorials/`, `_pkgdown.yml`) are removed, and the package code downloads the missing data at runtime from `https://raw.githubusercontent.com/dionnecargy/SeroTrackR/master/inst/extdata/`. Note that the author needs to consciously upload the R package to CRAN to make changes to the version available at the [SeroTrackR CRAN DOI](https://doi.org/10.32614/CRAN.package.SeroTrackR). 

### Maintainer checklist

- [ ] **Do not commit directly to `CRAN`** unless resolving a merge conflict; all changes should flow from master.
- [ ] **When adding a new large data file** (approaching CRAN's 5 MB limit) to `inst/extdata/`:
  1. Add its removal to the "Remove pkgdown-related files and oversized data" step of [`.github/workflows/sync-to-cran.yml`](.github/workflows/sync-to-cran.yml) and a matching line to the `.Rbuildignore` step.
  2. Write the loading code to fall back to the raw GitHub URL when the local file is absent (see `classifyResults()` for the pattern).
- [ ] **After pushing to master**, check the Actions tab to confirm both workflows succeeded:
  - `Sync to CRAN Branch` merges master into CRAN and re-applies the deletions. If it fails on merge conflicts, resolve manually: `git checkout CRAN && git merge master`, fix conflicts, push.
  - `R-CMD-check-CRAN` then runs `R CMD check --as-cran` on the CRAN branch; its badge above shows the latest result.
- [ ] **Before a CRAN submission**, confirm the CRAN branch version number matches master, that the `R-CMD-check-CRAN` badge is passing with no warnings, and that the runtime-fetched URLs still resolve.

<!-- add pathogen-agnostic use-case vignette -->
<!-- add link on vispa-malaria/sero-analysis repo to pkgdown website and include dois -->
<!-- update in pvseroapp the dois and references -->
<!-- update in vispa-malaria/sero-analysis the dois and references -->
<!-- is there a google analytics for vispa-malaria/sero-analysis? -->