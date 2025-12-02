# SeroTrackR

[SeroTrackR](https://github.com/dionnecargy/SeroTrackR) is a tool to for
serology data analysis and visualisation, and for the application of the
machine learning algorithms implemented in the `{PvSeroApp}`. The R
package is intended to give users more flexibility in assessing serology
data and manipulate data visualisations to the user’s interest. You can
download the R package using the following:

``` r
# Run once to configure your package to use and deploy SeroTrackR
if(!require(devtools)){
  install.packages("devtools") # If not already installed
} 
devtools::install_github("dionnecargy/SeroTrackR")
library(SeroTrackR)
```

Alternatively you can download the package from the
[remotes](https://remotes.r-lib.org) R package:

``` r
if(!require(remotes)){
  install.packages("remotes") # If not already installed
} 
library(remotes)
remotes::install_github("dionnecargy/SeroTrackR")
```

## Tutorials

- [PvSeroApp in R
  Tutorial](https://dionnecargy.github.io/SeroTrackR/articles/02_PvSeroApp_R_Tutorial.md)
- [Pk/Pv/Pf Serology R
  Tutorial](https://dionnecargy.github.io/SeroTrackR/articles/03_Pk_Pv_Pf_Serology_R_Tutorial.md)
- [PvLDH
  Tutorial](https://dionnecargy.github.io/SeroTrackR/articles/04_PvLDH_Tutorial.md)
- [FAQs](https://dionnecargy.github.io/SeroTrackR/articles/05_FAQs.md)
