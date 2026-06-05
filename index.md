# SeroTrackR

[SeroTrackR](https://github.com/dionnecargy/SeroTrackR) is a tool to for
serology data analysis and visualisation, and for the application of the
machine learning algorithms implemented in the `{PvSeroApp}`. The R
package is intended to give users more flexibility in assessing serology
data and manipulate data visualisations to the user’s interest. You can
download the R package using the following:

``` r

install.packages("SeroTrackR")
library(SeroTrackR)
```

Alternatively you can download the package from the
[pak](https://pak.r-lib.org/) R package:

``` r

if(!require(pak)){
  install.packages("pak") # If not already installed
} 
pak::pak("dionnecargy/SeroTrackR")
library(SeroTrackR)
```

## Tutorials

- [PvSeroApp in R
  Tutorial](https://dionnecargy.github.io/SeroTrackR/articles/02_PvSeroApp_R_Tutorial.md)
- [Pk/Pv/Pf Serology R
  Tutorial](https://dionnecargy.github.io/SeroTrackR/articles/03_Pk_Pv_Pf_Serology_Tutorial.md)
- [PvLDH
  Tutorial](https://dionnecargy.github.io/SeroTrackR/articles/04_PvLDH_Tutorial.md)
- [FAQs](https://dionnecargy.github.io/SeroTrackR/articles/05_FAQs.md)
