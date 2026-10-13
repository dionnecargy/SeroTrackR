---
title: "Getting Started"
---

# SeroTrackR 


`{SeroTrackR}` is a tool to for serology data analysis and visualisation, and for the application of the machine learning algorithms implemented in the `{PvSeroApp}`. The R package is intended to give users more flexibility in assessing serology data and manipulate data visualisations to the user's interest. 
You can download the R package using the following: 

``` r
install.packages("SeroTrackR")
library(SeroTrackR)
```

Alternatively you can download the package from the `{pak}` R package: 

``` r
if(!require(pak)){
  install.packages("pak") # If not already installed
} 
pak::pak("dionnecargy/SeroTrackR")
library(SeroTrackR)
```

## Tutorials

- [Before You Begin](articles/setup.html)
- [PvSeroApp in R Tutorial](articles/PvSeroApp_R_Tutorial.html)
- [PvLDH Tutorial](articles/PvLDH_Tutorial.html)
- [Pk/Pv/Pf Serology Tutorial](articles/Pk_Pv_Pf_Serology_Tutorial.html)
- [FAQs](articles/FAQs.html)

