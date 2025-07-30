# <img src="hex/SeroTrackR_sticker.png" width="25%" height="25%" align="left"/> SeroTrackR

The `{SeroTrackR}` R package compliments the PvSeroApp [see Github here](https://github.com/dionnecargy/PvSeroApp) for experienced users who are comfortable with implementing the code in their own R sessions. 

This package contains helper functions relevant for *Plasmodium* spp. projects for in-house and collaborator needs. 

<p>&nbsp;</p>
<p>&nbsp;</p>

## How to install the package:

```{r}
devtools::install_github("dionnecargy/SeroTrackR") # To download the package
library(SeroTrackR) # To load the package 
```

## How can I use the package?

Please see the [tutorial](https://dionnecargy.github.io/SeroTrackR/) or see the vignettes inside the package by running the following commands: 

```{r}
browseVignettes("SeroTrackR") 

# To run a specific vignette: 
vignette("PvSeroApp_R_Tutorial")
vignette("Pk_Pv_Pf_Serology_R_Tutorial")
```
