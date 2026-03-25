# Helper function to fit a 5-parameter logistic standard curve to dilutions

Helper function to fit a 5-parameter logistic standard curve to
dilutions

## Usage

``` r
.process_antigen_loglog(
  subset_data,
  antigen,
  dilution,
  s1_concentration,
  s_final_concentration,
  unknown_letters = c("U", "X")
)
```

## Arguments

- subset_data:

  Data for one plate.

- antigen:

  Data for one antigen.

- dilution:

  Set of five or ten.

- s1_concentration:

  Concentration of highest dilution.

- s_final_concentration:

  Concentration lowest dilution.

- unknown_letters:

  Bioplex, Magpix or Intelliflex known unknown letters (Default = U and
  X).

## Value

A list of the model results data frame and model.

## Author

Connie Li Wai Suen, Dionne Argyropoulos
