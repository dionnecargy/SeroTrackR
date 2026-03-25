# Convert known dilution to mfi from fitted standard curve

Convert dilution to predicted mfi using known standard curve fit.

## Usage

``` r
.convert_dilution_to_mfi(dilution, params)
```

## Arguments

- dilution:

  Known dilution of samples

- params:

  Known parameters for five parameter logistic fit.

## Value

Returns the predicted mfi of a sample with known dilution.

## Author

Eamon Conway

## Examples

``` r
# This function is typically called internally by higher-level workflows.
# Below is a minimal runnable example using dummy parameters.

# Five-parameter logistic model typically expects parameters in the order:
# a, b, c, d, e  (e often log-transformed)
dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01))

# Example dilution value
dilution_example <- 0.1

# Predict MFI from the dummy standard curve
.convert_dilution_to_mfi(dilution_example, dummy_params)
#>        b 
#> 3.320117 
```
