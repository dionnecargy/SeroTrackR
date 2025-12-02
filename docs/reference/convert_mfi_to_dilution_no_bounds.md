# Convert mfi to dilution using known standard curve fit and no bounds

Convert mfi to dilution using known standard curve fit and no bounds
unless you are below the asymptote of the standard curve. In this
situation we set your value to min_relative_dilution. I dunno argue?

## Usage

``` r
convert_mfi_to_dilution_no_bounds(mfi, params, min_relative_dilution)
```

## Arguments

- mfi:

  Known mfi of samples

- params:

  Known parameters for five parameter logistic fit.

- min_relative_dilution:

  Known minimum value of dilution in the standard curve. Relative means
  setting S1 to a dilution/RAU/concentration of 1.

## Value

Returns the dilution of each sample in mfi.

## Author

Eamon Conway

## Examples

``` r
# This function is generally called inside higher-level analysis workflows.
# Below is a minimal self-contained example using dummy values.

# Dummy five-parameter logistic fit parameters:
# a, b, c, d, e  (with e typically supplied on the log scale)
dummy_params <- c(a = 10000, b = 1.2, c = 0.05, d = 50, e = log(0.01))

# Example MFI value
mfi_example <- 1500

# Minimum relative dilution from the standard curve
min_rel_dil <- 1

# Convert MFI to dilution without bounds
convert_mfi_to_dilution_no_bounds(mfi_example, dummy_params, min_rel_dil)
#>            d 
#> 1.929222e-22 
```
