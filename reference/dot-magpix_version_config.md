# Helper function to identify Magpix version

Helper function to identify Magpix version

## Usage

``` r
.magpix_version_config(version)
```

## Arguments

- version:

  String with the raw data path.

## Value

specific column names for filtering for xPONENT software v4.2 and v4.3

## Author

Dionne Argyropoulos

## Examples

``` r
version = "4.2"
.magpix_version_config(version)
#> $end_count
#> [1] "Avg Net MFI"
#> 
```
