# Helper function to process bioplex sections

Helper function to process bioplex sections

## Usage

``` r
.post_process_bioplex(df)
```

## Arguments

- df:

  Output from \`.read_luminex_file()\`

## Value

List of data_raw, results, counts, blanks, stds, run

## Author

Dionne Argyropoulos

your_raw_data \<- system.file("extdata", "example_BioPlex_plate1.xlsx",
package = "SeroTrackR") df \<- .read_luminex_file(your_raw_data)
sections \<- .post_process_bioplex(df)
