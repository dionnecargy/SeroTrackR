# FAQs

## FAQs

### I have multiple plate layout files. How can I input them?

Use the
[`getPlateLayout()`](https://dionnecargy.github.io/SeroTrackR/reference/getPlateLayout.md)
function to create a master plate layout file to then input into the
other functions in the package!

``` r
getPlateLayout("your/folder/with/plate/layouts/")
```

Here replace “your/folder/with/plate/layouts/” with the main file that
contains your folders. For example, if your folder looks like this:

``` bash
my_R_project/
└── raw_data_files/
    ├── plate_1/
    │   ├── raw_magpix_data_plate1.csv
    │   └── plate_layout_1.xlsx
    ├── plate_2/
    │   ├── raw_magpix_data_plate2.csv
    │   └── plate_layout_2.xlsx
    └── plate_3/
        ├── raw_magpix_data_plate3.csv
        └── plate_layout_3.xlsx
```

you would write:

``` r
getPlateLayout("raw_data_files/")
```

you could ALSO write:

``` r
getPlateLayout()
```

OR:

``` r
getPlateLayout(folder_path = c("plate_layout_1.xlsx", "plate_layout_2.xlsx", "plate_layout_3.xlsx"))
```
