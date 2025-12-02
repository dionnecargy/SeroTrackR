# Get GitHub Version

A short function to obtain the github version from the repository. This
is a generalisable function that can be used for any version tags on a
repo.

## Usage

``` r
getGithubRelease(repo_owner, repo_name)
```

## Arguments

- repo_owner:

  GitHub Username

- repo_name:

  GitHub Repository Name

## Value

Version tag string

## Author

Dionne Argyropoulos

## Examples

``` r
getGithubRelease(
 repo_owner = "dionnecargy",
 repo_name = "SeroTrackR"
)
#> NULL
```
