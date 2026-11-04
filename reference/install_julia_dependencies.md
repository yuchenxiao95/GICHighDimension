# Install Julia and Required Packages

Manually install Julia (optional) and ensure external Julia packages are
available. Stdlibs (LinearAlgebra, Statistics, Random) are not installed
because they ship with Julia. This function is **interactive/manual**
and should not be used in examples/tests/vignettes.

## Usage

``` r
install_julia_dependencies(
  install_julia = FALSE,
  packages = c("Distributions")
)
```

## Arguments

- install_julia:

  Logical; if TRUE, attempt to install Julia (not CRAN-friendly).
  Default FALSE.

- packages:

  Character vector of external Julia packages to ensure. Default:
  "Distributions".

## Value

Invisibly TRUE on success, FALSE otherwise.
