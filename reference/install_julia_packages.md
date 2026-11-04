# Install Required 'Julia' Packages

Installs the external Julia packages required by this package. Must be
run manually; not intended for examples, tests, or vignettes.

## Usage

``` r
install_julia_packages(packages = c("Distributions"), quiet = FALSE)
```

## Arguments

- packages:

  Character vector of external Julia package names to ensure are
  installed. Defaults to c("Distributions"). Stdlibs (LinearAlgebra,
  Statistics, Random) are **not** installable and are always present.

- quiet:

  Logical; suppress messages.

## Value

Invisibly TRUE if all packages were installed/are present, FALSE
otherwise.
