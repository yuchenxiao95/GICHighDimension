# Initialize 'Julia' Environment

Sets up the connection to 'Julia' via 'JuliaCall'. Run this before
calling functions that rely on Julia. This function does **not** install
Julia by default (CRAN-friendly).

## Usage

``` r
setup_julia(
  julia_path = NULL,
  install_julia = FALSE,
  install_deps = TRUE,
  quiet = FALSE
)
```

## Arguments

- julia_path:

  Optional path to Julia: either the binary (e.g. /path/to/bin/julia) or
  JULIA_HOME (a directory containing bin/). If NULL, uses system
  default.

- install_julia:

  Logical; if TRUE, allows JuliaCall to attempt a Julia install (not
  recommended for CRAN). Default FALSE.

- install_deps:

  Logical; if TRUE, check for required Julia packages (no auto install).
  Default TRUE.

- quiet:

  Logical; suppress status messages. Default FALSE.

## Value

Invisibly TRUE on success, otherwise throws an error.
