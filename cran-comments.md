## Test environments
* local macOS Sonoma 14.6, R 4.4.3
* win-builder (devel)
* rhub (devel, Windows + Debian)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 1 NOTE (expected for new submission)

## Notes
* The NOTE about future timestamps is benign and can be ignored (local macOS limitation).
* All examples that depend on 'Julia' are now wrapped in \dontrun{} or \donttest{}.
* No software is installed in examples, vignettes, or tests — following CRAN policy.
* Parallel usage is capped (no more than 2 cores).
* Package passes all unit tests with `testthat`.
* Julia-dependent tests are skipped on CRAN via `testthat::skip_on_cran()`.
* The terms ‘GIC’ and ‘Hopfield’ are domain-specific and appropriately used in the context of model selection and neural optimization.

## Submission purpose
* Initial CRAN submission of the package `GICHighDimension`.

## Additional Information
* The package requires an external Julia installation but does not initiate installation automatically.
* Functions to configure the environment (e.g., `setup_julia()`, `install_julia_packages()`) are excluded from examples/tests/vignettes to comply with CRAN runtime rules.

