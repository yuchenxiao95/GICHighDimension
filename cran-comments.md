## Test environments
* macOS Sonoma 14.6, R 4.4.3
* Ubuntu 22.04 (via GitHub Actions), R-release
* Windows (via win-builder), R-devel

## R CMD check results
There were no ERRORs or WARNINGs.
There was 1 NOTE:
* Unable to verify current time — known harmless issue.

## Additional comments
* This is the initial submission of GICHighDimension to CRAN.
* The package provides Julia-accelerated variable selection tools based on Generalized Information Criterion (GIC).
* Julia dependencies are managed through JuliaCall and documented thoroughly.
