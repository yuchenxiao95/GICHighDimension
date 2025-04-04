## Test environments
* macOS Sonoma 14.6, R 4.4.3
* Ubuntu 22.04 (via GitHub Actions), R-release
* Windows (via win-builder), R-devel

## R CMD check results
There were no ERRORs or WARNINGs.
There was 1 NOTE:
* Unable to verify current time — known harmless issue.


## CRAN submission comments

* This is the first submission of GICHighDimension.
* All examples requiring Julia are wrapped in `\donttest{}` and skip execution unless Julia is available.
* The GitHub Actions badge was corrected to avoid broken URL.
* Package passes `R CMD check` locally on macOS and Linux (Ubuntu).

