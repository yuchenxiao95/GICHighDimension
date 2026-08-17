#' @title Install Julia and Required Packages
#' @description
#' Manually install Julia (optional) and ensure external Julia packages are available.
#' Stdlibs (LinearAlgebra, Statistics, Random) are not installed because they ship with Julia.
#' This function is **interactive/manual** and should not be used in examples/tests/vignettes.
#'
#' @param install_julia Logical; if TRUE, attempt to install Julia (not CRAN-friendly). Default FALSE.
#' @param packages Character vector of external Julia packages to ensure. Default: "Distributions".
#' @return Invisibly TRUE on success, FALSE otherwise.
#' @export
install_julia_dependencies <- function(install_julia = FALSE,
                                       packages = c("Distributions")) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Install with install.packages('JuliaCall').", call. = FALSE)
  }
  
  # Optionally install Julia itself (avoid on CRAN)
  if (isTRUE(install_julia)) {
    # JuliaCall exports install_julia(); if not present, fallback to julia_setup(installJulia=TRUE)
    if (is.function(getFromNamespace("install_julia", "JuliaCall"))) {
      JuliaCall::install_julia()
    } else {
      JuliaCall::julia_setup(installJulia = TRUE)
    }
  }
  
  # Ensure Julia is initialized (without auto-install)
  if (!isTRUE(tryCatch({ JuliaCall::julia_setup(installJulia = FALSE); TRUE },
                       error = function(e) FALSE))) {
    stop("Failed to initialize Julia. Run setup_julia() first or set install_julia = TRUE.", call. = FALSE)
  }
  
  # Filter out stdlibs if the user passed them by mistake
  stdlibs <- c("LinearAlgebra", "Statistics", "Random")
  packages <- setdiff(unique(packages), stdlibs)
  
  if (length(packages) == 0L) {
    return(invisible(TRUE))
  }
  
  ok <- TRUE
  for (pkg in packages) {
    # Try to import; if missing, install then import
    present <- isTRUE(tryCatch({ JuliaCall::julia_eval(sprintf("import %s; true", pkg)) },
                               error = function(e) FALSE))
    if (!present) {
      tryCatch({
        JuliaCall::julia_install_package_if_needed(pkg)
        JuliaCall::julia_library(pkg)
      }, error = function(e) {
        ok <<- FALSE
        warning(sprintf("Failed to install Julia package '%s': %s", pkg, e$message), call. = FALSE)
      })
    }
  }
  
  invisible(ok)
}
