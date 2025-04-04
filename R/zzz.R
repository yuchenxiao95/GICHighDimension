#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning col_red col_blue col_green col_yellow

# Package environment for state management
.pkg_env <- new.env(parent = emptyenv())

# Internal helper to check Julia status
get_julia_status <- function() {
  tryCatch({
    JuliaCall::julia_eval("1+1") == 2
  }, error = function(e) FALSE)
}

# Internal helper to check/install required Julia packages
install_julia_dependencies <- function(quiet = FALSE) {
  required_pkgs <- c("Distributions", "LinearAlgebra", "Statistics", "Random")
  success <- TRUE

  for (pkg in required_pkgs) {
    tryCatch({
      JuliaCall::julia_eval(sprintf("import %s", pkg))
    }, error = function(e) {
      if (!quiet) cli_alert_warning("Julia package {pkg} not found")
      success <<- FALSE
    })
  }

  if (!success && !quiet) {
    cli_alert_info("Install missing packages with install_julia_packages()")
  }

  success
}

#' @title Initialize Julia Environment
#' @description
#' Sets up the Julia connection and installs required Julia packages for this package to run.
#'
#' @param julia_path Optional path to Julia binary. If NULL, searches system PATH.
#' @param install_julia If TRUE, attempts to install Julia (default: FALSE).
#' @param install_deps If TRUE, installs required Julia packages (default: TRUE).
#' @param quiet If TRUE, suppresses messages (default: FALSE).
#' @return Invisible TRUE if successful, FALSE otherwise.
#' @export
setup_julia <- function(julia_path = NULL,
                        install_julia = FALSE,
                        install_deps = TRUE,
                        quiet = FALSE) {

  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Required package 'JuliaCall' not found.", call. = FALSE)
  }

  if (!is.null(julia_path)) {
    if (!quiet) cli_alert_info("Using Julia at: {julia_path}")
    JuliaCall::julia_setup(JULIA_HOME = julia_path)
  }

  tryCatch({
    if (!quiet) cli_alert_info("Initializing Julia...")
    JuliaCall::julia_setup(installJulia = install_julia)
    assign("julia_initialized", TRUE, envir = .pkg_env)

    if (install_deps) {
      if (!quiet) cli_alert_info("Checking Julia packages...")
      result <- install_julia_dependencies(quiet = quiet)
      assign("julia_deps_installed", result, envir = .pkg_env)
    }

    if (!quiet) cli_alert_success("Julia initialized successfully")
    invisible(TRUE)
  }, error = function(e) {
    assign("julia_initialized", FALSE, envir = .pkg_env)
    if (!quiet) cli_alert_danger("Julia initialization failed")
    stop(e$message, call. = FALSE)
  })
}

#' @title Install Required Julia Packages
#' @description
#' Installs the necessary Julia packages for the GICHighDimension package to function.
#'
#' @param packages Character vector of package names to install.
#' @param quiet If TRUE, suppresses output messages.
#' @return Invisible TRUE if all packages were installed successfully.
#' @export
install_julia_packages <- function(packages = c("Distributions", "LinearAlgebra", "Statistics", "Random"),
                                   quiet = FALSE) {
  if (!get_julia_status()) {
    stop("Julia not initialized. Run setup_julia() first.", call. = FALSE)
  }

  results <- list()
  for (pkg in packages) {
    tryCatch({
      if (!quiet) cli_alert_info("Installing {pkg}...")
      JuliaCall::julia_install_package_if_needed(pkg)
      JuliaCall::julia_library(pkg)
      results[[pkg]] <- TRUE
    }, error = function(e) {
      if (!quiet) cli_alert_danger("Failed to install {pkg}: {e$message}")
      results[[pkg]] <- FALSE
    })
  }

  success <- all(unlist(results))
  assign("julia_deps_installed", success, envir = .pkg_env)

  if (!quiet && success) {
    cli_alert_success("All packages installed successfully")
  }

  invisible(success)
}
