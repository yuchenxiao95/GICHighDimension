#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom cli col_red col_blue col_green col_yellow

# Internal package state
.pkg_env <- new.env(parent = emptyenv())

# Internal helper: check if Julia is responsive
get_julia_status <- function() {
  tryCatch({
    JuliaCall::julia_eval("1 + 1") == 2
  }, error = function(e) FALSE)
}

# Internal helper: check for required Julia packages
.check_julia_dependencies <- function(quiet = FALSE) {
  required_pkgs <- c("Distributions", "LinearAlgebra", "Statistics", "Random")
  all_installed <- TRUE

  for (pkg in required_pkgs) {
    tryCatch({
      JuliaCall::julia_eval(sprintf("import %s", pkg))
    }, error = function(e) {
      all_installed <<- FALSE
      if (!quiet) cli_alert_warning(sprintf("Missing Julia package: '%s'", pkg))
    })
  }

  if (!all_installed && !quiet) {
    cli_alert_info("Run install_julia_packages() to install missing packages.")
  }

  all_installed
}

#' @title Initialize 'Julia' Environment
#' @description
#' Sets up the connection to 'Julia' using the 'JuliaCall' interface. This function should be run manually before calling other functions that rely on 'Julia'.
#'
#' @param julia_path Optional character string. Path to the 'Julia' binary. If NULL, uses system default.
#' @param install_julia Logical. If TRUE, attempts to install 'Julia' (not recommended for CRAN use). Default is FALSE.
#' @param install_deps Logical. If TRUE, checks for required 'Julia' packages but does not install them automatically. Default is TRUE.
#' @param quiet Logical. If TRUE, suppresses status messages. Default is FALSE.
#'
#' @return Invisibly returns TRUE if initialization succeeds, otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
#'   setup_julia(install_julia = FALSE)
#' }
#' }
#'
#' @export
setup_julia <- function(julia_path = NULL,
                        install_julia = FALSE,
                        install_deps = TRUE,
                        quiet = FALSE) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Install it with install.packages('JuliaCall').", call. = FALSE)
  }

  tryCatch({
    if (!is.null(julia_path)) {
      if (!quiet) cli_alert_info(paste("Using Julia binary at:", julia_path))
      JuliaCall::julia_setup(JULIA_HOME = julia_path, installJulia = install_julia)
    } else {
      JuliaCall::julia_setup(installJulia = install_julia)
    }

    assign("julia_initialized", TRUE, envir = .pkg_env)

    if (install_deps) {
      if (!quiet) cli_alert_info("Checking for required Julia packages...")
      deps_ok <- .check_julia_dependencies(quiet = quiet)
      assign("julia_deps_installed", deps_ok, envir = .pkg_env)
    }

    if (!quiet) cli_alert_success("Julia environment initialized successfully.")
    invisible(TRUE)
  }, error = function(e) {
    assign("julia_initialized", FALSE, envir = .pkg_env)
    if (!quiet) cli_alert_danger("Julia initialization failed.")
    stop(e$message, call. = FALSE)
  })
}

#' @title Install Required 'Julia' Packages
#' @description
#' Installs the 'Julia' packages required by this package. This function must be run manually and is not intended for use in examples, tests, or vignettes.
#'
#' @param packages Character vector of 'Julia' package names. Defaults to core packages used by this package.
#' @param quiet Logical. If TRUE, suppresses installation messages. Default is FALSE.
#'
#' @return Invisibly returns TRUE if all packages were installed successfully, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
#'   setup_julia(install_deps = FALSE)
#'   install_julia_packages()
#' }
#' }
#'
#' @export
install_julia_packages <- function(
    packages = c("Distributions", "LinearAlgebra", "Statistics", "Random"),
    quiet = FALSE) {

  if (!get_julia_status()) {
    stop("Julia is not initialized. Please call setup_julia() first.", call. = FALSE)
  }

  results <- list()

  for (pkg in packages) {
    tryCatch({
      if (!quiet) cli_alert_info(paste("Installing Julia package:", pkg))
      JuliaCall::julia_install_package_if_needed(pkg)
      JuliaCall::julia_library(pkg)
      results[[pkg]] <- TRUE
    }, error = function(e) {
      results[[pkg]] <- FALSE
      if (!quiet) cli_alert_danger(sprintf("Failed to install '%s': %s", pkg, e$message))
    })
  }

  success <- all(unlist(results))
  assign("julia_deps_installed", success, envir = .pkg_env)

  if (!quiet && success) {
    cli_alert_success("All required Julia packages installed successfully.")
  }

  invisible(success)
}
