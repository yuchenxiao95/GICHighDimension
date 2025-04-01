# Package hooks and Julia dependency management

.onLoad <- function(libname, pkgname) {
  # Set up package environment variables
  assign(".JULIA_ENV", new.env(), envir = parent.env(environment()))

  # Check for Julia installation
  if (!julia_setup_ok()) {
    packageStartupMessage(
      "Julia not found. ",
      "Run `GICModelSelection::setup_julia()` to install Julia."
    )
  }
}

.onAttach <- function(libname, pkgname) {
  # Friendly startup message
  packageStartupMessage(
    sprintf("Loaded %s (v%s). Run `setup_julia()` if first-time use.",
            pkgname,
            utils::packageVersion(pkgname))
  )
}

#' @title Initialize Julia Environment
#' @description Installs required Julia packages if missing
#' @param julia_home Optional path to Julia binary (auto-detected if NULL)
#' @param pkg_check If TRUE, verifies all Julia packages are installed
#' @export
setup_julia <- function(julia_home = NULL, pkg_check = TRUE) {
  # 1. Julia binary setup
  if (!is.null(julia_home)) {
    Sys.setenv(JULIA_BINDIR = julia_home)
  }

  # 2. Initialize Julia session
  tryCatch({
    JuliaCall::julia_setup(installJulia = ifelse(julia_setup_ok(), FALSE, TRUE))
    assign(".JULIA_READY", TRUE, envir = .JULIA_ENV)
  }, error = function(e) {
    stop("Julia initialization failed:\n", e$message)
  })

  # 3. Install required packages
  if (pkg_check) {
    install_julia_dependencies()
  }

  invisible(TRUE)
}

# Internal helper: Check if Julia works
julia_setup_ok <- function() {
  tryCatch({
    JuliaCall::julia_eval("1+1")
    TRUE
  }, error = function(e) FALSE)
}

#' @importFrom JuliaCall julia_install_package julia_library
install_julia_dependencies <- function() {
  required_pkgs <- c(
    "Distributions",
    "LinearAlgebra",
    "Statistics",
    "Random"
  )

  # Install missing packages
  for (pkg in required_pkgs) {
    if (!julia_pkg_installed(pkg)) {
      message("Installing Julia package: ", pkg)
      JuliaCall::julia_install_package(pkg)
    }
    JuliaCall::julia_library(pkg)
  }
}

# Internal: Check if Julia package exists
julia_pkg_installed <- function(pkg) {
  tryCatch({
    JuliaCall::julia_eval(sprintf('using %s', pkg))
    TRUE
  }, error = function(e) FALSE)
}
