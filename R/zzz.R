#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom cli col_red col_blue col_green col_yellow

# Internal package state
.pkg_env <- new.env(parent = emptyenv())

# ---------------- Internal helpers ----------------

# Is Julia up and responding?
get_julia_status <- function() {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) return(FALSE)
  tryCatch({
    isTRUE(JuliaCall::julia_eval("1 + 1") == 2)
  }, error = function(e) FALSE)
}

# Normalize a user-supplied julia_path (binary or JULIA_HOME) -> JULIA_HOME
.normalize_julia_home <- function(julia_path) {
  if (is.null(julia_path) || !nzchar(julia_path)) return(NULL)
  
  # If path points to .../bin/julia (the binary), take its parent dir's parent as JULIA_HOME
  if (file.exists(julia_path) && grepl("(^|/|\\\\)julia(\\.exe)?$", julia_path)) {
    # julia_path = .../bin/julia
    bin_dir <- normalizePath(dirname(julia_path), winslash = "/", mustWork = FALSE)
    home <- normalizePath(dirname(bin_dir), winslash = "/", mustWork = FALSE)
    return(home)
  }
  
  # If path contains a bin/julia inside it, assume it's JULIA_HOME already
  bin_candidate <- file.path(julia_path, "bin", ifelse(.Platform$OS.type == "windows", "julia.exe", "julia"))
  if (file.exists(bin_candidate)) {
    return(normalizePath(julia_path, winslash = "/", mustWork = FALSE))
  }
  
  # Otherwise, just return as-is; JuliaCall may still resolve it
  normalizePath(julia_path, winslash = "/", mustWork = FALSE)
}

# Check core Julia deps. Split stdlibs vs external.
.check_julia_dependencies <- function(quiet = FALSE) {
  if (!get_julia_status()) return(FALSE)
  
  stdlibs   <- c("LinearAlgebra", "Statistics", "Random")
  externals <- c("Distributions")  # add more external deps here if needed
  
  all_ok <- TRUE
  
  # Check stdlibs (should always be present; don't try to install)
  for (pkg in stdlibs) {
    ok <- tryCatch({ JuliaCall::julia_eval(sprintf("import %s; true", pkg)) }, error = function(e) FALSE)
    if (!ok) {
      all_ok <- FALSE
      if (!quiet) cli_alert_warning(sprintf("Julia stdlib not importable: '%s' (unexpected)", pkg))
    }
  }
  
  # Check externals (installable)
  for (pkg in externals) {
    ok <- tryCatch({ JuliaCall::julia_eval(sprintf("import %s; true", pkg)) }, error = function(e) FALSE)
    if (!ok) {
      all_ok <- FALSE
      if (!quiet) cli_alert_warning(sprintf("Missing Julia package: '%s'", pkg))
    }
  }
  
  if (!all_ok && !quiet) {
    cli_alert_info("Run install_julia_packages() to install missing external packages.")
  }
  
  all_ok
}

# Source the Julia backend only once per session
.source_julia_backend_once <- function(quiet = FALSE) {
  if (isTRUE(get(".jl_backend_loaded", envir = .pkg_env, inherits = FALSE))) return(invisible(TRUE))
  if (!get_julia_status()) stop("Julia is not initialized. Call setup_julia() first.", call. = FALSE)
  
  jl_dir <- system.file("julia", package = "GICHighDimension")
  if (jl_dir == "" || !dir.exists(jl_dir)) {
    stop("Julia scripts directory not found in the package (inst/julia).")
  }
  
  files <- c("penalty_selection.jl", "GIC_Calculation.jl", "GIC_Model_Selection.jl")
  for (f in files) {
    fp <- file.path(jl_dir, f)
    if (file.exists(fp)) {
      if (!quiet) cli_alert_info(sprintf("Loading Julia script: %s", f))
      JuliaCall::julia_source(fp)
    } else if (!quiet) {
      cli_alert_warning(sprintf("Julia script not found: %s", f))
    }
  }
  
  # Optional: install "arity adapters" so your driver can pass extra args safely
  JuliaCall::julia_eval(
    "
    if !isdefined(Main, :_gic_wrappers_installed) || !_gic_wrappers_installed
        for f in (:Calculate_AIC, :Calculate_AIC_c, :Calculate_SIC, :Calculate_BIC,
                  :Calculate_CAIC, :Calculate_CAICF, :Calculate_AttIC)
            if isdefined(Main, f) && !hasmethod(getfield(Main, f),
                     Tuple{Union{AbstractVector,AbstractMatrix}, AbstractMatrix, Integer})
                @eval begin
                    $f(Y::Union{AbstractVector,AbstractMatrix}, X::AbstractMatrix, ::Integer) = $f(Y, X)
                end
            end
        end
        for f in (:Calculate_AIC_short, :Calculate_AIC_c_short, :Calculate_SIC_short, :Calculate_BIC_short,
                  :Calculate_CAIC_short, :Calculate_CAICF_short, :Calculate_AttIC_short)
            if isdefined(Main, f) && !hasmethod(getfield(Main, f),
                     Tuple{Union{AbstractVector,AbstractMatrix}, AbstractMatrix, AbstractMatrix, Integer})
                @eval begin
                    $f(Y::Union{AbstractVector,AbstractMatrix}, X::AbstractMatrix, Inv::AbstractMatrix, ::Integer) = $f(Y, X, Inv)
                end
            end
        end
        const _gic_wrappers_installed = true
    end
    "
  )
  
  assign(".jl_backend_loaded", TRUE, envir = .pkg_env)
  invisible(TRUE)
}

# ---------------- Public API ----------------

#' @title Initialize 'Julia' Environment
#' @description
#' Sets up the connection to 'Julia' via 'JuliaCall'. Run this before calling functions
#' that rely on Julia. This function does **not** install Julia by default (CRAN-friendly).
#'
#' @param julia_path Optional path to Julia: either the binary (e.g. /path/to/bin/julia)
#'   or JULIA_HOME (a directory containing bin/). If NULL, uses system default.
#' @param install_julia Logical; if TRUE, allows JuliaCall to attempt a Julia install (not recommended for CRAN). Default FALSE.
#' @param install_deps Logical; if TRUE, check for required Julia packages (no auto install). Default TRUE.
#' @param quiet Logical; suppress status messages. Default FALSE.
#' @return Invisibly TRUE on success, otherwise throws an error.
#' @export
setup_julia <- function(julia_path = NULL,
                        install_julia = FALSE,
                        install_deps = TRUE,
                        quiet = FALSE) {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Install it with install.packages('JuliaCall').", call. = FALSE)
  }
  
  tryCatch({
    julia_home <- .normalize_julia_home(julia_path)
    if (!is.null(julia_home)) {
      if (!quiet) cli_alert_info(paste("Using JULIA_HOME:", julia_home))
      JuliaCall::julia_setup(JULIA_HOME = julia_home, installJulia = install_julia)
    } else {
      JuliaCall::julia_setup(installJulia = install_julia)
    }
    
    assign("julia_initialized", TRUE, envir = .pkg_env)
    
    if (install_deps) {
      if (!quiet) cli_alert_info("Checking Julia dependencies...")
      deps_ok <- .check_julia_dependencies(quiet = quiet)
      assign("julia_deps_installed", deps_ok, envir = .pkg_env)
    }
    
    # Load backend once after setup
    .source_julia_backend_once(quiet = quiet)
    
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
#' Installs the external Julia packages required by this package. Must be run manually;
#' not intended for examples, tests, or vignettes.
#'
#' @param packages Character vector of external Julia package names to ensure are installed.
#'   Defaults to c("Distributions"). Stdlibs (LinearAlgebra, Statistics, Random) are **not**
#'   installable and are always present.
#' @param quiet Logical; suppress messages.
#' @return Invisibly TRUE if all packages were installed/are present, FALSE otherwise.
#' @export
install_julia_packages <- function(
    packages = c("Distributions"),
    quiet = FALSE) {
  
  if (!get_julia_status()) {
    stop("Julia is not initialized. Please call setup_julia() first.", call. = FALSE)
  }
  
  # Sanity: drop stdlibs if the user passed them
  stdlibs <- c("LinearAlgebra", "Statistics", "Random")
  if (any(packages %in% stdlibs) && !quiet) {
    cli_alert_info("Ignoring stdlib entries in 'packages': they are already available in Julia.")
  }
  packages <- setdiff(packages, stdlibs)
  
  if (length(packages) == 0L) {
    if (!quiet) cli_alert_success("No external Julia packages to install.")
    assign("julia_deps_installed", TRUE, envir = .pkg_env)
    return(invisible(TRUE))
  }
  
  results <- list()
  
  for (pkg in packages) {
    ok <- FALSE
    # Try to import; if fails, install
    ok <- tryCatch({ JuliaCall::julia_eval(sprintf("import %s; true", pkg)) }, error = function(e) FALSE)
    
    if (!ok) {
      tryCatch({
        if (!quiet) cli_alert_info(paste("Installing Julia package:", pkg))
        JuliaCall::julia_install_package_if_needed(pkg)
        JuliaCall::julia_library(pkg)
        results[[pkg]] <- TRUE
      }, error = function(e) {
        results[[pkg]] <- FALSE
        if (!quiet) cli_alert_danger(sprintf("Failed to install '%s': %s", pkg, e$message))
      })
    } else {
      results[[pkg]] <- TRUE
      if (!quiet) cli_alert_success(sprintf("Julia package already available: %s", pkg))
    }
  }
  
  success <- all(unlist(results))
  assign("julia_deps_installed", success, envir = .pkg_env)
  
  if (!quiet && success) {
    cli_alert_success("All required external Julia packages are installed.")
  }
  
  invisible(success)
}
