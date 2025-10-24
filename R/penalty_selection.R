#' @title Information Criterion Calculations via Julia
#'
#' @description
#' Compute information criteria using efficient 'Julia' backends via the 'JuliaCall' package.
#' These functions are useful for variable selection and model comparison in high-dimensional
#' settings. Each function returns either the computed criterion value, or a list with additional data.
#'
#' @details
#' This interface leverages 'Julia' to compute information-theoretic criteria efficiently,
#' especially when repeatedly evaluating models. Supported criteria include:
#' \itemize{
#'   \item AIC: Akaike Information Criterion
#'   \item AICc: Corrected AIC for small samples
#'   \item BIC / SIC: Bayesian or Schwarz Information Criterion
#'   \item CAIC: Consistent AIC
#'   \item CAICF: Consistent AIC with Fisher Information
#'   \item EBIC: Extended BIC (depends on the candidate pool size \(P\))
#'   \item GIC2–GIC6: Generalized Information Criterion family (depend on \(P\))
#'   \item AttIC: Attention-weighted Information Criterion
#' }
#' Short versions (e.g., `Calculate_AIC_Short`) allow reusing a precomputed inverse matrix
#' to save computational time.
#'
#' @param Y A numeric response (vector of length n). (Current package assumes univariate Y.)
#' @param X A numeric design matrix (n × p).
#' @param Inverse (for `_Short` variants only) a numeric matrix — the inverse of X'X.
#' @param P (only for EBIC and GIC2–GIC6) integer total number of candidate predictors.
#'   Defaults to `ncol(X)`.
#'
#' @return
#' For full versions: a named list with components:
#' \describe{
#'   \item{CriterionValue}{Numeric scalar of the selected information criterion.}
#'   \item{InverseMatrix}{Matrix inverse used in computation (if applicable).}
#' }
#' For `_Short` versions: a numeric scalar of the criterion value.
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
#'   setup_julia(install_julia = FALSE)
#'   X <- matrix(rnorm(100), ncol = 5)
#'   Y <- rnorm(20)
#'   out1 <- Calculate_AIC(Y, X)
#'   out2 <- Calculate_EBIC(Y, X)        # uses P = ncol(X) by default
#'   val_short <- Calculate_EBIC_Short(Y, X, solve(crossprod(X)))
#' }
#' }
#'
#' @name information_criteria
NULL

#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom cli col_red col_blue col_green col_yellow
#' @importFrom JuliaCall julia_setup julia_source julia_call julia_eval julia_exists julia_install_package_if_needed julia_library
NULL

# =========================
# Internal package state
# =========================
.pkg_env <- new.env(parent = emptyenv())

# =========================
# Internal helpers
# =========================

# Is Julia up and responding?
get_julia_status <- function() {
  if (!requireNamespace("JuliaCall", quietly = TRUE)) return(FALSE)
  tryCatch({
    isTRUE(JuliaCall::julia_eval("1 + 1") == 2)
  }, error = function(e) FALSE)
}

# Normalize user julia_path into JULIA_HOME (dir containing bin/)
.normalize_julia_home <- function(julia_path) {
  if (is.null(julia_path) || !nzchar(julia_path)) return(NULL)
  
  # If the path is .../bin/julia(.exe), use parent of bin/ as home
  if (file.exists(julia_path) && grepl("(^|/|\\\\)julia(\\.exe)?$", julia_path)) {
    bin_dir <- normalizePath(dirname(julia_path), winslash = "/", mustWork = FALSE)
    home <- normalizePath(dirname(bin_dir), winslash = "/", mustWork = FALSE)
    return(home)
  }
  
  # If the path contains bin/julia, assume it's JULIA_HOME already
  bin_candidate <- file.path(julia_path, "bin", ifelse(.Platform$OS.type == "windows", "julia.exe", "julia"))
  if (file.exists(bin_candidate)) {
    return(normalizePath(julia_path, winslash = "/", mustWork = FALSE))
  }
  
  # Otherwise return normalized input; JuliaCall may resolve it
  normalizePath(julia_path, winslash = "/", mustWork = FALSE)
}

# Check core Julia deps: stdlibs vs external
.check_julia_dependencies <- function(quiet = FALSE) {
  if (!get_julia_status()) return(FALSE)
  
  stdlibs   <- c("LinearAlgebra", "Statistics", "Random")
  externals <- c("Distributions")  # add external deps here as needed
  
  all_ok <- TRUE
  
  # stdlibs
  for (pkg in stdlibs) {
    ok <- tryCatch({ JuliaCall::julia_eval(sprintf("import %s; true", pkg)) }, error = function(e) FALSE)
    if (!ok) {
      all_ok <- FALSE
      if (!quiet) cli_alert_warning(sprintf("Julia stdlib not importable: '%s' (unexpected)", pkg))
    }
  }
  
  # external packages
  for (pkg in externals) {
    ok <- tryCatch({ JuliaCall::julia_eval(sprintf("import %s; true", pkg)) }, error = function(e) FALSE)
    if (!ok) {
      all_ok <- FALSE
      if (!quiet) cli_alert_warning(sprintf("Missing Julia package: '%s'", pkg))
    }
  }
  
  if (!all_ok && !quiet) {
    cli_alert_info("Run install_julia_dependencies() to install missing external packages.")
  }
  
  all_ok
}

# Source Julia backend once per session (idempotent)
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
  
  # Install arity adapters once so driver can pass extra args safely
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

# =========================
# Public: setup & installs
# =========================

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
  
  if (isTRUE(install_julia)) {
    if (is.function(getFromNamespace("install_julia", "JuliaCall"))) {
      JuliaCall::install_julia()
    } else {
      JuliaCall::julia_setup(installJulia = TRUE)
    }
  }
  
  # initialize (without auto-install)
  if (!isTRUE(tryCatch({ JuliaCall::julia_setup(installJulia = FALSE); TRUE },
                       error = function(e) FALSE))) {
    stop("Failed to initialize Julia. Run setup_julia() first or set install_julia = TRUE.", call. = FALSE)
  }
  
  # drop stdlibs if passed accidentally
  stdlibs <- c("LinearAlgebra", "Statistics", "Random")
  packages <- setdiff(unique(packages), stdlibs)
  
  if (length(packages) == 0L) return(invisible(TRUE))
  
  ok <- TRUE
  for (pkg in packages) {
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

# =========================
# Internal calculation helpers
# =========================

# Always pass Y as numeric vector (package currently assumes univariate Y)
.Y_arg <- function(Y) as.numeric(Y)

# Full versions: criteria that DO NOT depend on P
.Calculate_Criterion_NO_P <- function(func_name, Y, X) {
  .source_julia_backend_once(quiet = TRUE)
  res <- JuliaCall::julia_call(func_name, .Y_arg(Y), as.matrix(X))
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Full versions: criteria that DO depend on P
.Calculate_Criterion_WITH_P <- function(func_name, Y, X, P = ncol(X)) {
  .source_julia_backend_once(quiet = TRUE)
  res <- JuliaCall::julia_call(func_name, .Y_arg(Y), as.matrix(X), as.integer(P))
  list(CriterionValue = res[[1]], InverseMatrix = res[[2]])
}

# Short versions: DO NOT depend on P
.Calculate_Criterion_Short_NO_P <- function(func_name, Y, X, Inverse) {
  .source_julia_backend_once(quiet = TRUE)
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        .Y_arg(Y),
                        as.matrix(X),
                        as.matrix(Inverse))
}

# Short versions: DO depend on P
.Calculate_Criterion_Short_WITH_P <- function(func_name, Y, X, Inverse, P = ncol(X)) {
  .source_julia_backend_once(quiet = TRUE)
  JuliaCall::julia_call(paste0(func_name, "_short"),
                        .Y_arg(Y),
                        as.matrix(X),
                        as.matrix(Inverse),
                        as.integer(P))
}

# =========================
# Public API (exports)
# =========================

# -------- Full: NO-P criteria --------
#' @rdname information_criteria
#' @export
Calculate_AIC   <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_AIC",   Y, X)
#' @rdname information_criteria
#' @export
Calculate_AICc  <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_AIC_c", Y, X)
#' @rdname information_criteria
#' @export
Calculate_AttIC <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_AttIC", Y, X)
#' @rdname information_criteria
#' @export
Calculate_SIC   <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_SIC",   Y, X)
#' @rdname information_criteria
#' @export
Calculate_BIC   <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_BIC",   Y, X)
#' @rdname information_criteria
#' @export
Calculate_CAIC  <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_CAIC",  Y, X)
#' @rdname information_criteria
#' @export
Calculate_CAICF <- function(Y, X) .Calculate_Criterion_NO_P("Calculate_CAICF", Y, X)

# -------- Full: WITH-P criteria --------
#' @rdname information_criteria
#' @export
Calculate_EBIC  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_EBIC", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC2  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC2", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC3  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC3", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC4  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC4", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC5  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC5", Y, X, P)
#' @rdname information_criteria
#' @export
Calculate_GIC6  <- function(Y, X, P = ncol(X)) .Calculate_Criterion_WITH_P("Calculate_GIC6", Y, X, P)

# -------- Short: NO-P criteria --------
#' @rdname information_criteria
#' @export
Calculate_AIC_Short   <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_AIC",   Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_AICc_Short  <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_AIC_c", Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_AttIC_Short <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_AttIC", Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_SIC_Short   <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_SIC",   Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_BIC_Short   <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_BIC",   Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_CAIC_Short  <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_CAIC",  Y, X, Inverse)
#' @rdname information_criteria
#' @export
Calculate_CAICF_Short <- function(Y, X, Inverse) .Calculate_Criterion_Short_NO_P("Calculate_CAICF", Y, X, Inverse)

# -------- Short: WITH-P criteria --------
#' @rdname information_criteria
#' @export
Calculate_EBIC_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_EBIC", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC2_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC2", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC3_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC3", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC4_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC4", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC5_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC5", Y, X, Inverse, P)
#' @rdname information_criteria
#' @export
Calculate_GIC6_Short <- function(Y, X, Inverse, P = ncol(X)) .Calculate_Criterion_Short_WITH_P("Calculate_GIC6", Y, X, Inverse, P)
