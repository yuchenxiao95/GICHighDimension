# Skip Julia-dependent tests when Julia is not available.
skip_if_no_julia <- function() {
  skip_on_cran()
  skip_if_not_installed("JuliaCall")
  ok <- tryCatch({
    JuliaCall::julia_setup(installJulia = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) skip("Julia is not available")
}
