#' @title Install Julia and Required Packages
#' @description Installs Julia and required Julia packages (e.g., 'Distributions', 'LinearAlgebra').
#' Only call this function manually.
#' @export
install_julia_dependencies <- function() {
  JuliaCall::install_julia()
  JuliaCall::julia_install_package_if_needed("Distributions")
  JuliaCall::julia_install_package_if_needed("LinearAlgebra")
  JuliaCall::julia_install_package_if_needed("Random")
  JuliaCall::julia_install_package_if_needed("Statistics")
}
