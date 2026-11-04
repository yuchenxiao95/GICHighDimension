# Package index

## Variable selection

Search over variable subsets to maximize an information criterion. This
is how you will usually use the package.

- [`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md)
  : GIC-Based Variable Selection
- [`GICSelectionBoltzmann()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelectionBoltzmann.md)
  : GIC-Based Variable Selection Using Boltzmann Machine

## Information criteria

AIC, BIC, EBIC, GIC2–GIC6 and related criteria. Pass the name of a
criterion (and of its `_short` version) to
[`GICSelection()`](https://yuchenxiao95.github.io/GICHighDimension/reference/GICSelection.md),
or call these functions directly to score a single model.

- [`Calculate_AIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_AICc()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_AttIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_SIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_BIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_CAIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_CAICF()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_EBIC()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC2()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC3()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC4()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC5()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC6()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_AIC_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_AICc_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_AttIC_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_SIC_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_BIC_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_CAIC_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_CAICF_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_EBIC_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC2_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC3_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC4_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC5_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  [`Calculate_GIC6_Short()`](https://yuchenxiao95.github.io/GICHighDimension/reference/information_criteria.md)
  : Information Criterion Calculations via Julia

## Simulation and link transforms

Simulate responses from a linear predictor, map responses to the
linear-predictor scale, and generate sparse coefficient vectors. All
common GLM families are supported.

- [`LP_to_Y()`](https://yuchenxiao95.github.io/GICHighDimension/reference/LP_to_Y.md)
  : Transform Linear Predictor to Response
- [`Y_to_LP()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Y_to_LP.md)
  : Transform Outcome to Linear Predictor
- [`Generate_Beta()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Generate_Beta.md)
  : Generate Sparse Coefficient Vector

## Estimation

Estimate the coefficients of a selected model.

- [`Beta_estimate()`](https://yuchenxiao95.github.io/GICHighDimension/reference/Beta_estimate.md)
  : Coefficient Estimation via Julia

## Julia setup

Connect R to Julia and install the Julia packages the package needs.

- [`setup_julia()`](https://yuchenxiao95.github.io/GICHighDimension/reference/setup_julia.md)
  : Initialize 'Julia' Environment
- [`install_julia_packages()`](https://yuchenxiao95.github.io/GICHighDimension/reference/install_julia_packages.md)
  : Install Required 'Julia' Packages
- [`install_julia_dependencies()`](https://yuchenxiao95.github.io/GICHighDimension/reference/install_julia_dependencies.md)
  : Install Julia and Required Packages
