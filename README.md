# GICHighDimension

[![R-CMD-check](https://github.com/yuchenxiao95/GICHighDimension/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yuchenxiao95/GICHighDimension/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**'GICHighDimension'** is an R package for high-dimensional variable selection using the generalized information criterion ('GIC') and Hopfield network optimization. Performance-critical steps are accelerated using 'Julia', while the R interface ensures ease of use for modeling generalized linear models ('GLMs').

---

## 📦 Installation

### ✅ Requirements
- 'R' (≥ 4.0.0)
- 'Julia' (≥ 1.6)
- R package: 'JuliaCall'

### 🔧 Install from GitHub

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("yuchenxiao95/GICHighDimension")
