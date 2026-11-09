# Information Criterion Calculations via Julia

Compute information criteria using efficient 'Julia' backends via the
'JuliaCall' package. These functions are useful for variable selection
and model comparison in high-dimensional settings. Each function returns
either the computed criterion value, or a list with additional data.

## Usage

``` r
Calculate_AIC(Y, X)

Calculate_AICc(Y, X)

Calculate_AttIC(Y, X)

Calculate_SIC(Y, X)

Calculate_BIC(Y, X)

Calculate_CAIC(Y, X)

Calculate_CAICF(Y, X)

Calculate_EBIC(Y, X, P = ncol(X))

Calculate_GIC2(Y, X, P = ncol(X))

Calculate_GIC3(Y, X, P = ncol(X))

Calculate_GIC4(Y, X, P = ncol(X))

Calculate_GIC5(Y, X, P = ncol(X))

Calculate_GIC6(Y, X, P = ncol(X))
```

## Arguments

- Y:

  A numeric response: a vector of length n, or an n x q matrix for a
  multivariate response (CAICF supports vector responses only).

- X:

  A numeric design matrix (n × p).

- P:

  (only for EBIC and GIC2–GIC6) integer total number of candidate
  predictors. Defaults to `ncol(X)`.

## Value

A named list with components:

- CriterionValue:

  Numeric scalar of the selected information criterion.

- InverseMatrix:

  Matrix inverse used in computation (if applicable).

## Details

Every criterion has the form \\Y^\top H Y / n - p(n, K)\\, where \\H\\
is the hat matrix of the \\K\\ selected columns and \\p(n, K)\\ is the
penalty (Xiao and Walker, 2026). With \\\hat\sigma^2 = \mathrm{RSS}/(n -
K)\\ and \\p\\ the number of candidate predictors, the supported
criteria are:

- AIC, Akaike information criterion (Akaike, 1974):
  \\2K\hat\sigma^2/n\\.

- AICc, AIC corrected for small samples (Hurvich and Tsai, 1989):
  \\(K+1)\hat\sigma^2/(n-K-2)\\.

- BIC, Bayesian information criterion (Schwarz, 1978):
  \\K\hat\sigma^2\log(n)/n\\.

- CAIC, consistent AIC (Bozdogan, 1987): \\K\hat\sigma^2(\log n +
  1)/n\\.

- CAICF, consistent AIC with Fisher information (Bozdogan, 1987): the
  CAIC penalty plus \\\hat\sigma^2 \log\det(X^\top X/\hat\sigma^2)/n\\.

- SIC, square-root information criterion (Shao, 1997; Xiao and Walker,
  2026): \\K\hat\sigma^2/\sqrt{n}\\.

- GIC2–GIC6, generalized information criteria for high dimensions (Kim,
  Kwon and Choi, 2012), with penalties \\K\hat\sigma^2 p^{1/3}/n\\,
  \\2K\hat\sigma^2\log(p)/n\\, \\2K\hat\sigma^2(\log p + \log\log
  p)/n\\, \\K\hat\sigma^2\log\log(n)\log(p)/n\\ and
  \\K\hat\sigma^2\log(n)\log(p)/n\\.

- EBIC, extended BIC (Chen and Chen, 2008): \\K\hat\sigma^2\\\log n +
  \gamma\log\binom{P}{K}\\/n\\, with \\\gamma = 0.6\\ by default.

- AttIC, an attention-weighted criterion specific to this package: the
  fit term is replaced by \\Y^\top \mathrm{softmax}(HY)\\ and the
  penalty is that of SIC.

## References

Akaike, H. (1974). A new look at the statistical model identification.
*IEEE Transactions on Automatic Control*, 19(6), 716–723.
[doi:10.1109/TAC.1974.1100705](https://doi.org/10.1109/TAC.1974.1100705)

Bozdogan, H. (1987). Model selection and Akaike's information criterion
(AIC): The general theory and its analytical extensions.
*Psychometrika*, 52(3), 345–370.
[doi:10.1007/BF02294361](https://doi.org/10.1007/BF02294361)

Chen, J. and Chen, Z. (2008). Extended Bayesian information criteria for
model selection with large model spaces. *Biometrika*, 95(3), 759–771.
[doi:10.1093/biomet/asn034](https://doi.org/10.1093/biomet/asn034)

Hurvich, C. M. and Tsai, C.-L. (1989). Regression and time series model
selection in small samples. *Biometrika*, 76(2), 297–307.
[doi:10.1093/biomet/76.2.297](https://doi.org/10.1093/biomet/76.2.297)

Kim, Y., Kwon, S. and Choi, H. (2012). Consistent model selection
criteria on high dimensions. *Journal of Machine Learning Research*, 13,
1037–1057. <https://jmlr.org/papers/v13/kim12a.html>

Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
Statistics*, 6(2), 461–464.
[doi:10.1214/aos/1176344136](https://doi.org/10.1214/aos/1176344136)

Shao, J. (1997). An asymptotic theory for linear model selection.
*Statistica Sinica*, 7(2), 221–264.
<https://www3.stat.sinica.edu.tw/statistica/j7n2/j7n21/j7n21.htm>

Xiao, Y. and Walker, S. (2026). Fast variable selection under \\\ell_0\\
regularization in high-dimensions. *Statistics and Computing*, 36(3),
100.
[doi:10.1007/s11222-026-10853-5](https://doi.org/10.1007/s11222-026-10853-5)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && requireNamespace("JuliaCall", quietly = TRUE)) {
  setup_julia(install_julia = FALSE)
  X <- matrix(rnorm(100), ncol = 5)
  Y <- rnorm(20)
  out1 <- Calculate_AIC(Y, X)
  out2 <- Calculate_EBIC(Y, X)        # uses P = ncol(X) by default
}
} # }
```
