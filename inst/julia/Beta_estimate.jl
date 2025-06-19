##############################################################################
#  Beta_estimate.jl  (intercept-aware, 2025-06-20)
##############################################################################
using LinearAlgebra, Statistics
using DataFrames, StatsModels, GLM
using StatsBase
import GLM: LogitLink, LogLink, IdentityLink, InverseLink

"""
    Beta_estimate(Y, X; family=:auto, link=:auto,
                  n_trials=nothing, add_intercept=true)

Estimate regression coefficients for many response types.

If `add_intercept = true` (default) a leading column of 1s is prepended to
`X` and the returned vector/matrix includes β₀ as the first row.
"""
function Beta_estimate(
        Y, X;
        family        ::Union{String,Symbol} = :auto,
        link          ::Union{Link,Symbol}   = :auto,
        n_trials      ::Union{Nothing,Int}   = nothing,
        add_intercept ::Bool                 = true)

    # -- 0. Optionally prepend intercept column -------------------------------
    if add_intercept
        X = hcat(ones(size(X, 1)), X)   # (n × (p+1))
    end
    n, p = size(X)

    # -- 0b. Flatten n×1 matrices --------------------------------------------
    if isa(Y, AbstractMatrix) && size(Y, 2) == 1
        Y = vec(Y)
    end

    # -- 1. Detect / normalise family token ----------------------------------
    if family === :auto
        family = if isa(Y, AbstractMatrix)
                     :MultivariateNormal
                 elseif eltype(Y) <: Bool || all(y -> y in (0,1), Y)
                     :Bernoulli
                 elseif eltype(Y) <: Integer && all(Y .>= 0)
                     length(unique(Y)) > 2 ? :Multinomial :
                     isnothing(n_trials) ? :Poisson : :Binomial
                 else
                     :Linear
                 end
    else
        family = Symbol(family)
    end

    # -- 2. Linear link → OLS -------------------------------------------------
    if family == :Linear
        return Vector((X'X) \ (X'Y))          # length p (incl. intercept if added)
    end

    # -- 3. Multivariate Normal → column-wise OLS -----------------------------
    if family == :MultivariateNormal
        return Matrix((X'X) \ (X'Y))          # (p × q) incl. intercept row if added
    end

    # -- 4. Multinomial (one-vs-rest logistic) --------------------------------
    if family == :Multinomial
        classes  = sort!(collect(unique(Y)))
        k        = length(classes)
        beta_hat = zeros(p, k-1)
        for (j, c) in enumerate(classes[1:end-1])
            y_bin = Y .== c
            df    = DataFrame(X, Symbol.("x",1:p));  df.y = y_bin
            rhs   = join(Symbol.("x",1:p), " + ")
            fmla  = @eval @formula(y ~ $(Meta.parse(rhs)))   # intercept kept
            mdl   = glm(fmla, df, Binomial(), LogitLink())
            beta_hat[:, j] = coef(mdl)                      # keep all coefs
        end
        return beta_hat                                     # p × (k-1)
    end

    # -- 5. Uni-variate GLM families -----------------------------------------
    if isa(Y, AbstractMatrix)
        error("Family $family expects a vector response; got a matrix.")
    end

    glm_family = if family == :Bernoulli
                     Binomial()
                 elseif family == :Binomial
                     Binomial(isnothing(n_trials) ? 1 : n_trials)
                 elseif family == :Poisson
                     Poisson()
                 elseif family == :Gamma
                     Gamma()
                 elseif family == :Exponential
                     Gamma()
                 else
                     error("Unsupported family token: $family")
                 end

    # Canonical link if not supplied
    if link === :auto
        link = glm_family isa Binomial  ? LogitLink()   :
               glm_family isa Poisson   ? LogLink()     :
               glm_family isa Gamma     ? InverseLink() :
                                          IdentityLink()
    end

    df   = DataFrame(X, Symbol.("x",1:p)); df.y = Y
    fmla = @eval @formula(y ~ $(Meta.parse(join(Symbol.("x",1:p), " + "))))
    mdl  = glm(fmla, df, glm_family, link)
    return coef(mdl)                         # *KEEP* intercept (β₀) as first entry
end
