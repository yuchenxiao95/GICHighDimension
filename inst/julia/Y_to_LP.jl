##############################################################################
#  Y_to_LP.jl  --  map observed responses to a working linear predictor
#
#  Applies the family's link function to the data (with a small adjustment so
#  the link is finite at 0 / 1), giving a response on the linear-predictor
#  scale that GICSelection can use. The links match LP_to_Y.jl:
#
#  Family              Transform
#  ------------------  ------------------------------------------------------
#  Normal              Y
#  MultivariateNormal  Y
#  Bernoulli           logit(Y), with 0 -> 0.01 and 1 -> 0.99
#  Binomial            n_trials * logit(Y / n_trials), same 0/1 adjustment
#  Poisson             log(Y + 0.1)
#  Gamma               log(Y + 0.1)
#  Exponential         log(Y + 0.1)
#  InverseGaussian     log(Y + 0.1)
#  NegativeBinomial    log(Y + 0.1)
#  Geometric           log(Y + 0.1)
#  Multinomial         n x (K-1) matrix of baseline-category logits
#                      log(p_ic / p_iK) of the smoothed one-hot vector
#                      p_i = (onehot(Y_i) + 0.01) / (1 + 0.01 K)
#  Cox                 deviance residuals of the null Cox model, from an
#                      n x 2 matrix [time status] (status 1 = event):
#                      M_i = status_i - Lambda0(time_i)   (Nelson-Aalen),
#                      D_i = sign(M_i) sqrt(-2 [M_i + status_i log(status_i - M_i)])
#                      (Therneau, Grambsch and Fleming, 1990)
##############################################################################

const _Y2LP_LOG_FAMILIES = ("Poisson", "Gamma", "Exponential", "InverseGaussian",
                            "NegativeBinomial", "Geometric")

function _adjusted_logit(p)
    q = Float64.(p)
    q[q .<= 0.0] .= 0.01            # avoid log(0)
    q[q .>= 1.0] .= 0.99            # avoid log(Inf)
    return log.(q ./ (1 .- q))
end

"""
    Y_to_LP(Y, family; shape = nothing, n_trials = nothing, std = nothing,
            theta = nothing, n_categories = nothing)

Transform observed responses `Y` to the linear-predictor scale with the
family's link function (see the table at the top of this file). `shape`,
`std` and `theta` are accepted for interface consistency with `LP_to_Y`.
"""
function Y_to_LP(Y, family::String;
                 shape::Union{Nothing, Real} = nothing,
                 n_trials::Union{Nothing, Integer} = nothing,
                 std::Union{Nothing, Real} = nothing,
                 theta::Union{Nothing, Real} = nothing,
                 n_categories::Union{Nothing, Integer} = nothing)

    if family == "Normal" || family == "MultivariateNormal"
        return Y

    elseif family == "Bernoulli"
        return _adjusted_logit(Y)

    elseif family == "Binomial"
        isnothing(n_trials) && error("n_trials must be specified for Binomial family.")
        # NOTE: scaled by n_trials as in the original implementation.
        return _adjusted_logit(Y ./ n_trials) .* n_trials

    elseif family in _Y2LP_LOG_FAMILIES
        any(Y .< 0) && error("Family $family requires non-negative responses.")
        return log.(Y .+ 0.1)

    elseif family == "Multinomial"
        cats = Int.(vec(Y))
        K = isnothing(n_categories) ? maximum(cats) : Int(n_categories)
        K < 2 && error("n_categories must be ≥ 2")
        (minimum(cats) < 1 || maximum(cats) > K) &&
            error("Multinomial responses must be integers in 1:n_categories")
        eps = 0.01
        lo = log(eps / (1 + eps * K))           # log of a smoothed 0
        hi = log((1 + eps) / (1 + eps * K))     # log of a smoothed 1
        eta = fill(lo, length(cats), K - 1)
        for (i, c) in enumerate(cats)
            base = c == K ? hi : lo             # log p_iK
            for j in 1:(K - 1)
                eta[i, j] = (j == c ? hi : lo) - base
            end
        end
        return eta

    elseif family == "Cox"
        return _cox_deviance_residuals(Y)

    else
        error("Unsupported family: $family")
    end
end

# Deviance residuals of the null Cox model. Lambda0 is the Nelson-Aalen
# cumulative hazard: sum over event times t_j <= t of d_j / r_j (d_j events,
# r_j at risk at t_j), with ties handled as in the Nelson-Aalen estimator.
function _cox_deviance_residuals(Y)
    (Y isa AbstractMatrix && size(Y, 2) == 2) ||
        error("Family Cox expects an n x 2 matrix [time status].")
    time = Float64.(Y[:, 1])
    status = Float64.(Y[:, 2])
    all(s -> s == 0 || s == 1, status) || error("status must be 0 (censored) or 1 (event).")
    n = length(time)
    ord = sortperm(time)
    Lambda = zeros(n)
    cum = 0.0
    i = 1
    while i <= n                              # walk through distinct times
        t = time[ord[i]]
        j = i
        d = 0.0
        while j <= n && time[ord[j]] == t
            d += status[ord[j]]
            j += 1
        end
        cum += d / (n - i + 1)                # at risk: everyone with time >= t
        for k in i:(j - 1)
            Lambda[ord[k]] = cum
        end
        i = j
    end
    M = status .- Lambda
    return sign.(M) .* sqrt.(-2 .* (M .+ [s == 1 ? log(1 - m) : 0.0 for (s, m) in zip(status, M)]))
end
