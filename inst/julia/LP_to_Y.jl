using StatsBase
using Distributions


function LP_to_Y(X::Matrix{Float64}, true_beta::Union{Vector{Float64}, Matrix{Float64}}, 
        family::String="Normal";
        n_trials::Union{Nothing, Int64}=nothing,
        std::Union{Nothing, Float64}=nothing,
        shape::Union{Nothing, Float64}=nothing,
        cov_matrix::Union{Nothing, Matrix{Float64}}=nothing,
        n_categories::Union{Nothing, Int64}=nothing)

    # Calculate linear predictor
    linear_predictor = X * true_beta

    if family == "Bernoulli"
        mu = 1.0 ./ (1.0 .+ exp.(-linear_predictor))
        return Float64.(rand.(Bernoulli.(mu)))

    elseif family == "Binomial"
        isnothing(n_trials) && error("n_trials must be specified for Binomial family")
        mu = 1.0 ./ (1.0 .+ exp.(-linear_predictor))
        return rand.(Binomial.(n_trials, mu))

    elseif family == "Normal"
        isnothing(std) && (std = 1.0)
        return linear_predictor .+ rand(Normal(0, std), size(linear_predictor)...)

    elseif family == "Poisson"
        max_value = 30.0
        linear_predictor = clamp.(linear_predictor, -max_value, max_value)
        return rand.(Poisson.(exp.(linear_predictor)))

    elseif family == "Gamma"
        isnothing(shape) && (shape = 1.0)
        return rand.(Gamma.(shape, exp.(linear_predictor)))

    elseif family == "Exponential"
        max_value = 30.0
        linear_predictor = clamp.(linear_predictor, -max_value, max_value)
        return rand.(Exponential.(exp.(linear_predictor)))

    elseif family == "MultivariateNormal"
        isnothing(cov_matrix) && error("cov_matrix must be specified for MultivariateNormal")
        !isposdef(cov_matrix) && error("Covariance matrix must be positive definite")
        return [rand(MvNormal(row, cov_matrix)) for row in eachrow(linear_predictor)]

    elseif family == "Multinomial"
        isnothing(n_categories) && error("n_categories must be specified for Multinomial")
        n_categories < 2 && error("n_categories must be ≥ 2")
        size(true_beta, 2) == n_categories - 1 || error("true_beta must have size (n_features × (n_categories-1))")
        return [rand(Categorical(softmax([row; 0]))) for row in eachrow(linear_predictor)]

    else
        error("Unsupported family: $family")
    end
end

function softmax(z)
    exp_z = exp.(z .- maximum(z))  # Numerically stable version
    return exp_z ./ sum(exp_z)
end