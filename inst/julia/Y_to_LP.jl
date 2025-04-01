using StatsBase
using Distributions

# Define the function to calculate xβ from Y for different families
function Y_to_lp(Y, family::String;
                  shape::Union{Nothing, Float64} = nothing, 
                  n_trials::Union{Nothing, Int64} = nothing, 
                  std::Union{Nothing, Float64} = nothing)

"""
Transform observed responses `Y` into linear predictors using canonical link functions for various GLM families.

# Arguments
- `Y::Vector{Float64}`: Observed response vector.
- `family::String`: Distribution family. Supported options:
  - `"Bernoulli"`: Binary outcomes (0/1)
  - `"Binomial"`: Count outcomes with trials
  - `"Normal"`: Continuous outcomes
  - `"Poisson"`: Count outcomes
  - `"Gamma"`: Positive continuous outcomes
  - `"Exponential"`: Positive continuous outcomes

# Keyword Arguments
- `shape::Union{Nothing, Float64}`: Shape parameter for Gamma family (default: 1.0).
- `n_trials::Union{Nothing, Int64}`: Required for Binomial family (number of trials).
- `std::Union{Nothing, Float64}`: Unused (kept for interface consistency).

# Returns
- `Vector{Float64}`: Transformed linear predictor.
"""

    # For Binomial: Logit link function (logistic transformation)
    if family == "Binomial"
        if isnothing(n_trials)
            error("n_trials must be specified for Binomial family.")
        end
        p = Y ./ n_trials
        p_adjusted = copy(p)
        p_adjusted[p .== 0] .+= 0.01  # Avoid log(0) error
        p_adjusted[p .== 1] .-= 0.01  # Avoid log(1) error
        return log.(p_adjusted ./ (1 .- p_adjusted))  # Logistic transformation

    # For Bernoulli: Logit link function (logistic transformation)
    elseif family == "Bernoulli"
        Y_adjusted = copy(Y)
        Y_adjusted[Y .== 0.0] .+= 0.01  # Avoid log(0) error
        Y_adjusted[Y .== 1.0] .-= 0.01  # Avoid log(1) error
        return log.(Y_adjusted ./ (1 .- Y_adjusted))  # Logistic transformation

    # For Normal: Identity link function
    elseif family == "Normal"
        return Y  # Linear predictor is equal to Y for Normal distribution

    # For Poisson: Log link function
    elseif family == "Poisson"
        return log.(Y .+ 0.1)  # Avoid log(0) by adding a small constant

    # For Gamma: Log link function
    elseif family == "Gamma"
        if isnothing(shape)
            shape = 1.0  # Default shape parameter if not provided
        end
        return log.(Y .+ 0.1)  # Log link function for Gamma

    # For Exponential: Log link function
    elseif family == "Exponential"
        return log.(Y .+ 0.1)  # Log link function for Exponential

        # For Normal: Identity link function
    elseif family == "MultivariateNormal"
        return Y  # Linear predictor is equal to Y for Normal distribution


    else
        error("Unsupported family: $family")  # Error if the family is unsupported
    end
end

