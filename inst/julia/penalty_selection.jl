function softmax(z)
    exp_z = exp.(z)  # Element-wise exponentiation
    return exp_z ./ sum(exp_z)  # Normalize by the sum of exponentials
end


function huber_loss(r::Vector{Float64}, δ::Float64)
    loss = map(x -> abs(x) ≤ δ ? x^2 : δ * (2 * abs(x) - δ), r)
    return sum(loss)
end


# AIC Functions
function Calculate_AIC(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute AIC
    AIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T

    return (AIC, Inverse)
end

function Calculate_AIC_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var)) / (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute AIC
    AIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T

    return AIC
end

# AICc Functions
function Calculate_AIC_c(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Check if the denominator (T - K - 2) is valid
    if T - K - 2 <= 0
        return (-999.0, zeros(0, 0))  # Return special values for invalid models
    end

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute AICc
    AICc = (Y' * Hat_matrix * Y) / T - ((K + 1) * sample_variance) / (T - K - 2)

    return (AICc, Inverse)
end

function Calculate_AIC_c_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Check if the denominator (T - K - 2) is valid
    if T - K - 2 <= 0
        return -999.0  # Return a special value for invalid models
    end

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute AICc
    AICc = (Y' * Hat_matrix * Y) / T - ((K + 1) * sample_variance) / (T - K - 2)

    return AICc
end

# Attention Functions
function Calculate_AttIC(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end
  
    # Compute AttIC
    AttIC = Y' * softmax(Hat_matrix*Y) -   (K * sample_variance) / sqrt(T)


    return (AttIC, Inverse)
end

function Calculate_AttIC_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end
    # Compute AttIC
    AttIC = Y' * softmax(Hat_matrix*Y) - (K * sample_variance) / sqrt(T)

    return AttIC
end


# SIC Functions
function Calculate_SIC(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)
    T, K = size(X, 1), size(X, 2)
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'
    residuals = Y - Hat_matrix * Y

    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    SIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / sqrt(T)
    return (SIC, Inverse)
end

function Calculate_SIC_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute SIC
    SIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / sqrt(T)

    return SIC
end

# BIC Functions
function Calculate_BIC(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute BIC
    BIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(T)

    return (BIC, Inverse)
end

function Calculate_BIC_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)


    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute BIC
    BIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(T)

    return BIC
end

# CAIC Functions
function Calculate_CAIC(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute CAIC
    CAIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * (1 + log(T))

    return (CAIC, Inverse)
end

function Calculate_CAIC_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute CAIC
    CAIC = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * (1 + log(T))

    return CAIC
end


# ICOMP Functions
function Calculate_ICOMP(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute ICOMP
    ICOMP = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / sqrt(T) -
    (K * log(abs(tr(sample_variance * Inverse) / K)) - logabsdet(sample_variance *Inverse)[1])
    
    return (ICOMP, Inverse)
end

function Calculate_ICOMP_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute ICOMP
    ICOMP = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / sqrt(T) -
    ( K * log(abs(tr(sample_variance * Inverse) / K)) - logabsdet(sample_variance *Inverse)[1])
    
    return ICOMP
end


# ICOMPIFIM Functions
function Calculate_ICOMPIFIM(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end
    # Compute ICOMP
    ICOMPIFIM = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(T) +
    sample_variance * (logabsdet(Inverse)[1]  + log((2*sample_variance)/T))/ T  - 
    sample_variance * K * log(((abs(tr(Inverse)) + (2*sample_variance)/T)/K)) / T

    return (ICOMPIFIM, Inverse)
end

function Calculate_ICOMPIFIM_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute ICOMPIFIM
    ICOMPIFIM = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(T) +
    sample_variance * (logabsdet(Inverse)[1]  + log((2*sample_variance)/T))/ T  - 
    sample_variance * K * log(((abs(tr(Inverse)) + (2*sample_variance)/T)/K)) / T

    return ICOMPIFIM
end


# CAICF Functions
function Calculate_CAICF(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute CAICF
    CAICF = (Y' * Hat_matrix * Y) / T - (K * sample_variance * (1 + log(T))) / T -
            sample_variance * log(det((X' * X) / sample_variance)) / T

    return (CAICF, Inverse)
end

function Calculate_CAICF_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute CAICF
    CAICF = (Y' * Hat_matrix * Y) / T - (K * sample_variance * (1 + log(T))) / T -
            sample_variance * log(det((X' * X) / sample_variance)) / T

    return CAICF
end

# GIC2 Functions
function Calculate_GIC2(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC2
    GIC2 = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * K^(1/3)

    return (GIC2, Inverse)
end

function Calculate_GIC2_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC2
    GIC2 = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * K^(1/3)

    return GIC2
end

# GIC3 Functions
function Calculate_GIC3(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC3
    GIC3 = (Y' * Hat_matrix * Y) / T - (2 * K * sample_variance) / T * log(K)

    return (GIC3, Inverse)
end

function Calculate_GIC3_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC3
    GIC3 = (Y' * Hat_matrix * Y) / T - (2 * K * sample_variance) / T * log(K)

    return GIC3
end

# GIC4 Functions
function Calculate_GIC4(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC4
    GIC4 = (Y' * Hat_matrix * Y) / T - (2 * K * sample_variance) / T * (log(K) + log(log(K)))

    return (GIC4, Inverse)
end

function Calculate_GIC4_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC4
    GIC4 = (Y' * Hat_matrix * Y) / T - (2 * K * sample_variance) / T * (log(K) + log(log(K)))

    return GIC4
end

# GIC5 Functions
function Calculate_GIC5(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X' * X)
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC5
    GIC5 = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(log(T)) * log(K)

    return (GIC5, Inverse)
end

function Calculate_GIC5_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC5
    GIC5 = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(log(T)) * log(K)

    return GIC5
end

# GIC6 Functions
function Calculate_GIC6(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute inverse and hat matrix
    Inverse = inv(X'*X)
    Hat_matrix = X*Inverse*X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    GIC6 = (Y'*Hat_matrix*Y)/ T - (K*sample_variance)/T * log(T) * log(K)

    return (GIC6, Inverse)
end


function Calculate_GIC6_short(Y::Union{AbstractVector, AbstractMatrix}, X::AbstractMatrix, Inverse::AbstractMatrix, Huber::Bool = false)

    # Get dimensions
    T, K = size(X, 1), size(X, 2)

    # Compute hat matrix
    Hat_matrix = X * Inverse * X'

    # Compute residuals and sample variance
    residuals = Y - Hat_matrix * Y
    if Huber
        estimated_var = (residuals' * residuals) / (T-K)
        sample_variance = huber_loss(residuals, 0.8 * sqrt(estimated_var))/ (T-K)
    else
        sample_variance = (residuals' * residuals) / (T-K)
    end

    # Compute GIC5
    GIC6 = (Y' * Hat_matrix * Y) / T - (K * sample_variance) / T * log(T) * log(K)

    return GIC6
end







