using Random
using LinearAlgebra

"""
    GIC_Variable_Selection(X, Y, Init_Columns, Calculate_GIC, Calculate_GIC_short; Nsim=1)

Perform variable selection using Generalized Information Criterion (GIC) with iterative feature 
addition/removal. Optimized for both single-response (vector `Y`) and multi-response (matrix `Y`) cases.

# Arguments
- `X::AbstractMatrix`: Design matrix of size `(n, p)` where `n` = samples, `p` = features.
- `Y::Union{AbstractVector, AbstractMatrix}`: Response vector (`n × 1`) or matrix (`n × q` for `q` responses).
- `Init_Columns::AbstractVector{Int}`: Initial column indices (features) to include in the model.
- `Calculate_GIC`: Function to compute full GIC and inverse covariance matrix. Signature `(Y, X_subset) -> (GIC, M_inv)`.
- `Calculate_GIC_short`: Function to compute GIC efficiently using pre-calculated `M_inv`. Signature `(Y, X_subset, M_inv) -> GIC`.
- `Nsim::Int64=1`: Number of Monte Carlo simulations for stochastic search (default: deterministic).

# Returns
- `GIC_list::Vector{Float64}`: Trace of GIC values at each iteration.
- `GIC_coeff::Vector{Vector{Int}}`: Selected feature indices at each iteration.

# Notes
- Uses Sherman-Morrison-Woodbury for efficient inverse updates when adding/removing features.
- For multi-response `Y`, `Calculate_GIC` functions should return GIC as a matrix (e.g., trace for scalar comparison).
"""
function GIC_Variable_Selection(
    X::AbstractMatrix, 
    Y::Union{AbstractVector, AbstractMatrix}, 
    Init_Columns::AbstractVector{Int}, 
    Calculate_GIC, 
    Calculate_GIC_short;
    Nsim::Int64 = 1
)
    # --- Input Validation ---
    m, n = size(X)
    @assert length(Init_Columns) ≤ n "Initial columns exceed design matrix size."
    @assert m == size(Y, 1) "Sample size mismatch between X and Y."

    # --- Initialization ---
    sets = collect(1:n)  # All feature indices
    repeated_list = repeat(sets, Nsim)  # Feature sequence for iteration

    # Initial GIC calculation and inverse covariance
    GIC_coef_sets = Init_Columns
    GIC_c, M_inv = Calculate_GIC(Y, X[:, GIC_coef_sets])
    current_X = X[:, GIC_coef_sets]

    # --- Output Storage ---
    GIC_list = zeros(length(repeated_list))
    GIC_coeff = [Int[] for _ in 1:length(repeated_list)]
    w = 1  # Iteration counter

    # --- Main Iteration Loop ---

    for z in repeated_list
        if z in GIC_coef_sets  
            # Case 1: Test removing feature `z`
            index = findfirst(==(z), GIC_coef_sets)
            GIC_coef_sets_temp = deleteat!(copy(GIC_coef_sets), index)
            X_subsets = X[:, GIC_coef_sets_temp]

            # Block matrix update for inverse covariance (efficient removal)
            A_hat = M_inv[setdiff(1:end, index), setdiff(1:end, index)]
            B_hat = M_inv[setdiff(1:end, index), index]
            C_hat = M_inv[index, setdiff(1:end, index)]'
            D_hat = only(M_inv[index, index])
            A_inv = A_hat - ((B_hat / D_hat) * C_hat)  # Schur complement

            # GIC evaluation after removal
            GIC_i = Calculate_GIC_short(Y, X_subsets, A_inv)

            if tr(GIC_c) < tr(GIC_i)  # Keep change if GIC improves
                GIC_c = GIC_i
                GIC_coef_sets = GIC_coef_sets_temp
                M_inv = A_inv
                current_X = X_subsets 
            end
            GIC_list[w] = tr(GIC_c)
            GIC_coeff[w] = copy(GIC_coef_sets)

        elseif z ∉ GIC_coef_sets  
            # Case 2: Test adding feature `z`
            GIC_coef_sets_temp = prepend!(copy(GIC_coef_sets), z)
            X_subsets = X[:, GIC_coef_sets_temp]
            index = findfirst(==(z), GIC_coef_sets_temp)

            # Block matrix update for inverse covariance (efficient addition)
            Xsquare = X_subsets' * X_subsets
            A_hat = Xsquare[setdiff(1:end, index), setdiff(1:end, index)]
            B_hat = Xsquare[setdiff(1:end, index), index]
            C_hat = Xsquare[index, setdiff(1:end, index)]'
            D_hat = only(Xsquare[index, index])

            # Sherman-Morrison-Woodbury formula
            topleft = M_inv + M_inv * B_hat * inv(D_hat - C_hat * M_inv * B_hat) * C_hat * M_inv
            topright = -M_inv * B_hat * inv(D_hat - C_hat * M_inv * B_hat)
            bottomleft = -inv(D_hat - C_hat * M_inv * B_hat) * C_hat * M_inv
            bottomright = inv(D_hat - C_hat * M_inv * B_hat)
            A_inv = [topleft topright; bottomleft bottomright]

            # GIC evaluation after addition
            GIC_i = Calculate_GIC_short(Y, X_subsets, A_inv)

            if tr(GIC_c) < tr(GIC_i)  # Keep change if GIC improves
                GIC_c = GIC_i
                GIC_coef_sets = GIC_coef_sets_temp
                M_inv = A_inv
                current_X = X_subsets 
            end
            GIC_list[w] = tr(GIC_c)
            GIC_coeff[w] = copy(GIC_coef_sets)
        end
        w += 1
    end

    return (GIC_list, GIC_coeff)
end