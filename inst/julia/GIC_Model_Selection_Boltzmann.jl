using Random 

function GIC_Variable_Selection_Boltzmann(
    X::AbstractMatrix, 
    Y::Union{AbstractVector, AbstractMatrix},
    Init_Columns::AbstractVector{Int}, 
    Calculate_GIC, 
    Calculate_GIC_short;
    T::Float64=0.2,  
    Nsim::Int64=2
)
    """
    Perform variable selection using the Generalized Information Criterion (GIC) with a Boltzmann-like 
    simulated annealing approach. The function efficiently searches through subsets of features 
    to minimize the GIC while employing probabilistic updates based on the Boltzmann distribution.

    ### Arguments:
    - `X::AbstractMatrix`: An `n x p` matrix of input features, where `n` is the number of samples and `p` is the number of features.
    - `Y::AbstractVector`: A vector of target values with length `n`, corresponding to the rows of `X`.
    - `Init_Columns::AbstractVector{Int}`: A vector of initial feature indices that are assumed to have non-zero coefficients.
    - `Calculate_GIC`: A function that computes the GIC for a given feature subset. It expects the outcome vector `Y` and the feature matrix for the subset.
    - `Calculate_GIC_short`: A function to calculate the GIC using incremental matrix updates, which avoids recalculating the full inverse each time.
    - `T::Float64`: The temperature parameter that controls the randomness of updates. The default value is `0.2`, with lower values leading to fewer updates (more deterministic).
    - `debug::Bool`: A flag to print debugging information for each iteration. The default is `false`.
    - `Nsim::Int64`: The number of times to repeat the feature subset list in parallel. The default is `2`.

    ### Returns:
    - `GIC_list::Vector{Float64}`: A vector of GIC values for each candidate subset of features.
    - `GIC_coeff::Vector{Vector{Int}}`: A vector of feature subsets (sets of indices) corresponding to the computed GIC values.
    - `prob_distribution::Vector{Float64}`: The normalized probability distribution over the feature subsets (not currently returned, but can be included for future extension).

    """

    # Input validation
    m, n = size(X)
    
    sets = collect(1:size(X,2))  # Get the feature indices
    shuffle!(sets)
    repeated_list = repeat(sets, Nsim)  # Repeat the list multiple times

    GIC_coef_sets =  Init_Columns
    GIC_c, M_inv = Calculate_GIC(Y, X[:, GIC_coef_sets])
    
    # Prepare outputs
    GIC_list = zeros(length(repeated_list))
    GIC_coeff = [Int[] for _ in 1:length(repeated_list)]
    w = 1

    # Parallelized loop
    @sync for z in repeated_list
        @async begin

            local GIC_i, GIC_coef_sets_temp, A_inv, delta

            if z in GIC_coef_sets  # Test removing a feature

                index = findfirst(==(z), GIC_coef_sets)
                GIC_coef_sets_temp = deleteat!(copy(GIC_coef_sets), index)
                X_subsets = X[:, GIC_coef_sets_temp]

                # Update inverse using block matrix manipulations
                A_hat = M_inv[setdiff(1:end, index), setdiff(1:end, index)]
                B_hat = M_inv[setdiff(1:end, index), index]
                C_hat = M_inv[index, setdiff(1:end, index)]'
                D_hat = only(M_inv[index, index])
                A_inv = A_hat - ((B_hat / D_hat) * C_hat)

                A_inv = inv(X_subsets' * X_subsets)
                GIC_i = Calculate_GIC_short(Y, X_subsets, A_inv)

                delta = tr(GIC_c) -  tr(GIC_i)
              
                if Float64(rand(Bernoulli(logistic(- delta/T)))) == 1  # Accept update with probability
                    GIC_c = GIC_i
                    GIC_coef_sets = GIC_coef_sets_temp
                    M_inv = A_inv
                    current_X = X_subsets    
                end
                GIC_list[w] = tr(GIC_c)
                GIC_coeff[w] = copy(GIC_coef_sets)

            elseif z ∉ GIC_coef_sets 
                GIC_coef_sets_temp = prepend!(copy(GIC_coef_sets), z)
                X_subsets = X[:, GIC_coef_sets_temp]

                index = findfirst(==(z), GIC_coef_sets_temp)
                Xsquare = X_subsets' * X_subsets

                A_hat = Xsquare[setdiff(1:end, index), setdiff(1:end, index)]
                B_hat = Xsquare[setdiff(1:end, index), index]
                C_hat = Xsquare[index, setdiff(1:end, index)]'
                D_hat = only(Xsquare[index, index])

                topleft = M_inv + M_inv * B_hat * inv(D_hat - C_hat * M_inv * B_hat) * C_hat * M_inv
                topright = -M_inv * B_hat * inv(D_hat - C_hat * M_inv * B_hat)
                bottomleft = -inv(D_hat - C_hat * M_inv * B_hat) * C_hat * M_inv
                bottomright = inv(D_hat - C_hat * M_inv * B_hat)
                A_inv = [
                    topleft           topright;
                    bottomleft        bottomright
                ]

                GIC_i = Calculate_GIC_short(Y, X_subsets, A_inv)
                
                delta = GIC_i - GIC_c

                if Float64(rand(Bernoulli(logistic(- delta/T)))) == 0  # Accept update with probability
                    GIC_c = GIC_i
                    GIC_coef_sets = GIC_coef_sets_temp
                    M_inv = A_inv
                    current_X = X_subsets 
                end

                GIC_list[w] = tr(GIC_c)
                GIC_coeff[w] = copy(GIC_coef_sets)
            end
            # Increment w after each iteration
            w += 1
        end
    end
    
    return (GIC_list, GIC_coeff)
end

