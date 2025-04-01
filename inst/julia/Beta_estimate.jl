function Beta_estimate(Y, X, Inverse)
    # Inputs:
    # Y: Outcome vector
    # X: Design matrix
    # T = size(X, 1)  # Number of observations
    # K = size(X, 2)  # Number of predictors

    (T, K) = (size(X, 1), size(X, 2))
    Beta_est = Inverse * X' * Y

    return (Beta_est) 

end






