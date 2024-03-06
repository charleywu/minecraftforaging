import numpy as np
from scipy.special import expit, logit

def sigmoid(x):
    return expit(x)

def kernel(X1, X2, theta):
    """
    Isotropic squared exponential kernel.

    Args:
        X1: Array of m points (m x d).
        X2: Array of n points (n x d).
        theta: Kernel parameters [signal variance, length scale]

    Returns:
        (m x n) matrix
    """

    sqdist = np.sum(X1 ** 2, 1).reshape(-1, 1) + np.sum(X2 ** 2, 1) - 2 * np.dot(X1, X2.T)
    return theta[1] ** 2 * np.exp(-0.5 / theta[0] ** 2 * sqdist)

def K_(X, theta, diag_only=False, nu=1e-5):
    """Helper to apply kernel function."""
    if diag_only:
        # Specific solution for isotropic 
        # squared exponential kernel.
        return theta[1] ** 2 + nu
    else:
        return kernel(X, X, theta) + nu * np.eye(X.shape[0])

def W_(z):
    """Helper to compute matrix W."""
    r = sigmoid(z) * (1 - sigmoid(z))
    return np.diag(r.ravel())

def posterior_mode(X, y, K_z, max_iter=10, tol=1e-9):
    """
    Computes the mode of posterior p(z|D).
    """
    z_h = np.zeros_like(y)
    I = np.eye(X.shape[0])

    for i in range(max_iter):
        W = W_(z_h)
        Q_inv = np.linalg.inv(I + W @ K_z)
        z_h_new = (K_z @ Q_inv).dot(y - sigmoid(z_h) + W.dot(z_h))
        z_h_diff = np.abs(z_h_new - z_h)
        z_h = z_h_new

        if not np.any(z_h_diff > tol):
            break

    return z_h

#likelihood function
def nll_fn(X, y):
    """
    Returns the negative log-likelihood function for data X, t.
    """
    y = y.ravel()

    def nll(theta):
        K_z = K_(X, theta)
        K_z_inv = np.linalg.inv(K_z)

        # posterior mode depends on theta (via K)
        z_h = posterior_mode(X, y, K_z).ravel()
        W = W_(z_h)

        ll = - 0.5 * z_h.T.dot(K_z_inv).dot(z_h) \
             - 0.5 * np.linalg.slogdet(K_z)[1] \
             - 0.5 * np.linalg.slogdet(W + K_z_inv)[1] \
             + y.dot(z_h) - np.sum(np.log(1.0 + np.exp(z_h))) #not sure if this is correct yet

        return -ll

    return nll


#likelihood function over multiple envs
def nll_multiple(X, ylist):
    """
    runs nLL over multiple envs
    """
    def nll(theta):
        K_z = K_(X, theta)
        K_z_inv = np.linalg.inv(K_z)
       
        ll = 0
        for y in ylist:
            # posterior mode depends on theta (via K)
            z_h = posterior_mode(X, y, K_z).ravel()
            W = W_(z_h)

            ll += - 0.5 * z_h.T.dot(K_z_inv).dot(z_h) \
                - 0.5 * np.linalg.slogdet(K_z)[1] \
                - 0.5 * np.linalg.slogdet(W + K_z_inv)[1] \
                + y.dot(z_h) - np.sum(np.log(1.0 + np.exp(z_h))) #not sure if this is correct yet

        return -ll
    return nll
 

def predict_z(X_test, X, y, theta, priorReward = 0.25):
    """
    Computes the mean and variance of logits at points X_test
    given training data X, y and kernel parameters theta.
    """
    prior_z = logit(priorReward)
    K_z = K_(X, theta)
    K_s = kernel(X, X_test, theta)
    z_h = posterior_mode(X, y, K_z)

    W_inv = np.linalg.inv(W_(z_h))
    R_inv = np.linalg.inv(W_inv + K_z)

    z_test_mu = K_s.T.dot(y - sigmoid(z_h)) + prior_z
    # Compute variances only (= diagonal) instead of full covariance matrix
    z_test_var = K_(X_test, theta, diag_only=True) - np.sum((R_inv @ K_s) * K_s, axis=0).reshape(-1, 1)

    return z_test_mu, z_test_var

def predict_pt(X_test, X, y, theta):
    """
    Computes the probability of y=1 at points X_test
    given training data X, y and kernel parameters theta.
    """
    z_mu, z_var = predict_z(X_test, X, y, theta)
    kappa = 1.0 / np.sqrt(1.0 + np.pi * z_var / 8)
    return sigmoid(kappa * z_mu)
