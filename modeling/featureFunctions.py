from collections import defaultdict
from functools import lru_cache
import math
import numpy as np
from immutables import Map
from scipy.special import expit, logit
from scipy.optimize import minimize
import pandas as pd


#Define global dictionaries mapping block locations to ids, and back
grid_x, grid_y = np.mgrid[2:60:3, 2:60:3] #X_star: All possible locations
ALL_FRUIT_LOCS = [(xy[0], xy[1]) for xy in np.stack([grid_x, grid_y], axis=-1).reshape(-1, 2)]
blockRange = np.arange(start = 2,stop = 60, step = 3)
blockIds = {}
counter = 0
for x in blockRange:
    for z in blockRange:
        blockIds['%i.%i' % (x,z)] = counter
        counter+=1
idBlocks =  {v: k for k, v in blockIds.items()} #invert the dictionary
BLOCKID_TO_LOC = {v: tuple([int(ki) for ki in k.split('.')]) for k, v in blockIds.items()} #tuple of location


#Define state representation
def convert_data_to_state(currentState, agent_locations, agent_directions, observed_rewards, soc_observed_rewards, successful_players, visible_players, other_players):
    """
    Converts experiment data into model format
    currentState is a dataframe up until the current decision time
    agent_locations is a dictionary {agent_name: location, ...}
    """
    destroyedBlocks = currentState.blockId.tolist()
    locs = [BLOCKID_TO_LOC[bid] for bid in destroyedBlocks]
    rewards = ['+' if r else 'o' for r in currentState.reward]
    open_locs_fruits = dict(zip(locs, rewards))
    unopen_locs_fruits = {l: "?" for l in ALL_FRUIT_LOCS if l not in open_locs_fruits.keys()}
    return Map(
        open_locs_fruits=Map(open_locs_fruits),
        unopen_locs_fruits=Map(unopen_locs_fruits),
        observed_rewards = Map(observed_rewards),
        soc_observed_rewards = Map(soc_observed_rewards),
        agents_locs=Map(agent_locations),
        agents_dirs=Map(agent_directions),
        successful_players=Map(successful_players),
        visible_players=Map(visible_players),
        other_players=Map(other_players)
    )

def feature_constructor(*, agent, state, feature_functions):
    """
    Each function in dest_feature_functions takes 
    the agent, the state, and current dest and returns a 
    sequence of dest-feature pairs.
    """
    featureDF = pd.DataFrame(ALL_FRUIT_LOCS, columns =['x','z'])
    for ff in feature_functions:
        featureDF[ff.__name__] = np.nan 
        if (ff.__name__ == 'social_GP_pred' or ff.__name__ == 'asocial_GP_pred'): #GP_pred returns two features, z and uncertainty
            dest_features = ff(agent, state) 
            #featureDF['GP_uncertainty'] = np.nan #add another column for the uncertainty estimates
            for dest, mean_est, uncertainty_est in dest_features:
                featureDF.loc[(featureDF['x'] ==dest[0]) & (featureDF['z'] == dest[1]),  ff.__name__] = mean_est
                #featureDF.loc[(featureDF['x'] ==dest[0]) & (featureDF['z'] == dest[1]), 'GP_uncertainty'] = uncertainty_est
        else: #all other feature functions return a single feature
            dest_features = ff(agent, state) 
            for dest, feature in dest_features:
                featureDF.loc[(featureDF['x'] ==dest[0]) & (featureDF['z'] == dest[1]), ff.__name__] = feature
    return featureDF


# --------------------------------------
# Destination feature functions
# --------------------------------------

#Distance to block
def fruit_distance(agent, state):
    """
    Distance features
    """
    here = state['agents_locs'][agent]
    for loc in state['unopen_locs_fruits'].keys():
        yield loc, ((loc[0] - here[0])**2 + (loc[1] - here[1])**2)**.5
        
        

def angle_between(v1, v2):
    """ 
    Returns the angle in radians between 2d vectors 'v1' and 'v2'
    """
    norm_1 = math.sqrt(v1[0]*v1[0] + v1[1]*v1[1])
    norm_2 = math.sqrt(v2[0]*v2[0] + v2[1]*v2[1])
    return math.acos((v1[0]*v2[0]+v1[1]*v2[1])/(norm_1*norm_2))

def is_visible(ego_loc, ego_dir, target_loc, max_FOV_degrees=108.5):
    if ego_loc == target_loc:
        return True
    max_FOV_radians = max_FOV_degrees*math.pi/180 *.5
    displacement = [target_loc[0] - ego_loc[0], target_loc[1]- ego_loc[1] ]
    norm = math.sqrt(displacement[0] * displacement[0] + displacement[1] * displacement[1])
    direction = (displacement[0] / norm, displacement[1] / norm) #direction vector
    viewing_angle = angle_between(ego_dir, direction) 
    return viewing_angle <= max_FOV_radians

        
#block in FOV (0=no,1=yes)
def fruit_pov(agent, state, FOV_degrees = 108.5):
    """
    POV feature
    """
    maxFOV = FOV_degrees*math.pi/180 *.5 #max field of view degree in radians
    here = state['agents_locs'][agent]
    facing_direction = state['agents_dirs'][agent] #Note: data also has yaw component of gaze, which we will ignore
    for loc in state['unopen_locs_fruits'].keys():
        if is_visible(here, facing_direction, loc, FOV_degrees):
            yield loc, 1
        else:
            yield loc, 0
        

#GP predictions based on past observed rewards
def create_GP_pred(type = 'social', lengthscale=np.sqrt(48), data = 'experiment'):
    #Define which set of reward observations to use
    if type=='social':
        rewardObs = 'soc_observed_rewards'
    elif type == 'asocial':
        rewardObs = 'observed_rewards'
    #Create feature function
    def GP_pred(agent, state, optimizePars = False, lengthscale = lengthscale, signalvariance=10, priorMean = logit(.25)):
        """
        GP predictions about reward
        Lengthscale is set to the true underlying structure of the environment (relative to the change in coordinate range)
        """
        if data =='simulations': #simulation data has a slightly different state representation
            agentState = state[rewardObs]
            prevObservations = agentState
        else:
            agentState = state[rewardObs][agent] #TODO: check that this is correct
            prevObservations = state[rewardObs].get(agent, [])
        if len(prevObservations)>0: #attempt to see if the agent has previous observations, defaulting to an empty list if no in the dictionary
            if data =='simulations': #simulation data uses locations rather than blockID and has a slightly different state representation
                X = np.array([bid for bid in agentState.keys()]) #previous observations of foraged blocks
                y = np.array([reward for reward in agentState.values()]) #Binary variable indicating if observed blocks contain rewards
            else: #experiment data uses block ids
              X = np.array([BLOCKID_TO_LOC[bid] for bid in agentState.keys()]) #previous observations of foraged blocks
              y = np.array([reward for reward in agentState.values()]) #Binary variable indicating if observed blocks contain rewards
            if (optimizePars):
                theta = minimize(nll_fn(X, y), [lengthscale, signalvariance],bounds=((1e-3, 100), (1e-3, 100)),method='L-BFGS-B').x
            else:
                theta = np.array([lengthscale, signalvariance])
            #Compute kernel
            #K_a = K_(X, theta) #compute kernel
            #Make predictions
            X_star = np.array(list(state['unopen_locs_fruits'].keys())) #states we want to make predictions over; numpy array in shape (N, 2)
            mean_est, uncertainty_est = predict_z(X_star, X, y, theta)
            for i in range(len(X_star)):
                yield tuple(X_star[i]), mean_est[i],  uncertainty_est[i][0]
        else:
            #no observations, default to prior
            theta = np.array([lengthscale, signalvariance]) #use ground truth parameter values as prior
            X_star = np.array(list(state['unopen_locs_fruits'].keys())) #states we want to make predictions over; numpy array in shape (N, 2)
            priorVar = K_(X_star, theta, diag_only=True)
            for i in range(len(X_star)):
                yield tuple(X_star[i]), priorMean,  priorVar #TODO: check that this is the correct prior var
    GP_pred.__name__ = f"{type}_GP_pred"
    return GP_pred

    

def successful_player_distance(agent, state):
    """
    Distance to other successful players (observed, thus visible by definition)
    """
    if len(state['successful_players'].keys())==0:
        for loc in state['unopen_locs_fruits'].keys():
            yield loc, np.nan
    else:
        centroid = np.mean([x for x in state['successful_players'].values()], axis= 0)
        for loc in state['unopen_locs_fruits'].keys():
            yield loc, ((loc[0] - centroid[0])**2 + (loc[1] - centroid[1])**2)**.5

def visible_player_distance(agent, state):
    """
    Distance to other visible players (no success observation)
    """
    if len(state['visible_players'].keys())==0:
        for loc in state['unopen_locs_fruits'].keys():
            yield loc, np.nan
    else:
        centroid = np.mean([x for x in state['visible_players'].values()], axis= 0)
        for loc in state['unopen_locs_fruits'].keys():
            yield loc, ((loc[0] - centroid[0])**2 + (loc[1] - centroid[1])**2)**.5
       

def other_player_distance(agent, state):
    """
    Distance to other players (no success and not visible)
    """
    if len(state['other_players'].keys())==0:
        for loc in state['unopen_locs_fruits'].keys():
            yield loc, np.nan
    else:
        for loc in state['unopen_locs_fruits'].keys():
            summedDistance = 0
            for other in state['other_players'].keys():
                otherHere =  state['other_players'][other]
                summedDistance += ((loc[0] - otherHere[0])**2 + (loc[1] - otherHere[1])**2)**.5
            yield loc, summedDistance/len(state['other_players'].keys())-1
            
            
def all_visible_player_distance(agent, state):
    """
    Distance to other visible players (either success or no success)
    """
    if (len(state['visible_players'].keys()) + len(state['successful_players'].keys()))==0:
        for loc in state['unopen_locs_fruits'].keys():
            yield loc, np.nan
    else:
        successfulPlayers =  {str(x):state['successful_players'][x] for x in state['successful_players'].keys()}
        unsuccessfulPlayers =  {str(x):state['visible_players'][x] for x in state['visible_players'].keys()} 
        allVisiblePlayers = dict(list(successfulPlayers.items()) + list(unsuccessfulPlayers.items()))
        for loc in state['unopen_locs_fruits'].keys():
            summedDistance = 0
            for other in allVisiblePlayers.keys():
                otherHere =  allVisiblePlayers[other]
                summedDistance += ((loc[0] - otherHere[0])**2 + (loc[1] - otherHere[1])**2)**.5
            yield loc, summedDistance/len(allVisiblePlayers.keys())-1


def create_playerN_dist(N):
    def playerN_dist(agent, state):
        """
        Distance to player N
        """
        selfId = agent[0:5]
        targetPlayer = f'MPIB{N}'
        if selfId == targetPlayer:  #if target is self, return null
            for loc in state['unopen_locs_fruits'].keys():
                yield loc, np.nan
        #Compile all visible players
        successfulPlayers =  {str(x):state['successful_players'][x] for x in state['successful_players'].keys()}
        unsuccessfulPlayers =  {str(x):state['visible_players'][x] for x in state['visible_players'].keys()} 
        allVisiblePlayers = dict(list(successfulPlayers.items()) + list(unsuccessfulPlayers.items()))
        #Is the target player visible?
        if (targetPlayer in allVisiblePlayers.keys()): 
            for loc in state['unopen_locs_fruits'].keys():
                summedDistance = ((loc[0] - allVisiblePlayers[targetPlayer][0])**2 + (loc[1] - allVisiblePlayers[targetPlayer][1])**2)**.5
                yield loc, summedDistance-1
        else: #target player not visible
            for loc in state['unopen_locs_fruits'].keys():
                yield loc, np.nan
    
    playerN_dist.__name__ = f"player{N}_dist"
    return playerN_dist

   
# --------------------------------------
# GP functions
# --------------------------------------

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



##### Testing ######
# import matplotlib.pyplot as plt

# playerdist = []
# for dest, feature in successful_player_distance(agent, state):
#     playerdist.append(feature)
# n, x, _ = plt.hist(playerdist,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()

# playerdist = []
# for dest, feature in visible_player_distance(agent, state):
#     playerdist.append(feature)
# n, x, _ = plt.hist(playerdist,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()

# playerdist = []
# for dest, feature in other_player_distance(agent, state):
#     playerdist.append(feature)

# n, x, _ = plt.hist(playerdist,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()

# fruitdist = []
# for dest, feature in fruit_distance(agent, state):
#     fruitdist.append(feature)


# n, x, _ = plt.hist(fruitdist,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()
