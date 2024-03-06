import numpy as np
from task import State
import datetime, sys, random
import pandas as pd
#from tqdm.notebook import tqdm
from simulation import Simulation, Policy, SimulationParameters, SimulationParameterSpace

np.seterr(divide='ignore')

def tot_pairwise_dist(x, y):
    return np.sum(np.sqrt((x[None, :] - x[:, None])**2 + (y[None, :] - y[:, None])**2))/2

#Define simulation parameters
global_seed =int(sys.argv[1])
global_rng = random.Random(global_seed) # global random number generator

max_steps = 600
mining_steps = 9

envFiles = ['../environments/smooth.%i.csv' % global_rng.randint(1,20), '../environments/random.%i.csv' % global_rng.randint(1,20)]

# batch = 'batch1' #simple set up

# #Define agents
# agent_types = {
#     "dist_only": Policy(dist_weight=-2, in_view_weight=0, gp_weight=0, any_social_weight=0, success_social_weight=0),
#     # "in_view_dist_only": Policy(dist_weight=-2, in_view_weight=1, gp_weight=0, any_social_weight=0, success_social_weight=0),
#     # "asocial": Policy(dist_weight=-2, in_view_weight=1, gp_weight=5, any_social_weight=0, success_social_weight=0),
#     # "social_any": Policy(dist_weight=-2, in_view_weight=1, gp_weight=5, any_social_weight=2, success_social_weight=0),
#     # "social_success": Policy(dist_weight=-2, in_view_weight=1, gp_weight=5, any_social_weight=0, success_social_weight=2),
#     "dist_only_social_success": Policy(dist_weight=-2, in_view_weight=0, gp_weight=0, any_social_weight=0, success_social_weight=2),
#     "dist_only_social_any": Policy(dist_weight=-2, in_view_weight=0, gp_weight=0, any_social_weight=2, success_social_weight=0),
# }

batch = 'batch2' #all relevant features

#Define agents
agent_types = {
    "dist_only": Policy(dist_weight=-1, in_view_weight=1, gp_weight=1, any_social_weight=0, success_social_weight=0),
    # "in_view_dist_only": Policy(dist_weight=-2, in_view_weight=1, gp_weight=0, any_social_weight=0, success_social_weight=0),
    # "asocial": Policy(dist_weight=-2, in_view_weight=1, gp_weight=5, any_social_weight=0, success_social_weight=0),
    # "social_any": Policy(dist_weight=-2, in_view_weight=1, gp_weight=5, any_social_weight=2, success_social_weight=0),
    # "social_success": Policy(dist_weight=-2, in_view_weight=1, gp_weight=5, any_social_weight=0, success_social_weight=2),
    "dist_only_social_success": Policy(dist_weight=-1, in_view_weight=1, gp_weight=1, any_social_weight=0, success_social_weight=1),
    "dist_only_social_any": Policy(dist_weight=-1, in_view_weight=1, gp_weight=1, any_social_weight=1, success_social_weight=0),
}



parameter_space = SimulationParameterSpace(
    agent_type_policies=agent_types,
    n_agents=[1, 4],
    environment_files=envFiles,
    n_init_states=10,
    n_rollouts=10,
    global_seed=global_seed,
)
results = []
for params in parameter_space:
    params : SimulationParameters
    sim = Simulation(
        agent_policies=[params.policy]*params.n_agents,
        initial_state=State.from_env_file(params.env_file, seed=params.init_seed, n_agents=params.n_agents),
        max_steps=max_steps,
        mining_steps=mining_steps,
    )
    simres = sim.run(seed=params.policy_seed, sim_slice_rate=None)

    # get statistics
    df = pd.DataFrame(simres.stats)
    df['navigation'] = df['agent_state_mode'] == 'navigate_to'
    total_steps = df['step'].max()
    cum_rewards = df.pivot(index='step', columns='agent', values='reward').cumsum()
    cum_navigation = df.pivot(index='step', columns='agent', values='navigation').cumsum()
    revealed_state = df.groupby('step')['revealed'].mean()
    agent_xys = df.pivot(index='step', columns='agent', values=['x', 'y'])
    for step in list(range(0,total_steps, 100)) + [total_steps]:
        if step>0:
            results.append(dict(
                **params.summary_dict(),
                step=step,
                final=step == total_steps - 1,
                max_reward=cum_rewards.iloc[step].max(),
                min_reward=cum_rewards.iloc[step].min(),
                tot_reward=cum_rewards.iloc[step].sum(),
                tot_pairwise_dist=tot_pairwise_dist(agent_xys.iloc[step].x.values, agent_xys.iloc[step].y.values),
                n_foraged_blocks=revealed_state.iloc[step],
                min_distance_traveled_prop=cum_navigation.iloc[step].min()/step,
                max_distance_traveled_prop=cum_navigation.iloc[step].max()/step,
                mean_distance_traveled_prop=cum_navigation.iloc[step].mean()/step,
            ))
    # if params.simulation_number % 20 == 0:
    #     pd.DataFrame(results).to_pickle(results_filename)


dat = pd.DataFrame(results)
results_filename = 'results/%s/simulations.%s.csv' % (batch, sys.argv[1])
dat.to_csv(results_filename)
