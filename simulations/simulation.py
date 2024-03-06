from typing import List
import numpy as np
import dataclasses
#import matplotlib.pyplot as plt
from typing import List, Tuple, Dict, Optional
import pandas as pd
#import matplotlib.pyplot as plt
import copy
import random
from itertools import product
from task import State, AgentView, \
    is_visible, distance, gaussian_process_logits, visible_player_logits, successful_visible_player_logits


@dataclasses.dataclass
class Simulation:
    agent_policies : List["Policy"]
    initial_state : State
    max_steps : int = 1000
    mining_steps : int = 9

    def run(self, seed=None, sim_slice_rate=1):
        rng = np.random.Generator(np.random.PCG64(seed))
        state = copy.deepcopy(self.initial_state)
        agent_views = [AgentView.from_state(state, agent_i) for agent_i in range(state.n_agents)]
        agent_states = [('choose_cell', None) for _ in range(state.n_agents)]
        agent_moves = np.zeros((2, state.n_agents), dtype=int)
        agent_mine_actions = np.zeros(state.n_agents, dtype=bool)
        sim_slices = []
        stats = []
        for step in range(self.max_steps):
            agent_mine_actions[:] = False
            for agent_i in range(state.n_agents):
                agent_view = agent_views[agent_i]
                policy = self.agent_policies[agent_i]
                agent_state = agent_states[agent_i]
                cur_cell = state.agent_locs[:, agent_i]
                if agent_state[0] == 'choose_cell':
                    goal_cell = policy.sample_next_cell(agent_view, state, rng=rng)
                    agent_states[agent_i] = ("navigate_to", goal_cell)
                    agent_view.clear_agent_info()
                elif agent_state[0] == 'navigate_to':
                    goal_cell = agent_state[1]
                    if np.all(goal_cell == cur_cell):
                        if not state.revealed_map[tuple(cur_cell)]:
                            agent_states[agent_i] = ("mine_cell", self.mining_steps)
                        else:
                            agent_states[agent_i] = ("choose_cell", None)
                        goal_cell = None
                elif agent_state[0] == 'mine_cell':
                    goal_cell = None
                    if agent_state[1] == 0:
                        agent_states[agent_i] = ("choose_cell", None)
                        agent_mine_actions[agent_i] = True
                    else:
                        agent_states[agent_i] = ("mine_cell", agent_state[1] - 1)
                else:
                    raise ValueError(f"Unknown agent state {agent_state}")
                if goal_cell is not None:
                    if goal_cell[0] > cur_cell[0]:
                        agent_move = np.array([1, 0])
                    elif goal_cell[0] < cur_cell[0]:
                        agent_move = np.array([-1, 0])
                    elif goal_cell[1] > cur_cell[1]:
                        agent_move = np.array([0, 1])
                    elif goal_cell[1] < cur_cell[1]:
                        agent_move = np.array([0, -1])
                    else:
                        agent_move = np.array([0, 0])
                    agent_moves[:, agent_i] = agent_move
                else:
                    agent_moves[:, agent_i] = 0
            state.update(agent_moves, agent_mine_actions)
            for agent_i in range(state.n_agents):
                agent_views[agent_i].update(state)
            
            if (sim_slice_rate is not None) and (step % sim_slice_rate == 0):
                sim_slices.append(self.step_slice(step, state, agent_views, agent_states))
            stats.extend(self.step_stats(step, state, agent_views, agent_states))
            if np.all(state.revealed_map):
                break
        return SimulationResult(stats=stats, sim_slices=sim_slices, init_state=self.initial_state)
    
    def step_slice(self, step, state, agent_views, agent_states):
        return SimulationSlice(**{
            'step': step,
            'state': copy.deepcopy(state),
            'agent_views': copy.deepcopy(agent_views),
            'agent_states': copy.deepcopy(agent_states),
            'agent_policies': self.agent_policies,
        })
    def step_stats(self, step, state : State, agent_views : List[AgentView], agent_states) -> List[Dict]:
        revealed = state.revealed_map.sum()
        return [
            {
                'step': step,
                'agent': i,
                'reward': state.just_rewarded_agents[i],
                'agent_state_mode': agent_states[i][0],
                'agent_state_param': agent_states[i][1],
                'x': state.agent_locs[0, i],
                'y': state.agent_locs[1, i],
                'revealed': revealed
            }
            for i in range(state.n_agents)
        ]

@dataclasses.dataclass
class SimulationSlice:
    step : int
    state : State
    agent_views : List[AgentView]
    agent_states : List
    agent_policies : List["Policy"]

    def plot_agent_views(self, axes=None):
        if axes is None:
            fig, axes = plt.subplots(1, self.state.n_agents, figsize=((self.state.n_agents)*5, 5))
            if self.state.n_agents == 1:
                axes = [axes]
        assert len(axes) == self.state.n_agents
        for agent_i in range(self.state.n_agents):
            self.agent_policies[agent_i].plot_next_cell_dist(self.agent_views[agent_i], self.state, ax=axes[agent_i])
            if self.agent_states[agent_i][0] == 'navigate_to':
                goal_cell = self.agent_states[agent_i][1]
                cur_cell = self.state.agent_locs[:, agent_i]
                axes[agent_i].plot([goal_cell[0], cur_cell[0]], [goal_cell[1], cur_cell[1]], '-', color='green', lw=2)
            axes[agent_i].set_title(f"Agent {agent_i}: {self.agent_states[agent_i]}")
        axes[0].figure.suptitle(f"Step {self.step}")
        return axes

@dataclasses.dataclass
class SimulationResult:
    stats : List
    sim_slices : List[SimulationSlice]
    init_state : State

    def plot_stats(self, axes=None):
        if axes is None:
            fig, axes = plt.subplots(1, 4, figsize=(22, 5))
        
        self.init_state.plot(axes[0])
        axes[0].set_title("Initial State")

        df = pd.DataFrame(self.stats)

        locs = df.pivot(index='step', columns='agent', values=['x', 'y'])
        locs['step_bin'] = locs.index // 100
        locs = locs.groupby('step_bin').mean()
        # fig, ax = plt.subplots(1, 1, figsize=(10, 10))
        ax = axes[1]
        s = np.arange(len(locs))*10
        for i in range(4):
            ax.plot(locs['x'][i], locs['y'][i], label=f"Agent {i}", alpha=.5)
            ax.scatter(locs['x'][i], locs['y'][i], label=f"Agent {i}", s=s, alpha=.5)

        ax.plot(locs['x'].mean(axis=1), locs['y'].mean(axis=1), label=f"Mean", color='k')
        ax.scatter(locs['x'].mean(axis=1), locs['y'].mean(axis=1), label=f"Mean", s=s, color='k')
        # ax.errorbar(locs['x'].mean(axis=1), locs['y'].mean(axis=1), xerr=locs['x'].std(axis=1), yerr=locs['y'].std(axis=1), label=f"Mean", color='k', )
        ax.set_title("Agent locations")
        ax.set_xlim(*axes[0].get_xlim())
        ax.set_ylim(*axes[0].get_ylim())


        locs = df.pivot(index='step', columns='agent', values=['x', 'y'])
        locs['mean_x'] = locs['x'].mean(axis=1)
        locs['mean_y'] = locs['y'].mean(axis=1)
        locs['centroid_dist'] = locs.apply(lambda row: np.sqrt((row['x'] - row['mean_x'].item())**2 + (row['y'] - row['mean_y'].item())**2).mean(), axis=1)
        locs['centroid_dist'].plot(ax=axes[2]).set_title("Mean distance from centroid")
        axes[2].set_ylim(-1, np.max(self.init_state.rewards_map.shape))

        df.pivot(index='step', columns='agent', values='reward').cumsum().plot(ax=axes[3]).set_title("Cumulative Rewards")
        return axes

@dataclasses.dataclass
class Policy:
    dist_weight : float
    in_view_weight : float
    gp_weight : float
    any_social_weight : float
    success_social_weight : float
    lengthscale : float = 1
    signalvariance : float = 1

    def next_cell_logits(self, agent_view: AgentView, state: State):
        ax, ay = agent_view.agent_locs[:, agent_view.agent_i]
        dx, dy = agent_view.agent_dirs[:, agent_view.agent_i]
        cx, cy = np.where(state.revealed_map == False)
        obs_cx, obs_cy = np.where(~np.isnan(agent_view.agent_rewards_map))
        rewarded = agent_view.agent_rewards_map[(obs_cx, obs_cy)] == 1
 
        if self.in_view_weight == 0:
            in_view = np.ones_like(cx)
        else:
            in_view = is_visible(ax, ay, dx, dy, cx, cy)
        if self.dist_weight == 0:
            dist = np.zeros_like(cx)
        else:
            dist = distance(ax, ay, cx, cy)
        if self.gp_weight == 0:
            reward_gp = np.zeros_like(cx)
        else:
            reward_gp = gaussian_process_logits(
                cx=obs_cx, cy=obs_cy, r=rewarded, ncx=cx, ncy=cy, 
                lengthscale=self.lengthscale, signalvariance=self.signalvariance
            )
        if self.any_social_weight == 0:
            any_social = np.zeros_like(cx)
        else:
            any_social = visible_player_logits(
                cx, cy, agent_view.agent_locs, agent_view.agent_dirs,
                agent_view.agent_i
            )
        if self.success_social_weight == 0:
            success_social = np.zeros_like(cx)
        else:
            success_social = successful_visible_player_logits(
                cx, cy, agent_view.agent_locs, agent_view.agent_dirs,
                agent_view.agent_i, agent_view.rewarded_agents
            )
        logits = dist*self.dist_weight + in_view*self.in_view_weight + \
            reward_gp*self.gp_weight + any_social*self.any_social_weight + \
            success_social*self.success_social_weight
        return cx, cy, logits
    
    def sample_next_cell(self, agent_view: AgentView, state: State, rng: np.random.RandomState = None):
        if rng is None:
            rng = np.random
        cx, cy, logits = self.next_cell_logits(agent_view, state)
        prob = np.exp(logits - np.max(logits))
        prob = prob/np.sum(prob)
        ci = rng.choice(len(prob), p=prob)
        return cx[ci], cy[ci]
    
    def plot_next_cell_dist(self, agent_view: AgentView, state: State, ax=None):
        if ax is None:
            fig, ax = plt.subplots(1, 1, figsize=(5, 5))
        agent_view.plot(ax=ax, state=state, plot_unrevealed=False)
        cx, cy, logits = self.next_cell_logits(agent_view, state)
        prob = np.exp(logits - np.max(logits))
        prob = prob/np.sum(prob)
        ax.scatter(x=cx, y=cy, marker='o', s=np.sqrt(prob)*100, color='grey', alpha=.5)
        ax.set_xlim(-1, state.rewards_map.shape[0])
        ax.set_ylim(-1, state.rewards_map.shape[1])

@dataclasses.dataclass
class SimulationParameters:
    simulation_number : int
    env_file : str
    agent_type : str
    n_agents : int
    policy : Policy
    init_seed : int
    policy_seed : int
    def summary_dict(self):
        return {
            'simulation_number': self.simulation_number,
            'env_file': self.env_file,
            'agent_type': self.agent_type,
            'n_agents': self.n_agents,
            **self.policy.__dict__,
            'init_seed': self.init_seed,
            'policy_seed': self.policy_seed,
        }

@dataclasses.dataclass
class SimulationParameterSpace:
    agent_type_policies : Dict
    n_agents : List[int]
    environment_files : List
    n_init_states : int
    n_rollouts : int
    global_seed : int

    def __iter__(self) -> Tuple[SimulationParameters]:
        rng = random.Random(self.global_seed)
        iterator = product(
            self.n_agents,
            self.agent_type_policies.items(),
            self.environment_files,
            [rng.randint(0, 2**32 - 1) for _ in range(self.n_init_states)],
            [rng.randint(0, 2**32 - 1) for _ in range(self.n_rollouts)],
        )
        for simulation_number, params in enumerate(iterator):
            n_agents, (agent_type, policy), env_file, init_seed, policy_seed = params
            yield SimulationParameters(
                simulation_number,
                env_file,
                agent_type,
                n_agents,
                policy,
                init_seed,
                policy_seed
            )
    
    def __len__(self):
        return self.n_init_states*self.n_rollouts*len(self.n_agents)*len(self.agent_type_policies)*len(self.environment_files)
