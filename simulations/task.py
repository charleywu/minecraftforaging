import csv
import numpy as np
import dataclasses
#import matplotlib.pyplot as plt

from spatial_gp import predict_z

@dataclasses.dataclass
class State:
    rewards_map : np.ndarray
    revealed_map : np.ndarray
    just_revealed_map : np.ndarray
    just_rewarded_agents : np.ndarray
    agent_locs : np.ndarray
    agent_dirs : np.ndarray

    def update(self, agent_moves, mine_action):
        dir_change = np.any(agent_moves != 0, axis=0)
        self.agent_dirs[:, dir_change] = agent_moves[:, dir_change]
        self.agent_locs = self.agent_locs + agent_moves
        # mine_action = np.all(agent_moves == 0, axis=0)
        self.just_revealed_map[:] = False
        mine_locs = self.agent_locs[:, mine_action]
        self.just_rewarded_agents = mine_action & (self.revealed_map[tuple(self.agent_locs)] == False) & (self.rewards_map[tuple(self.agent_locs)] == True)
        self.just_revealed_map[tuple(mine_locs)] = self.revealed_map[tuple(mine_locs)] == False
        self.revealed_map[tuple(mine_locs)] = True
    
    def plot(self, ax=None):
        if ax is None:
            fig, ax = plt.subplots(1, 1, figsize=(5, 5))
        height, width = self.rewards_map.shape
        cy, cx = np.meshgrid(np.arange(height), np.arange(width))
        ax.scatter(cx, cy, s=(~self.revealed_map)*50, color='grey', alpha=.2)
        ax.scatter(cx, cy, s=self.rewards_map*10, color='green', marker='*')
        ax.plot(self.agent_locs[0] + .2, self.agent_locs[1] + .2, 'o', color='blue')
        ax.plot(self.agent_locs[0] + self.agent_dirs[0]*.2 + .2, self.agent_locs[1] + self.agent_dirs[1]*.2 + .2, '.', color='lightblue')
        for agent_i in range(self.n_agents):
            agent_str = str(agent_i) + ('*' if self.just_rewarded_agents[agent_i] else '')
            ax.text(self.agent_locs[0, agent_i] + .2, self.agent_locs[1, agent_i] + .8, agent_str, color='r', ha='center', va='center', fontsize=12)
        ax.set_xlim(-1, width)
        ax.set_ylim(-1, height)
    
    @property
    def n_agents(self):
        return self.agent_locs.shape[1]

    @classmethod
    def random_init(cls, height, width, n_agents=4, seed=None):
        rng = np.random.Generator(np.random.PCG64(seed))
        rewards_map = rng.binomial(1, .3, height*width).reshape(height, width)
        revealed_map = np.zeros((height, width), dtype=bool)
        just_revealed_map = np.zeros((height, width), dtype=bool)
        just_rewarded_agents = np.zeros(n_agents, dtype=bool)
        agent_locs = rng.choice(height*width, n_agents, replace=False)
        agent_locs = np.stack(np.unravel_index(agent_locs, (height, width)))
        agent_dirs = rng.choice(4, n_agents, replace=True)
        agent_dirs = np.array([np.array([0, 1]), np.array([1, 0]), np.array([0, -1]), np.array([-1, 0])])[agent_dirs].T
        return cls(rewards_map, revealed_map, just_revealed_map, just_rewarded_agents, agent_locs, agent_dirs)
    
    @classmethod
    def from_env_file(cls, filename, seed=None, n_agents=4):
        rng = np.random.Generator(np.random.PCG64(seed))
        env = [line[0].split(";") for line in csv.reader(open(filename, 'r'))]
        rewards_map = np.array([row[1:] for row in env[1:]]).astype(int)
        revealed_map = np.zeros_like(rewards_map, dtype=bool)
        just_revealed_map = np.zeros_like(rewards_map, dtype=bool)
        just_rewarded_agents = np.zeros(n_agents, dtype=bool)
        agent_locs = rng.choice(rewards_map.size, n_agents, replace=False)
        agent_locs = np.stack(np.unravel_index(agent_locs, rewards_map.shape))
        agent_dirs = rng.choice(4, n_agents, replace=True)
        agent_dirs = np.array([np.array([0, 1]), np.array([1, 0]), np.array([0, -1]), np.array([-1, 0])])[agent_dirs].T
        return cls(rewards_map, revealed_map, just_revealed_map, just_rewarded_agents, agent_locs, agent_dirs)

@dataclasses.dataclass
class AgentView:
    agent_rewards_map : np.ndarray
    agent_locs : np.ndarray
    rewarded_agents : np.ndarray
    agent_dirs : np.ndarray
    agent_i : int

    def update(self, state : State):
        loc, dir = state.agent_locs[:, self.agent_i], state.agent_dirs[:, self.agent_i]
        locs_just_revealed = np.stack(np.where(state.just_revealed_map))
        in_view = is_visible(loc[0], loc[1], dir[0], dir[1], locs_just_revealed[0], locs_just_revealed[1])
        locs_in_view = locs_just_revealed[:, in_view]
        self.agent_rewards_map[tuple(locs_in_view)] = state.rewards_map[tuple(locs_in_view)]
        agents_in_view = is_visible(loc[0], loc[1], dir[0], dir[1], state.agent_locs[0], state.agent_locs[1])
        self.agent_locs[:, agents_in_view] = state.agent_locs[:, agents_in_view]
        self.agent_dirs[:, agents_in_view] = state.agent_dirs[:, agents_in_view]
        in_view_rewarded_agents = agents_in_view & state.just_rewarded_agents
        self.rewarded_agents = in_view_rewarded_agents | self.rewarded_agents
    
    def clear_agent_info(self):
        self.agent_locs[:] = np.nan
        self.agent_dirs[:] = np.nan
        self.rewarded_agents[:] = False

    @classmethod
    def from_state(cls, state : State, agent_i):
        agent_rewards_map = np.zeros_like(state.rewards_map)*np.nan
        agent_locs = np.zeros_like(state.agent_locs)*np.nan
        agent_locs[:, agent_i] = state.agent_locs[:, agent_i]
        agent_dirs = np.zeros_like(state.agent_dirs)*np.nan
        agent_dirs[:, agent_i] = state.agent_dirs[:, agent_i]
        rewarded_agents = np.zeros(state.agent_locs.shape[1], dtype=bool)
        return cls(agent_rewards_map, agent_locs, rewarded_agents, agent_dirs, agent_i)
    
    def plot(self, state : State, ax=None, plot_unrevealed=False):
        if ax is None:
            fig, ax = plt.subplots(1, 1, figsize=(5, 5))
        height, width = self.agent_rewards_map.shape
        cy, cx = np.meshgrid(np.arange(height), np.arange(width))
        if plot_unrevealed:
            ax.scatter(cx, cy, s=(~state.revealed_map)*50, color='grey', alpha=.2)
        ax.scatter(cx, cy, s=(self.agent_rewards_map == 1)*10, color='green', marker='o')
        ax.scatter(cx, cy, s=(self.agent_rewards_map == 0)*10, color='brown', marker='o', alpha=.7)

        for agent_i in range(state.n_agents):
            if np.isnan(self.agent_locs[0, agent_i]):
                continue
            if agent_i == self.agent_i:
                ax.plot(self.agent_locs[0, agent_i] + .2, self.agent_locs[1, agent_i] + .2, 'o', color='blue')
                ax.plot(self.agent_locs[0, agent_i] + self.agent_dirs[0, agent_i]*.2 + .2, self.agent_locs[1, agent_i] + self.agent_dirs[1, agent_i]*.2 + .2, '.', color='lightblue')
            else:
                ax.plot(self.agent_locs[0, agent_i] + .2, self.agent_locs[1, agent_i] + .2, 'o', color='blue', alpha=.5)
                ax.plot(self.agent_locs[0, agent_i] + self.agent_dirs[0, agent_i]*.2 + .2, self.agent_locs[1, agent_i] + self.agent_dirs[1, agent_i]*.2 + .2, '.', color='lightblue', alpha=.5)
            agent_str = str(agent_i) + ("*" if self.rewarded_agents[agent_i] else '')
            ax.text(self.agent_locs[0, agent_i] + .2, self.agent_locs[1, agent_i] + .8, agent_str, color='r', ha='center', va='center', fontsize=12)
        ax.set_xlim(-1, width)
        ax.set_ylim(-1, height)

def distance(ax, ay, cx, cy, **kwargs):
    return np.sqrt((ax-cx)*(ax-cx) + (ay-cy)*(ay-cy))

def angle_between(v1, v2):
    """ 
    Returns the angle in radians between 2d vectors 'v1' and 'v2'
    """
    norm_1 = np.sqrt(v1[0]*v1[0] + v1[1]*v1[1])
    norm_2 = np.sqrt(v2[0]*v2[0] + v2[1]*v2[1])
    return np.arccos((v1[0]*v2[0]+v1[1]*v2[1])/(norm_1*norm_2))

def is_visible(ax, ay, dx, dy, cx, cy, max_FOV_degrees=108.5, **kwargs):
    max_FOV_radians = max_FOV_degrees*np.pi/180 *.5
    mx, my = cx - ax, cy - ay
    norm = np.sqrt(mx*mx + my*my)
    mx = mx / norm
    my = my / norm
    viewing_angle = angle_between([dx, dy], [mx, my]) 
    return (viewing_angle <= max_FOV_radians) | ((ax == cx) & (ay == cy))

def gaussian_process_logits(cx, cy, r, ncx, ncy, lengthscale, signalvariance):
    theta = np.array([lengthscale, signalvariance])
    examples = np.stack([cx, cy]).T
    new_examples = np.stack([ncx, ncy]).T
    mean_est, uncertainty_est = predict_z(X_test=new_examples, X=examples, y=r, theta=theta)
    return mean_est

def visible_player_logits(cx, cy, agent_locs, agent_dirs, agent_i):
    ax, ay = agent_locs[:, agent_i]
    dx, dy = agent_dirs[:, agent_i]
    other_agents = np.arange(agent_locs.shape[1]) != agent_i
    return centroid_player_logits(cx, cy, ax, ay, dx, dy, agent_locs[:, other_agents])

def centroid_player_logits(cx, cy, ax, ay, dx, dy, agent_locs):
    in_view = is_visible(ax, ay, dx, dy, agent_locs[0], agent_locs[1])
    if not np.any(in_view):
        return np.zeros_like(cx)
    centroid = np.mean(agent_locs[:, in_view], axis=1)
    return -distance(cx, cy, centroid[0], centroid[1])

def successful_visible_player_logits(cx, cy, agent_locs, agent_dirs, agent_i, rewarded_agents):
    rewarded_agents = rewarded_agents.copy()
    rewarded_agents[agent_i] = False
    ax, ay = agent_locs[:, agent_i]
    dx, dy = agent_dirs[:, agent_i]
    return centroid_player_logits(cx, cy, ax, ay, dx, dy, agent_locs[:, rewarded_agents])