import os
import sys
import json
import numpy as np
import pandas as pd

sessions = []

### Helper functions for parsing
################################
def load_json(path):
    with open(path, "r", encoding='utf-8') as read_file:
        return json.load(read_file)

def parse_list(bytes):
    s = bytes.decode("utf-8")
    assert(len(s)>1)
    if len(s[1:len(s)-1].split(","))<=1:
        return []
    return s[1:len(s)-1].split(",")

#Otherwise it accidentally reads .DS_Store on macs
def listdir_nohidden(path):
    for f in os.listdir(path):
        if not f.startswith('.'):
            yield f

# Conversion functions for parsing:
f2f = lambda x: float(x)
b2b = lambda x: str(x) == "b'true'"
s2s = lambda x: x.decode("utf-8")
l2l = parse_list


### Round class
###############
class Round:
    raw = None          # raw json data
    session = None      # session this round belongs to
    index = -1          # index of the round inside the session
    is_solo = False     # Is this a solo round
    is_random = False   # Is this a random or smooth round
    players = None      # dataframe of player-events
    blocks = None       # dataframe of block-events
    players_path = ""   # Path of players file
    blocks_path = ""    # Path of blocks file

    # Initialization will read data from players_path and blocks_path and save it as a dataframe
    def __init__(self, raw, session, index, is_solo, is_random, players_path, blocks_path):
        self.players_path = players_path
        self.blocks_path = blocks_path
        self.raw = raw
        self.session = session
        self.index = index - 2 # Subtract the indices of the two training rounds
        self.is_solo = is_solo
        self.is_random = is_random
        self.players = np.genfromtxt(players_path, delimiter=';', skip_header=1, max_rows=99999999, names=['time', 'name', 'x', 'z', 'xlook', 'ylook', 'zlook', 'vis'], converters = {'time': f2f, 'name':s2s, 'x':f2f, 'z':f2f, 'xlook':f2f, 'ylook':f2f, 'zlook':f2f, 'vis':l2l}, dtype='object')
        self.blocks = np.genfromtxt(blocks_path, delimiter=';', skip_header=1, max_rows=99999999, names=['time', 'name', 'x', 'z', 'reward'], converters = {'time': f2f, 'name':s2s, 'x':f2f, 'z':f2f, 'reward':b2b}, dtype='object')
        # Convert players and blocks to pd dataframe:
        self.players = pd.DataFrame(data = self.players)
        self.blocks = pd.DataFrame(data = self.blocks)
        # Add in other info
        self.blocks['session'] = self.players['session'] = self.session
        self.blocks['round'] = self.players['round'] = self.index
        if self.is_solo:
            self.blocks['type'] = self.players['type'] = 'solo'
        else:
            self.blocks['type'] = self.players['type'] = 'group'
        if self.is_random:
            self.blocks['env'] = self.players['env'] = 'random'
        else:
            self.blocks['env'] = self.players['env'] = 'smooth'
        # Player visibilty: (Commented out, as now handled separately)
        p1_vis = [int("MPIB1" in row['vis']) for _, row in self.players.iterrows()]
        self.players.insert(loc=10, column='MPIB1_visible', value=p1_vis)
        p2_vis = [int("MPIB2" in row['vis']) for _, row in self.players.iterrows()]
        self.players.insert(loc=10, column='MPIB2_visible', value=p2_vis)
        p3_vis = [int("MPIB3" in row['vis']) for _, row in self.players.iterrows()]
        self.players.insert(loc=10, column='MPIB3_visible', value=p3_vis)
        p4_vis = [int("MPIB4" in row['vis']) for _, row in self.players.iterrows()]
        self.players.insert(loc=10, column='MPIB4_visible', value=p4_vis)

        del self.players['vis'] # Delete vis column

    # List of block events where a reward was found
    def blocks_true(self):
        return self.blocks[self.blocks['reward']==True]

    # list of block events where no reward was found
    def blocks_false(self):
        return self.blocks[self.blocks['reward']==False]


# Session class
###############
class Session:
    rounds = []         # list of rounds in this session
    name = "unnamed"    # Name of this session
    raw = None          # raw json data

    def __init__(self, name, raw):
        self.rounds = []
        self.name = name
        self.raw = raw

    # List of all solo-random rounds in this session
    def solo_random(self):
        return [x for x in self.rounds if x.is_random and x.is_solo]

    # List of all solo-smooth rounds in this session
    def solo_smooth(self):
        return [x for x in self.rounds if not x.is_random and x.is_solo]

    # List of all group-random rounds in this session
    def group_random(self):
        return [x for x in self.rounds if x.is_random and not x.is_solo]

    # List of all group-smooth rounds in this session
    def group_smooth(self):
        return [x for x in self.rounds if not x.is_random and not x.is_solo]

    # Returns the index of the session (Meaning session13.json -> 13)
    def get_index(self):
        return 0

    # Dataframe of all player events
    def all_players(self):
        return pd.concat([r.players for r in self.rounds]).reset_index(drop=True)

    # Dataframe of all block events
    def all_blocks(self):
        return pd.concat([r.blocks for r in self.rounds]).reset_index(drop=True)

# Dataframe of all player events
def all_players():
    return pd.concat([r.players for r in all_rounds()]).reset_index(drop=True)

# Dataframe of all block events
def all_blocks():
    return pd.concat([r.blocks for r in all_rounds()]).reset_index(drop=True)

# List of all rounds across all sessions
def all_rounds():
    ret = []
    for s in sessions: ret += s.rounds
    return ret

# List of all solo-random rounds across all sessions
def all_solo_random():
    ret = []
    for s in sessions: ret += s.solo_random()
    return ret

# List of all group-random rounds across all sessions
def all_group_random():
    ret = []
    for s in sessions: ret += s.group_random()
    return ret

# List of all solo-smooth rounds across all sessions
def all_solo_smooth():
    ret = []
    for s in sessions: ret += s.solo_smooth()
    return ret

# List of all group-smooth rounds across all sessions
def all_group_smooth():
    ret = []
    for s in sessions: ret += s.group_smooth()
    return ret


# Print some generic info on a list of rounds
def roundarray_genericinfo(list, title=""):
    ret = title + "\n"

    true_block_amount = sum( [len(x.blocks_true()) for x in list] )
    false_block_amount = sum( [len(x.blocks_false()) for x in list] )
    total_block_amount = true_block_amount + false_block_amount
    ret += ">> Total blocks destroyed: " + str(total_block_amount) + "\n"
    ret += ">> Block reward distr: T"+str(true_block_amount)+" / F"+str(false_block_amount) + "\n"
    ret += ">> Block reward ratio: " + str(round(true_block_amount / total_block_amount, 2)) + "\n"
    ret += ">> Block reward relative ratio: " + str(round(true_block_amount / false_block_amount, 2)) + "\n"

    return ret

lang = "_ge_" #German
#lang = "_" #English

def loadData(datasubfolder):
    # This loop parses all data included in the /data directory in accordance to what is in the /sessions direcotry
    for sess in listdir_nohidden("./sessions/"+datasubfolder):
        print("\rParsing session: " + sess + " [" + ''.join(map(str, (["-"]*17))) + "]", end='')
        s = Session(sess, load_json("./sessions/"+datasubfolder+"/"+sess))
        rounds = s.raw['roundData']

        for i in range(len(rounds)):
            print("\rParsing session: " + sess + " [" + ''.join(map(str, (["#"]*i))) + ''.join(map(str, (["-"]*(17-i)))) + "]", end='')
            if s.raw['roundData'][i]['trainingRound']=="TRUE": continue # Ignore training rounds
            r = Round( \
                raw = s.raw['roundData'][i], \
                session = s.name, \
                index = i, \
                is_solo = s.raw['roundData'][i]['soloRound']=="TRUE", \
                is_random = s.raw['roundData'][i]['environment']=="random", \
                players_path = "./data/" + datasubfolder +"/"+sess[0:len(sess)-5] + lang + str(i) + "_players.log.csv", \
                blocks_path = "./data/" + datasubfolder +"/"+sess[0:len(sess)-5] + lang + str(i) + "_blocks.log.csv" \
            )
            s.rounds.append(r)
        print("")

        sessions.append(s)
        s.all_players().to_feather(r'./data/'+datasubfolder+"/"+sess[0:len(sess)-5]+"_players_pd_cache.feather")
        s.all_blocks().to_feather(r'./data/'+datasubfolder+"/"+sess[0:len(sess)-5]+"_blocks_pd_cache.feather")
    all_players().to_feather(r'./data/'+datasubfolder+"/all_players_pd_cache.feather")
    all_blocks().to_feather(r'./data/'+datasubfolder+"/all_blocks_pd_cache.feather")

    #Write all paths for group rounds into vis_generate project
    f = open("./vis_generate/players_paths.txt", "w")
    f.write('\n'.join([r.players_path for r in all_group_smooth()]))
    f.write('\n'.join([r.players_path for r in all_group_random()]))
    f.close()
    f = open("./vis_generate/blocks_paths.txt", "w")
    f.write('\n'.join([r.blocks_path for r in all_group_smooth()]))
    f.write('\n'.join([r.blocks_path for r in all_group_random()]))
    f.close()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("******")
        print("Data parse: Extracts the data logs from each experiment and saves it in dataframe format using the feather file format")
        print("******")
        print("")
        print("Usage: python dataparse.py '<datasubfolder>'")
        
    else:
        loadData(sys.argv[1])

