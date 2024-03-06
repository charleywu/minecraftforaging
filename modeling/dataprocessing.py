import os, sys 
import pandas as pd
import numpy as np
import pyarrow.feather as feather


####################################################################################################
# Load data and preprocess
####################################################################################################
class importData:
    #Import participant data
    def __init__(self, visibility=True, dataFolders = ['2021batch']):
        self.playerDF = pd.DataFrame()
        self.blockDF = pd.DataFrame()
        self.pvisDF = pd.DataFrame()
        self.evisDF = pd.DataFrame()
        for dataFolder in dataFolders:
          self.playerDF = self.playerDF.append(pd.read_feather('../analysis/data/%s/all_players_pd_cache.feather' % dataFolder))
          self.blockDF = self.blockDF.append(pd.read_feather('../analysis/data/%s/all_blocks_pd_cache.feather' % dataFolder))
          if (visibility==True):
            self.pvisDF = self.pvisDF.append(pd.read_feather('../analysis/simData/pvisDF.feather'))
            self.evisDF = self.evisDF.append(pd.read_feather('../analysis/simData/evisDF.feather'))
        
        #add unique ids
        self.blockDF['id'] = self.blockDF['name']+ self.blockDF['session']
        self.playerDF['id'] = self.playerDF['name']+ self.playerDF['session']
        
        #Create unique identifiers for each block
        blockRange = np.arange(start = 2,stop = 60, step = 3)
        self.blockIds = {}
        counter = 0
        for x in blockRange:
            for z in blockRange:
                self.blockIds['%i.%i' % (x,z)] = counter
                counter+=1
        
        #add block ids to dataframes
        self.blockDF['blockId'] = -1 #create dummy id
        blocks = ['%i.%i' % (x,z) for x,z in zip(self.blockDF['x'], self.blockDF['z'])]
        self.blockDF['blockId'] = [self.blockIds[block] for block in blocks]





