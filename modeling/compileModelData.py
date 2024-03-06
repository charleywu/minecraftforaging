#Compile feature vectors for each destroyed block in order to model decision weights
#Charley Wu Feb 2022
import numpy as np
import os
import sys
import pyarrow
import pandas as pd
#pd.set_option('display.max_columns', None)
#os.chdir('producerScrounger')
#os.chdir('modeling')

from dataprocessing import importData #data import
from featureFunctions import * #feature functions


#Which features to use
batchName = 'modelFeatures'
featureSet = (fruit_distance,create_GP_pred('asocial'), fruit_pov, successful_player_distance, visible_player_distance, all_visible_player_distance, create_playerN_dist(1), create_playerN_dist(2), create_playerN_dist(3), create_playerN_dist(4))

#batchName = 'successFailure'
#featureSet = (fruit_distance,GP_pred,successful_player_distance, visible_player_distance)

#batchName = 'sameWeights'
#featureSet = (fruit_distance, GP_pred, all_visible_player_distance)

#batchName = 'uniqueWeights'
#featureSet = (fruit_distance, GP_pred, create_playerN_dist(1), create_playerN_dist(2), create_playerN_dist(3), create_playerN_dist(4))


# Load in participant data
data = importData(visibility=True)
playerDF = data.playerDF
blockDF = data.blockDF
pvisDF = data.pvisDF
evisDF = data.evisDF

#Define global dictionaries mapping block locations to ids, and back
blockIds =  data.blockIds  #dictionary of block locations to block ids
idBlocks =  {v: k for k, v in blockIds.items()} #invert the dictionary
BLOCKID_TO_LOC = {v: tuple([int(ki) for ki in k.split('.')]) for k, v in blockIds.items()} #tuple of location


#TESTING
# pid = 'MPIB3session3.json'
# p = pid
# rid = 7
# t = 16
# time = subdf.time.iloc[t-1]
# time_minus_one=subdf.time.iloc[t-2]

allFeatureDF = pd.DataFrame()
pid = blockDF.id.unique().tolist()[int(sys.argv[1]) - 1] #use sys.argv to run a separate job for each participant; -1 is to swap from base 1 to base 0
print(pid)
for rid in blockDF['round'].unique():
    print(rid)
    subdf = blockDF[(blockDF['id'] ==  pid) & (blockDF['round']==rid)] #player specific block destructions
    #Either ignore social data in solo rounds, or compile into a single dataframe in group rounds to keep tabs on the current state of the world
    if (subdf.type.iat[0]=='solo'):
        allblocks = subdf
    else:
        allblocks =  blockDF[(blockDF['session'] ==  subdf.session.iat[0]) & (blockDF['round']==rid)]
        pvisSub =  pvisDF[(pvisDF['session']==subdf['session'].iloc[0]) & (pvisDF['round']==rid ) & (pvisDF['name']==pid[0:5])]
    #Start location
    startLoc = playerDF[(playerDF['id'] ==  pid) & (playerDF['time']==0) & (playerDF['round']==rid)]
    playerLoc =  (startLoc.x.iloc[0], startLoc.z.iloc[0]) #player start location
    time_minus_one = 0.0
    #loop through blocks
    for t in range(len(subdf)):       
        block = subdf.blockId.iloc[t]
        if t==0:
            time = 0
        else:
            time = subdf.time.iloc[t-1]
        currentState =  allblocks[allblocks['time']<=time] #define this based on the current decision time, otherwise we would be including ineligible blocks
        #build game state
        if (subdf.type.iat[0]=='solo'): #If solo
            currentPlayer =  playerDF[(playerDF['id'] ==  pid) & (playerDF['round']==rid) & (playerDF['time']==time_minus_one)] #define starting player state based on last block (or at t=0)
            prevObs = currentState[currentState['id'] == pid]
            playerLoc =  (currentPlayer.x.iloc[0], currentPlayer.z.iloc[0]) 
            playerDir = (currentPlayer.xlook.iloc[0], currentPlayer.ylook.iloc[0], currentPlayer.zlook.iloc[0]) 
            rewardObs = dict(zip(prevObs.blockId, prevObs.reward))
            state = convert_data_to_state(currentState, {pid: playerLoc}, {pid: playerDir}, {pid:rewardObs}, dict(), dict(), dict(), dict()) 
        else: #if group
            locDict = dict()
            gazeDict = dict()
            rewardObs = dict()
            socrewards = dict()
            successfulPlayers = dict()
            visiblePlayers = dict()
            otherPlayers = dict()
            for p in allblocks.id.unique():
                currentGame=playerDF[(playerDF['id'] ==  p) & (playerDF['round']==rid) & (playerDF['time']==time)] #current player location and gaze direction
                prevObs = currentState[currentState['id'] == p] 
                #update location and gaze dictionaries
                locDict[p] = (currentGame.x.iloc[0], currentGame.z.iloc[0]) 
                gazeDict[p] = (currentGame.xlook.iloc[0], currentGame.zlook.iloc[0]) 
                #if target player, augment previous observations with socially observed reward destructions and social info
                if (p == pid):
                    visEvents = evisDF[(evisDF['session']==subdf['session'].iloc[0]) & (evisDF['round']==rid ) & (evisDF['eventtype']=='SPL' ) & (evisDF['PlayerName']==pid[0:5]) &  (evisDF['TriggerPlayer']!= pid[0:5]) & (evisDF['Time']<= time) ]
                    socialRewardObs = visEvents.groupby(['eventid', 'TriggerPlayer']).agg({'Occupancy': max, 'xPos' : np.mean, 'zPos' :  np.mean , 'Time': min}).reset_index()
                    #get block ids 
                    socObsLocations = []
                    for trigger, triggerTime in zip(socialRewardObs.TriggerPlayer, socialRewardObs.Time):
                        #find the block destruction from the trigger player nearest to the observation time
                        socObsLocations.append(blockDF[(blockDF['session'] ==  subdf['session'].iloc[0]) & (blockDF['round']==rid) & (blockDF['reward']==True)  & (blockDF['name'] == trigger) & (blockDF['time'] <= triggerTime) ].sort_values('time', ascending=False).blockId.iloc[0])
                    #create new data frame
                    socialObsDF = pd.DataFrame({'time' : socialRewardObs['Time'],
                        'name' : pid[0:5],
                        'x': [BLOCKID_TO_LOC[loc][0] for loc in socObsLocations],
                        'z' : [BLOCKID_TO_LOC[loc][1] for loc in socObsLocations],
                        'reward': True,
                        'session': subdf['session'].iloc[0],
                        'round': rid,
                        'type':  subdf['type'].iloc[0],
                        'env':  subdf['env'].iloc[0],
                        'id': pid,
                        'blockId' : socObsLocations
                        })
                    socObs = pd.concat([prevObs,socialObsDF]) #Create a new dataframe with both individual and socially observed rewards
                    #Successful player observed from time t-1 to time t
                    successObs = visEvents[(visEvents['Time'] >= time_minus_one)].TriggerPlayer.unique()
                    if (len(successObs)>0):
                        for successfulPlayer in successObs:
                            lastObservation = pvisSub[(pvisSub['target']==successfulPlayer) & (pvisSub['time']<= time) & (pvisSub['time']>= time_minus_one)].sort_values('time', ascending=False).iloc[0]
                            theirLoc = playerDF[(playerDF['name']==successfulPlayer) & (playerDF['session']==subdf.session.iloc[0]) & (playerDF['time']==lastObservation.time)& (playerDF['round'] == rid)] #latest visible location
                            successfulPlayers[successfulPlayer] = (theirLoc.x.iloc[0],theirLoc.z.iloc[0])
                    #visible but not successful player 
                    visPlayers = pvisSub[(pvisSub['name']==pid[0:5]) & (pvisSub['time']<= time) & (pvisSub['time']>= time_minus_one) & (pvisSub['unityVis']==True)].sort_values('time', ascending=False)
                    for visP in visPlayers.target.unique():
                        if visP != pid[0:5]: #if not self
                            if visP not in successfulPlayers.keys():
                                visLoc = playerDF[(playerDF['id']==visP + subdf.session.iloc[0]) & (playerDF['round'] == rid) & (playerDF['time'] == visPlayers[visPlayers['target']==visP].time.iloc[0])] #latest visible location
                                visiblePlayers[visP] = (visLoc.x.iloc[0],  visLoc.z.iloc[0])
                    #all other players
                    nonVis = [player for player in allblocks.name.unique() if (player not in [pid[0:5]]|visiblePlayers.keys()|successfulPlayers.keys())]
                    for nP in nonVis:
                        nloc = playerDF[(playerDF['id'] ==  nP + subdf.session.iloc[0]) & (playerDF['round']==rid) & (playerDF['time']>=time_minus_one) & (playerDF['time']<=time)] 
                        otherPlayers[nP] = (nloc.x.iloc[0],  nloc.z.iloc[0])
                    #update reward dictionaries
                    rewardObs[pid] = dict(zip(prevObs.blockId, prevObs.reward))
                    socrewards[pid] = dict(zip(socObs.blockId, socObs.reward))
            #compute social state
            state = convert_data_to_state(currentState, locDict, gazeDict, rewardObs,socrewards, successfulPlayers, visiblePlayers, otherPlayers) 
        #Update reference points
        time_minus_one = time  
        #Now compute feature DF from the state representations
        featureDF = feature_constructor(agent=pid, state=state, feature_functions=featureSet)
        #add identifiers
        id_cols = ['id', 'type', 'env',  'round',  'session', 'decision_time', 'decision_number', 'chosen']
        #describe chosen block 
        chosen = np.array([np.nan for i in range(400)]) #default to NaN, for blocks that are not available
        chosen[~np.isnan(featureDF['fruit_distance'].tolist())] = False #False for blocks that were available
        chosen[block] = True #set true for the actual destroyed block
        id_vals = [pid, subdf.type.iat[0], subdf.env.iat[0], rid, subdf.session.iat[0], time, t, chosen]
        identifiers = dict(zip(id_cols, id_vals))
        featureDF = featureDF.assign(**identifiers) #add additional identifier columns
        allFeatureDF = pd.concat([allFeatureDF,featureDF])

allFeatureDF.reset_index().to_feather('data/%s/%s.feather' % (batchName,sys.argv[1]))

####################################################
#Testing
###################################################
# from datar.all import *
# from plotnine import *
# #What range of p(reward) do we get?
# #preward=sigmoid((1.0 / np.sqrt(1.0 + np.pi * allFeatureDF.GP_uncertainty / 8)) * allFeatureDF.GP_pred)
# preward=expit(allFeatureDF[allFeatureDF['decision_number']>-1].GP_pred)
# preward.min()
# preward.max()
# n, x, _ = plt.hist(preward,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()

# #Are the prior GP mean and uncertainty correct?
# n, x, _ = plt.hist(allFeatureDF[allFeatureDF['decision_number']==0].GP_pred,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()


# n, x, _ = plt.hist(allFeatureDF[allFeatureDF['decision_number']==0].GP_uncertainty,
#                    histtype=u'step', density=True)  
# plt.plot(x, density(x))
# plt.show()

