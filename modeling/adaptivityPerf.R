#Adaptivity and performance
#Charley Wu 2023
rm(list=ls()) #house keeping
#setwd('modeling')

#load packages
packages <- c('ggplot2','GGally', 'trajr', 'ggpubr', 'tidyverse', 'scales',  'cowplot','jsonlite', 'DescTools', 'ggbeeswarm','brms', 
              'sjPlot','ggthemes','ggsignif', 'RColorBrewer',  'GGally', 'network', 'sna', 'igraph')
invisible(lapply(packages, require, character.only = TRUE))


theme_set(theme_classic()) #set theme
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette


####################################################################################################
# Load model weights
####################################################################################################
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#Load model weights
# group rounds

s_ars_conditional <- loadRData("Stanfits/s_conditional_ARS_weights")
names(s_ars_conditional)

#Compute posterior mean 
W <- data.frame(apply(s_ars_conditional$v_ID, c(2,3), mean))
names(W) <- c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful')

#Convert distance weights to proximity, for interpretability
W$Locality <- W$Locality * -1
W$`Successful Proximity` <- W$`Successful Proximity` * -1
W$`Unsuccessful Proximity` <- W$`Unsuccessful Proximity` * -1
W$`Adapt. Locality` <- W$`Adapt. Locality` * -1
W$`Adapt. Successful` <- W$`Adapt. Successful` * -1
W$`Adapt. Unsuccessful` <- W$`Adapt. Unsuccessful` * -1


#Id order from compileModelData.py
W$id <- c('MPIB2session1.json', 'MPIB3session1.json', 'MPIB4session1.json', 'MPIB1session1.json', 'MPIB3session10.json', 'MPIB2session10.json', 'MPIB4session10.json', 'MPIB1session10.json', 'MPIB4session11.json', 'MPIB3session11.json', 'MPIB2session11.json', 'MPIB1session11.json', 'MPIB4session12.json', 'MPIB3session12.json', 'MPIB2session12.json', 'MPIB1session12.json', 'MPIB4session13.json', 'MPIB2session13.json', 'MPIB1session13.json', 'MPIB3session13.json', 'MPIB4session14.json', 'MPIB1session14.json', 'MPIB3session14.json', 'MPIB2session14.json', 'MPIB2session15.json', 'MPIB1session15.json', 'MPIB4session15.json', 'MPIB3session15.json', 'MPIB3session16.json', 'MPIB1session16.json', 'MPIB4session16.json', 'MPIB2session16.json', 'MPIB2session17.json', 'MPIB4session17.json', 'MPIB3session17.json', 'MPIB1session17.json', 'MPIB4session18.json', 'MPIB1session18.json', 'MPIB2session18.json', 'MPIB3session18.json', 'MPIB4session19.json', 'MPIB2session19.json', 'MPIB1session19.json', 'MPIB3session19.json', 'MPIB3session2.json', 'MPIB2session2.json', 'MPIB4session2.json', 'MPIB1session2.json', 'MPIB3session20.json', 'MPIB4session20.json', 'MPIB1session20.json', 'MPIB2session20.json', 'MPIB3session21.json', 'MPIB1session21.json', 'MPIB4session21.json', 'MPIB2session21.json', 'MPIB2session22.json', 'MPIB3session22.json', 'MPIB1session22.json', 'MPIB4session22.json', 'MPIB1session23.json', 'MPIB4session23.json', 'MPIB2session23.json', 'MPIB3session23.json', 'MPIB2session24.json', 'MPIB1session24.json', 'MPIB4session24.json', 'MPIB3session24.json', 'MPIB2session25.json', 'MPIB1session25.json', 'MPIB3session25.json', 'MPIB4session25.json', 'MPIB1session26.json', 'MPIB3session26.json', 'MPIB2session26.json', 'MPIB4session26.json', 'MPIB1session27.json', 'MPIB2session27.json', 'MPIB3session27.json', 'MPIB4session27.json', 'MPIB3session28.json', 'MPIB1session28.json', 'MPIB2session28.json', 'MPIB4session28.json', 'MPIB3session29.json', 'MPIB1session29.json', 'MPIB4session29.json', 'MPIB2session29.json', 'MPIB4session3.json', 'MPIB2session3.json', 'MPIB3session3.json', 'MPIB1session3.json', 'MPIB4session30.json', 'MPIB2session30.json', 'MPIB1session30.json', 'MPIB3session30.json', 'MPIB1session31.json', 'MPIB3session31.json', 'MPIB4session31.json', 'MPIB2session31.json', 'MPIB3session32.json', 'MPIB2session32.json', 'MPIB1session32.json', 'MPIB4session32.json', 'MPIB2session4.json', 'MPIB3session4.json', 'MPIB1session4.json', 'MPIB4session4.json', 'MPIB4session5.json', 'MPIB2session5.json', 'MPIB1session5.json', 'MPIB3session5.json', 'MPIB2session6.json', 'MPIB4session6.json', 'MPIB1session6.json', 'MPIB3session6.json', 'MPIB3session7.json', 'MPIB2session7.json', 'MPIB1session7.json', 'MPIB4session7.json', 'MPIB3session8.json', 'MPIB4session8.json', 'MPIB2session8.json', 'MPIB1session8.json', 'MPIB4session9.json', 'MPIB1session9.json', 'MPIB3session9.json', 'MPIB2session9.json')

#Pivot
#W <- W %>% pivot_longer(cols = c('Locality', 'BlockVis', 'GP pred', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt Locality', 'Adapt Successful', 'Adapt Unsuccessful'), names_to = 'Feature', values_to = 'Weights')
#W$Feature <- factor(W$Feature, levels = c('Locality', 'BlockVis', 'GP pred', 'Adapt Locality', 'Successful Proximity', 'Unsuccessful Proximity',  'Adapt Successful', 'Adapt Unsuccessful'))

#Duplicate for env; individual offsets computed across envs
Wrand <- W
Wsmooth<- W
Wrand$env <- 'random'
Wsmooth$env <- 'smooth'
W <-rbind(Wrand,Wsmooth)
####################################################################################################
# Load behavioral data
####################################################################################################
setwd('../analysis')
source('statisticalTests.R')
source('utilities.R')

blockDF <- data.frame()
playerDF <- data.frame()
#read in data

#2021 data
dataFolders <- c('data/2021batch/')   
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
  blockDF <- rbind(blockDF, arrow::read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
}


#add unique ids
blockDF$id <- paste0(blockDF$name, blockDF$session) 
playerDF$id <- paste0(playerDF$name, playerDF$session) 

#Convert playerDF into long format
visDF <- playerDF %>% dplyr::filter(type == 'group') %>% pivot_longer(cols = MPIB4_visible:MPIB1_visible,  names_to = 'target', values_to = 'serverVis')
visDF$target <- factor(visDF$target)
levels(visDF$target) <- c("MPIB1","MPIB2", "MPIB3", "MPIB4")

#Visibility info
pvisDF <- readRDS('simData/pvisDF.Rds') 
evisDF <- readRDS('simData/evisDF.Rds')

#Join pvis with visDF
#Compute number of visible players
socialD <- pvisDF %>% filter(type=='group') %>% group_by(session, name, type, env) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
socialD$id <- paste0(socialD$name, socialD$session)
#Now add reward and num blocks destroyed from blockDF
blockInfo<- blockDF %>% dplyr::filter(type == 'group') %>% group_by(session, name, type, env) %>% dplyr:: summarize(reward = sum(reward)/4, blocksDestroyed = n()/4) 
socialD$reward <- blockInfo$reward
socialD$blocksDestroyed <- blockInfo$blocksDestroyed

#Vis network
visNetworkDF <- readRDS('networks/visNet.Rds')
visNetworkDF$env <- factor(visNetworkDF$env, levels = c('random', 'smooth'))

#Distance network
distanceNetworkDF <- readRDS('networks/distNet.Rds')
#Calculate average per participant
distPlayerDF <- distanceNetworkDF %>% group_by(player, session, env) %>% dplyr::summarize(EC = mean(EC), score = mean(score)) 
distPlayerDF$env <- factor(distPlayerDF$env, levels=c('random', 'smooth'))


#Leadership and pulls
eventDF <- readRDS(paste0('events/pullsFiltered.0.1.Rds'))
#Compute the mean number of events in each round
allCombs <- expand.grid(env = unique(playerDF$env), session = unique(playerDF$session), roundtype = unique(playerDF$type), type = c('anchor', 'pull'), round = c(1,2,3,4)) #Account for missing combinations of session x round
allCombs$events <- 0 #placeholder
#do a simple plot fo the mean events ± SD
eventCounts <- eventDF %>% group_by(env, session, roundtype, type, round) %>% dplyr::summarize(events = n()) #count events in each round
eventCounts$round <- eventCounts$round+1 #convert to base 1
eventCounts$round <- ifelse(eventCounts$round >4,  eventCounts$round - (4 *(floor((eventCounts$round-.000001)/ 4))), eventCounts$round) #convert to 1-4 counting
for (i in 1:nrow(eventCounts)){
  row <- eventCounts[i,]
  allCombs[allCombs$env==row$env & allCombs$session == row$session & allCombs$roundtype == row$roundtype & allCombs$type == row$type & allCombs$round == row$round, 'events'] <- row$events #update based on the number of events
}
#Compute mean and sd
meanEventDF <- allCombs%>% group_by(env, roundtype, type) %>% dplyr::summarize(meanEvents = sum(events)/(length(unique(eventDF$session))*4), n = length(events), sdEvents = sd(c(events, rep(0,length(unique(eventDF$session))*4 - length(events) ))))
meanEventDF$seEvents <-meanEventDF$sdEvents / sqrt(meanEventDF$n) #standard error
#meanEventDF$env <- factor(meanEventDF$env, levels = c('random', 'smooth'))


#Count the number of times each player was a leader or a follower
eventDF$session <- factor(eventDF$session)
eventDF$leader <- factor(eventDF$leader)
eventDF$follower <- factor(eventDF$follower)

#Relate leadership to score and in/out degree
leaderShipDF <- data.frame()
envs <-c('smooth', 'random') #Only look at smooth, since there are too few data points in random
for (envType in envs){ #loop through env type
  leaderCounts <- eventDF %>% dplyr::filter(env == envType & type == 'pull' & roundtype == 'group') %>% select(session, leader) %>% table
  followerCounts <- eventDF %>% dplyr::filter(env == envType & type == 'pull' & roundtype == 'group') %>% select(session, follower) %>% table
  leaderShip <- leaderCounts - followerCounts #positive when leader more than follower
  leaderProb <- leaderCounts / rowSums(leaderCounts)
  followerProb <- followerCounts / rowSums(followerCounts)
  leaderIndex = leaderProb - followerProb
  leaderDF <- as.data.frame(leaderIndex) #convert to dataframe
  names(leaderDF) <- c('session', 'name', 'leadership')
  leaderDF$name <- factor(paste0('MPIB', leaderDF$name))
  #compute average score for each group
  groupScoreDF <- blockDF %>% dplyr::filter(env == envType & type  == 'group') %>% group_by(session) %>% dplyr::summarize(avgScore = sum(reward)/16)
  #Compute score for each player
  playerScoreDF <- blockDF %>% dplyr::filter(env == envType & type  == 'group') %>% group_by(session, name) %>% dplyr::summarize(avgScore = sum(reward)/4)
  playerScoreDF$groupScore <- rep(groupScoreDF$avgScore, each = 4)
  playerScoreDF$relScore <- playerScoreDF$avgScore - playerScoreDF$groupScore
  #Put together
  leaderDF <- merge(leaderDF, playerScoreDF, by = c('session', 'name'))
  leaderDF$env <- envType
  leaderShipDF <- rbind(leaderShipDF, leaderDF)
}
leaderShipDF$id <- paste0(leaderShipDF$name, leaderShipDF$session)


setwd('../cogmodeling')

####################################################################################################
# Combine data together
####################################################################################################

df <- merge(W, leaderShipDF, by = c('id','env')) #weights and leadership
df <- merge(df, socialD, by = c('id','env')) #weights and basic social/behavioral data


#Visibility network
visNetworkDF$id <- paste0(visNetworkDF$player,visNetworkDF$session)
visNetworkDF <- visNetworkDF %>% group_by(id, env) %>% dplyr::summarize(inWeight = mean(inWeight), outWeight = mean(outWeight))
df <- merge(df, visNetworkDF, by=c('id', 'env'))

#Proximity network
distanceNetworkDF$id <- paste0(distanceNetworkDF$player,distanceNetworkDF$session)
distanceNetworkDF <- distanceNetworkDF %>% group_by(id, env) %>% dplyr::summarize(Centrality = mean(EC))
df <- merge(df, distanceNetworkDF, by=c('id', 'env'))


df$env <- factor(df$env, levels = c('smooth', 'random'))
####################################################################################################
# Plots: Weights and performance
####################################################################################################
#Overview
#names(df)
#pOverview <- ggpairs(subset(df, env=='smooth')[, c(14, 16, 15, 3,4,5,6,7,8,9,10,13, 20,23,24,25)])
#ggsave('plots/weightsPerf.pdf',pOverview, width = 24, height = 24, units = 'in')


df <- df %>% pivot_longer(cols = c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful'), names_to = 'Feature', values_to = 'Weights')
df$Feature <- factor(df$Feature, levels =c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful'))


#AvgScore
pScore <- ggscatter(df, x = 'Weights', y = 'avgScore', size=.7,
          color = 'env', add='reg.line', conf.int=TRUE, facet.by='Feature', alpha = 0.8)+
          stat_cor(aes(color = env), label.y = c(25, 22), p.accuracy = 0.001, r.accuracy = 0.01)+
          theme_classic()+
          xlab('Individual Weight Offset')+
          ylab(' Avg. Score')+
          guides(color = guide_legend(override.aes = aes(label = ""))) +
          coord_cartesian(ylim=c(0, 25))+
          scale_fill_manual(values = rev(c("#E69F00","#009E73")), name="Environment")+
          scale_color_manual(values =rev(c("#E69F00","#009E73")), name="Environment")+
          facet_wrap(~Feature, scales = "free", nrow = 2)+
          theme(legend.position = 'top', strip.background=element_blank(),legend.background=element_blank(), legend.spacing.y = unit(0.01, 'cm'),legend.margin=margin(),strip.text = element_text(hjust = 0))

pScore

#Centrality
pCentrality <- ggscatter(df, x = 'Weights', y = 'Centrality', size=.7,
                    color = 'env', add='reg.line', conf.int=TRUE, facet.by='Feature', alpha = 0.8)+
  stat_cor(aes(color = env), label.y = c(1.05, 1.01), p.accuracy = 0.001, r.accuracy = 0.01)+
  theme_classic()+
  xlab('Individual Weight Offset')+
  ylab('Centrality (ProximityNet)')+
  guides(color = guide_legend(override.aes = aes(label = ""))) +
  #coord_cartesian(ylim=c(0.7,1.1))+
  scale_fill_manual(values = rev(c("#E69F00","#009E73")), name="Environment")+
  scale_color_manual(values =rev(c("#E69F00","#009E73")), name="Environment")+
  facet_wrap(~Feature, scales = "free", nrow = 2)+
  theme(legend.position = 'top', strip.background=element_blank(),legend.background=element_blank(), legend.spacing.y = unit(0.01, 'cm'),legend.margin=margin(),strip.text = element_text(hjust = 0))

pCentrality


#Out degree
pOut <- ggscatter(df, x = 'Weights', y = 'outWeight', size=.7,
                         color = 'env', add='reg.line', conf.int=TRUE, facet.by='Feature', alpha = 0.8)+
  stat_cor(aes(color = env), label.y = c(1.4, 1.27), p.accuracy = 0.001, r.accuracy = 0.01)+
  theme_classic()+
  xlab('Individual Weight Offset')+
  ylab('Out-degree (VisNet)')+
  guides(color = guide_legend(override.aes = aes(label = ""))) +
  #coord_cartesian(ylim=c(0.7,1.1))+
  scale_fill_manual(values = rev(c("#E69F00","#009E73")), name="Environment")+
  scale_color_manual(values =rev(c("#E69F00","#009E73")), name="Environment")+
  facet_wrap(~Feature, scales = "free", nrow = 2)+
  theme(legend.position = 'top', strip.background=element_blank(),legend.background=element_blank(), legend.spacing.y = unit(0.01, 'cm'),legend.margin=margin(),strip.text = element_text(hjust = 0))

pOut


#save plots
ggsave('plots/adaptivityScore.pdf', pScore, width = 12, height = 6, units= 'in')
ggsave('plots/adaptivityCentrality.pdf', pCentrality, width = 12, height = 6, units= 'in')
ggsave('plots/adaptivityVis.pdf', pOut, width = 12, height = 6, units= 'in')

