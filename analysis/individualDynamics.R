#Dynamic analyses
# 
rm(list=ls()) #house keeping


#load packages
packages <- c('tidyverse','zoo',  'DescTools','brms','arrow')
invisible(lapply(packages, require, character.only = TRUE))

#source('statisticalTests.R')
#source('utilities.R')


theme_set(theme_classic()) #set theme
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette


#Utility function
lag_func <- function(x, k = 1, pad = NA){
  if(k == 0)
    return(x)
  nas <- rep(pad, min(length(x), abs(k)))
  if(k < 0)
    c(tail(x, k), nas) else c(nas, head(x, -k))
}



# # ####################################################################################################
# # # Load and process data
# # ####################################################################################################
# blockDF <- data.frame()
# playerDF <- data.frame()
# #read in data
# 
# #2021 data
# dataFolders <- c('data/2021batch/')
# for (dataFolder in dataFolders){
#   playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
#   blockDF <- rbind(blockDF, arrow::read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
# }
# 
# 
# #add unique ids
# blockDF$id <- paste0(blockDF$name, blockDF$session)
# playerDF$id <- paste0(playerDF$name, playerDF$session)
# 
# #Visibility info
# pvisDF <- readRDS('simData/pvisDF.Rds')
# 
# 
# pvisDF$outId <- paste0(pvisDF$name, pvisDF$session)
# pvisDF$inId <- paste0(pvisDF$target, pvisDF$session)
# 
# #Distance computations
# distanceDF <- readRDS('trajectories/pairwiseDistances.Rds') #Computed in pullAnalysis.R
# distanceDF$id <- paste0(distanceDF$p1, distanceDF$session)
# 
# 
# #Construct new dynamics dataframe
# dynamicsDF <- playerDF %>% select(session, id,env, type,round, time)
# 
# 
# #Organize dataframes in the same order
# dynamicsDF<- dynamicsDF[with(dynamicsDF, order(session, id,env, type,round, time)), ]
# blockDF<- blockDF[with(blockDF, order(session, id,env, type,round, time)), ]
# 
# 
# #add reward and block destructions
# dynamicsDF$reward <- 0
# dynamicsDF$smash <- 0
# for (i in unique(dynamicsDF$id)){
#   cat(i)
#   blocksub <- subset(blockDF, id == i)
#   for (r in unique(blocksub$round)){
#     dynamicsDF[dynamicsDF$id ==i & dynamicsDF$round ==r &   dynamicsDF$time %in% (subset(blocksub, round == r & reward == TRUE)$time), 'reward'] <- 1
#     dynamicsDF[dynamicsDF$id ==i & dynamicsDF$round ==r &   dynamicsDF$time %in% (subset(blocksub, round == r)$time), 'smash'] <- 1
#   }
# }
# 
# #make sure it stays in the same order
# dynamicsDF<- dynamicsDF[with(dynamicsDF, order(session, id,env, type,round, time)), ]
# 
# #Add visibility and distance
# 
# pvisDF <- pvisDF[with(pvisDF, order(session, outId,env, type,round, time)), ] #order by outId
# dynamicsDF$visiblePeers <-  pvisDF %>% group_by(session, outId,env, type, round, time ) %>% dplyr::summarize(visiblePeers = sum(unityVis>0)) %>% pull(visiblePeers)
# 
# pvisDF <- pvisDF[with(pvisDF, order(session, inId,env, type,round, time)), ] #order by inId
# dynamicsDF$observers <-  pvisDF %>% group_by(session, inId,env, type, round, time ) %>% dplyr::summarize(observers = sum(unityVis>0)) %>% pull(observers)
# 
# 
# distanceDF <- distanceDF[with(distanceDF, order(session, id,env, type,round, time)), ] #order by inId
# dynamicsDF$proximity <-  distanceDF %>%group_by(session, id,env, type, round, time ) %>% dplyr::summarize(proximity = mean(1/distance)) %>% pull(proximity)
# 
# #save to disk
# write_feather(dynamicsDF, 'dynamicData/individualDynamics.feather')

####################################################################################################
# Compute time-lagged correlations (Individual level)
####################################################################################################

dynamicsDF <- read_feather('dynamicData/individualDynamics.feather') #load data

timeSeq <- seq(0,120, by=.05)
offsets <- seq(-20,20, by=.05)
numPermutations <- 100

#Cluster id
clusterid <- as.integer(commandArgs(TRUE)[1])
offset <- offsets[clusterid]


dynamicCorrDF <- data.frame()


for (i in unique(dynamicsDF$id)){
  #i <- unique(dynamicsDF$id)[23]
  cat(i)
  blocksub <- subset(dynamicsDF, id == i)
  for (r in unique(blocksub$round)){
    #r <- unique(blocksub$round)[4]
    #Subset data
    subDF <- subset(dynamicsDF, id == i & round == r)
    #Create dataframe for storing results
    corDF <- data.frame(id = i, session = unique(blocksub$session), round = r,  env = unique(subDF$env), type = unique(subDF$type), offset = offset) 
    #compute reward offset
    rewardOffset<- lag_func(subDF$reward ,  as.integer(offset/.05))
    
    #reward ~ vis In
    corDF$rewardVisIn <- FisherZ(cor.test(rewardOffset,subDF$observers)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
      FisherZ(cor.test(rewardOffset,sample(subDF$observers))$estimate)}), na.rm=TRUE)
    
    #reward ~ vis Out
    corDF$rewardVisOut <- FisherZ(cor.test(rewardOffset,subDF$visiblePeers)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
      FisherZ(cor.test(rewardOffset,sample(subDF$visiblePeers))$estimate)}), na.rm=TRUE)
    
    #reward ~ proximity
    corDF$rewardProx <- FisherZ(cor.test(rewardOffset,subDF$proximity)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
      FisherZ(cor.test(rewardOffset,sample(subDF$proximity))$estimate)}), na.rm=TRUE)
    
    #proximity ~ visIn
    proxLagged <- lag_func(subDF$proximity,  as.integer(offset/.05))
    corDF$proxVisIn <- FisherZ(cor.test(proxLagged,subDF$observers)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
      FisherZ(cor.test(proxLagged,sample(subDF$observers))$estimate)}), na.rm=TRUE)
    
    #proximity ~ visOut
    corDF$proxVisOut <- FisherZ(cor.test(proxLagged,subDF$visiblePeers)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
      FisherZ(cor.test(proxLagged,sample(subDF$visiblePeers))$estimate)}), na.rm=TRUE)
    
    #visIn ~ visOut
    visInLagged <- lag_func(subDF$observers,  as.integer(offset/.05))
    corDF$visInOut <- FisherZ(cor.test(visInLagged,subDF$visiblePeers)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
      FisherZ(cor.test(visInLagged,sample(subDF$visiblePeers))$estimate)}), na.rm=TRUE)
    
    #put into data frame
    dynamicCorrDF <- rbind(dynamicCorrDF, corDF)
    
  }
}



saveRDS(dynamicCorrDF, paste0('dynamicData/individual/', clusterid, '.Rds'))

