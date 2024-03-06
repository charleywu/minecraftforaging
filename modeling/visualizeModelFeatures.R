#Visualize model features
#Charley Wu, 2023

rm(list=ls()) #house keeping


#load packages
packages <- c('ggplot2', 'trajr', 'pryr', 'rstatix', 'gganimate','pdist','TTR', 'signal', 'zoo', 'cowplot','jsonlite','lubridate', 'ggimage','ggbeeswarm', 
              'ggthemes','ggsignif', 'feather', 'gifski', 'RColorBrewer',  'gmodels', 'Hmisc', 'ggExtra', 'sjstats',
              'tidyverse', 'lme4', 'brms', 'sjPlot', 'sjmisc', 'coefplot', 'infotheo', 'SimilarityMeasures')
invisible(lapply(packages, require, character.only = TRUE))



source('../analysis/statisticalTests.R') #load stats
source('../analysis/utilities.R') #load stats

theme_set(theme_cowplot()) #set theme
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette

####################################################################################################
# Load behavioral data 
####################################################################################################
blockDF <- data.frame()
playerDF <- data.frame()
#read in data

#2021 data
dataFolders <- c('../analysis/data/2021batch/')   #Pilot data, first 2 sessions, sessions 3-7
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
  blockDF <- rbind(blockDF, arrow::read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
}


#add unique ids
blockDF$id <- paste0(blockDF$name, blockDF$session) 
playerDF$id <- paste0(playerDF$name, playerDF$session) 

####################################################################################################
# Load visibility data 
####################################################################################################
#Visibility data
pvisDF <- readRDS('../analysis/simData/pvisDF.Rds')
evisDF <- readRDS('../analysis/simData/evisDF.Rds')

####################################################################################################
# Load model weights
####################################################################################################

modelWeights <- read_csv('data/modelWeights/allPostMeans.csv') #computed in modelPlots
modelWeights <- modelWeights[,-1] #remove row names
modelWeights <- subset(modelWeights , model == 'ARS+Cond' & env=='smooth' ) #Subset to relevant model and features

meanWeights <- na.omit(modelWeights %>% arrange(factor(weights, levels = c('Locality', 'BlockVis', 'GP pred', 'Successful Proximity', 'Unsuccessful Proximity')))  %>% select(est))
meanWeights <- as.numeric(unlist(meanWeights[1:5,1])) #select the non adaptive weights
meanWeights <- meanWeights * c(-1,1,1,-1,-1) #convert proximity weights back to distance weights for plotting, by multiplying with -1
####################################################################################################
# Plots
####################################################################################################

# Plot one characteristic game
plotGame <- function(sampleSession, focusPlayer, repNum, roundProgression, shrinkFactor = -0.2){
  targetPlayer <- paste0('MPIB', focusPlayer, sampleSession)  #convert to participant Id
  #Which feature data to load (computed in compileModelData.py)
  pids <-match(unique(subset(blockDF, session ==sampleSession)$id), unique(blockDF$id)) - 1 #-1 for conversion to pythonic base 0 
  
  #Load feature filesdata
  dir_path <- 'data/modelFeatures' #data directory
  datalist <- list.files(path=dir_path)
  mdf <- data.frame()
  for (pid in pids){
    filename <- paste0(dir_path,'/', sprintf("%03d",pid), '.feather')
    if (file.exists(filename)){
      participantDF  <- arrow::read_feather(filename)
      mdf <- rbind(mdf, participantDF)
    }else{
      print(filename)
    }
  }
  
  # Create round data
  exampleDf <-  playerDF %>% dplyr::filter(type=="group" & session== sampleSession & env=="smooth")
  targetRound <- unique(exampleDf$round)[repNum]
  featureDF <- subset(mdf,round == targetRound) #subset model features
  
  #Find set of decisions where social features are present
  validDecisions <- featureDF %>% dplyr::filter( id == targetPlayer & !is.na(successful_player_distance) & !is.na(visible_player_distance)) %>% select(decision_number) %>% unique() %>% unlist() #consider only decisions where there is also successful player observations
  
  if (length(validDecisions)<1){
    return(NULL)
  }
  else{
    targetChoice <- validDecisions[as.integer(length(validDecisions) * roundProgression)] #select a choice we want to model midway through the round
    #Time points: t-1 to time t is used to make predictions about choice t+1
    cat(paste(targetRound, targetChoice)) #Print
    #note that all target choices are incremented by 1 to switch from base 0 to base 1 indexing
    time_t_plus_one <- unique(subset(featureDF, id == targetPlayer)$decision_time)[targetChoice+2]
    timepoint <- unique(subset(featureDF, id == targetPlayer)$decision_time)[targetChoice+1] #Current time, where we stop accumulating information for predicting the next choice
    time_t_minus_one <- unique(subset(featureDF, id == targetPlayer)$decision_time)[targetChoice] #previous time, where we begin accumulating information
    
    #first recreate the game field 
    exampleDf <- exampleDf %>% dplyr::filter(round==targetRound)
    exampleDf$name <- factor(exampleDf$name)
    #exampleDf$zlook <- -exampleDf$zlook
    exampleDf$angle <-sapply(1:nrow(exampleDf), FUN=function(t){atan2(exampleDf$zlook[t], exampleDf$xlook[t])}) 
    levels(exampleDf$name) <-c('P1', 'P2', 'P3', 'P4')
    #Find out which env it corresponds to
    session <- read_json(paste0('../analysis/sessions/2021batch/', sampleSession))
    env <- read.csv(paste0('../environments/', session$roundData[[targetRound+2]]$envFile), sep = ';') #plus 2 is to account for training rounds
    
    initial <- expand.grid(x = seq(2,59, length.out = 20), z = seq(2,59, length.out = 20))
    
    
    #Now plot the feature values for a given player at one point in time
    pSnapshot <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = subset(blockDF,type == 'group' & session == sampleSession & round== targetRound & time<=timepoint), aes(fill = reward),size=3, shape = 22, color = 'white') +
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      geom_path(aes(alpha=time, color = name), size = 1)+
      geom_spoke(data = subset(exampleDf, time ==timepoint), aes(angle=angle, color=name), radius = 5, arrow=arrow(length = unit(0.1,"cm")), size = 1)+
      geom_image( data=subset(exampleDf, time==timepoint),  image="../images/minecraft.png", size=.05 , by="height")+
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'), name ='Participant')+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle(paste0('Snapshot at ', timepoint,'s'))+
      scale_fill_manual(values=c('white', '#0096FF'), name ='Reward')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides( alpha = 'none')
    pSnapshot
    
    #Block visibility
    pBlockVisible <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = subset(featureDF, decision_number==targetChoice & id == targetPlayer), aes(fill = fruit_pov),size=3, shape = 22, color = 'white') +
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      geom_path(data=subset(exampleDf, time<=timepoint & time>=time_t_minus_one & name == paste0('P', focusPlayer)), aes(alpha=time, color = name), size = 1)+
      geom_spoke(data = subset(exampleDf, time ==timepoint  & name== paste0('P', focusPlayer)), aes(angle=angle, color=name), radius = 5, arrow=arrow(length = unit(0.1,"cm")), size = 1)+
      geom_image( data=subset(exampleDf, time==timepoint &  name==paste0('P', focusPlayer)),  image="../images/minecraft.png", size=.05 , by="height")+
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999')[focusPlayer])+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle('Block Visibility')+
      scale_fill_viridis_c(na.value="white", name = 'f(local)')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides(colour = "none", alpha = 'none')
    pBlockVisible
    
    
    #Locality feature
    pLocality <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = subset(featureDF, decision_number==targetChoice & id == targetPlayer), aes(fill = -fruit_distance),size=3, shape = 22, color = 'white') +
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      geom_image( data=subset(exampleDf, time==timepoint & name == paste0('P', focusPlayer)),  image="../images/minecraft.png", size=.05 , by="height")+
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle('Locality')+
      scale_fill_viridis_c(na.value="white", name = 'f(local)')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides(colour = "none", alpha = 'none')
    pLocality
    
    
    
    
    #GP pred
    pGPpred <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = subset(featureDF, decision_number==targetChoice  & id == targetPlayer), aes(fill = asocial_GP_pred),size=3, shape = 22, color = 'white') +
      geom_point(data = subset(blockDF,type == 'group' & session == sampleSession & round== targetRound & time<=timepoint & id == targetPlayer & reward==TRUE ), fill ='#0096FF', size=3, shape = 22, color = 'white') +
      geom_point(data = subset(blockDF,type == 'group' & session == sampleSession & round== targetRound & time<=timepoint & id == targetPlayer & reward==FALSE ), fill ='white', size=3, shape = 22, color = 'black') +
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      # geom_path(aes(alpha=time, color = id), size = 1)+
      # geom_image( data=subset(exampleDf, time==timepoint),  image="../images/minecraft.png", size=.05 , by="height")+
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle('Reward Prediction')+
      scale_fill_viridis_c(na.value="white", name = 'E(reward)')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides(colour = "none", alpha = 'none')
    pGPpred
    
    #GP uncertainty
    # pGPvar <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
    #   geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
    #   geom_point(data = subset(featureDF, decision_number==targetChoice  & id == targetPlayer), aes(fill=GP_uncertainty),size=3, shape = 22, color = 'white') +
    #   coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
    #   geom_path(aes(alpha=time, color = id), size = 1)+
    #   geom_image( data=subset(exampleDf, time==timepoint),  image="../images/minecraft.png", size=.05 , by="height")+
    #   scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
    #   #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
    #   ggtitle('')+
    #   theme_void()+
    #   scale_alpha_continuous(range = c(0.3, 1))+
    #   coord_fixed()+
    #   ggtitle('GP Uncertainty')+
    #   scale_fill_viridis_c(na.value="white", name = 'V(reward)')+
    #   #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
    #   theme(legend.position = 'right')+
    #   guides(colour = "none", alpha = 'none')
    # pGPvar
    
    #Compute which players were visible
    timerange <-  c(time_t_minus_one, timepoint)
    vis <- subset(pvisDF,type=="group" & session== sampleSession & round == targetRound & env=="smooth" & name == paste0('MPIB', focusPlayer) & time >= timerange[1] & time<= timerange[2])
    visiblePlayers <- vis %>% filter(unityVis==TRUE) %>% group_by(target) %>% dplyr::summarize(visible = sum(unityVis)>0) %>% select(target)
    
    #Compute which players were observed as successful
    evis <- subset(evisDF, eventtype=="SPL" & session== sampleSession &  round == targetRound & PlayerName == paste0('MPIB', focusPlayer) &TriggerPlayer != paste0('MPIB', focusPlayer) & Time >= timerange[1] & Time<= timerange[2])
    successfulPlayers <- evis %>% group_by(TriggerPlayer)%>% dplyr::summarize(visible = sum(Occupancy)>0) %>% select(TriggerPlayer)
    
    #Construct ids
    successIds <- sapply(intersect(successfulPlayers$TriggerPlayer, visiblePlayers$target),FUN=function(i) paste0(i,sampleSession)) #which players were visible
    VisTimePoints <- vis %>% dplyr::filter(time >= timerange[1] & time<= timerange[2] & unityVis == TRUE) %>% group_by(target) #visible time points
    successDF <-  subset(exampleDf,  id %in% successIds & time>=timerange[1] & time<=timerange[2])
    successTimeDF <- data.frame()
    for (pid in successIds){
      timeVec <- subset(VisTimePoints,target==substr(pid,1,5))$time
      preciseVisDF <- subset(successDF, id == pid & time %in% timeVec)
      preciseVisDF$lastSeen <- FALSE
      preciseVisDF[nrow(preciseVisDF), 'lastSeen'] <- TRUE
      successTimeDF <- rbind(successTimeDF, preciseVisDF)
    }
    
    #Success
    psuccess <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = subset(featureDF, decision_number==targetChoice  & id == targetPlayer), aes(fill=-successful_player_distance),size=3, shape = 22, color = 'white') + #blocks
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      #geom_path(aes(alpha=time, color = id), size = 1)+
      geom_path(data=successTimeDF, aes(alpha=time, color = name), size = 1)+ 
      geom_image(data=subset(successTimeDF, lastSeen==TRUE),  image="../images/minecraft.png", size=.05 , by="height")+ #TODO plot only those who are visible + successful
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle('Successful Proximity')+
      scale_fill_viridis_c(na.value="white", name = 'f(success)')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides(colour = "none", alpha = 'none')
    psuccess
    
    #Visible but without observation of success
    unsuccessfulIds <- sapply(setdiff(visiblePlayers$target,successfulPlayers$TriggerPlayer),FUN=function(i) paste0(i,sampleSession))
    unsuccessDF <-  subset(exampleDf,  id %in% unsuccessfulIds & time>=timerange[1] & time<=timerange[2])
    unsuccessTimeDF <- data.frame()
    for (pid in as.vector(unsuccessfulIds)){
      timeVec <- subset(VisTimePoints,target==substr(pid,1,5))$time
      preciseVisDF <- subset(unsuccessDF, id == pid & time %in% timeVec)
      preciseVisDF$lastSeen <- FALSE
      preciseVisDF[nrow(preciseVisDF), 'lastSeen'] <- TRUE
      unsuccessTimeDF <- rbind(unsuccessTimeDF, preciseVisDF)
    }
    
    #visible player distance
    pNoSuccess <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = subset(featureDF, decision_number==targetChoice  & id == targetPlayer), aes(fill=-visible_player_distance),size=3, shape = 22, color = 'white') +
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      geom_path(data=unsuccessTimeDF, aes(alpha=time, color = name), size = 1)+
      geom_image(data=subset(unsuccessTimeDF, lastSeen==TRUE),  image="../images/minecraft.png", size=.05 , by="height")+#TODO plot only those who are visible + successful
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle('Unsuccessful Proximity')+
      scale_fill_viridis_c(na.value="white", name = 'f(NoSuccess)')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides(colour = "none", alpha = 'none')
    pNoSuccess
    
    #non visible player distance
    # pNovisibility <- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
    #   geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
    #   geom_point(data = subset(featureDF, decision_number==targetChoice  & id == targetPlayer), aes(fill=-other_player_distance),size=3, shape = 22, color = 'white') +
    #   coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
    #   geom_path(aes(alpha=time, color = id), size = 1)+
    #   geom_image( data=subset(exampleDf, time==timepoint),  image="../images/minecraft.png", size=.05 , by="height")+
    #   scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
    #   #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
    #   ggtitle('')+
    #   theme_void()+
    #   scale_alpha_continuous(range = c(0.3, 1))+
    #   coord_fixed()+
    #   ggtitle('Non visible player distance')+
    #   scale_fill_viridis_c(na.value="white", name = 'f(notVisible)')+
    #   #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
    #   theme(legend.position = 'right')+
    #   guides(colour = "none", alpha = 'none')
    # pNovisibility
    
    
    
    #Extract feature weights
    
    modelPreds <- subset(featureDF, decision_number==targetChoice & id == targetPlayer) #features for target player
    
    modelPreds$weightedFeats <-  as.matrix(modelPreds[,c('fruit_distance', 'fruit_pov', 'asocial_GP_pred', 'successful_player_distance', 'visible_player_distance')]) %*% meanWeights
    modelPreds$probChoice <- exp(modelPreds$weightedFeats)
    modelPreds$probChoice <- modelPreds$probChoice/sum(modelPreds$probChoice, na.rm=T)
    
    actualChoice <- modelPreds %>% dplyr::filter(chosen == TRUE) %>% select(x,z)
    
    #weighted model features
    pWeighted<- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
      geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
      geom_point(data = modelPreds, aes(fill=weightedFeats),size=3, shape = 22, color = 'white') +
      coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
      # geom_path(aes(alpha=time, color = id), size = 1)+
      # geom_image( data=subset(exampleDf, time==timepoint),  image="../images/minecraft.png", size=.05 , by="height")+
      geom_point(data = actualChoice, color='#D81B60',size=3, shape = 4) +
      scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
      #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
      ggtitle('')+
      theme_void()+
      scale_alpha_continuous(range = c(0.3, 1))+
      coord_fixed()+
      ggtitle('Model Predictions')+
      scale_fill_viridis_c(na.value="white", name = 'V(choice)')+
      #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
      theme(legend.position = 'none')+
      guides(colour = "none", alpha = 'none')
    
    pWeighted
    # 
    # #Choice Probs
    # pChoice<- ggplot(subset(exampleDf, time<=timepoint), aes( x=x,y=z)) +
    #   geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') + #initial layout of grid
    #   geom_point(data = modelPreds, aes(fill=probChoice),size=3, shape = 22, color = 'white') +
    #   coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
    #   geom_path(aes(alpha=time, color = id), size = 1)+
    #   geom_image( data=subset(exampleDf, time==timepoint),  image="../images/minecraft.png", size=.05 , by="height")+
    #   geom_point(data = actualChoice, color='#D81B60',size=3, shape = 4) +
    #   scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
    #   #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
    #   ggtitle('')+
    #   theme_void()+
    #   scale_alpha_continuous(range = c(0.3, 1))+
    #   coord_fixed()+
    #   ggtitle('Model Predictions')+
    #   scale_fill_viridis_c(na.value="white", name = 'P(choice)')+
    #   #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
    #   theme(legend.position = 'right')+
    #   guides(colour = "none", alpha = 'none')
    
    pModelFeatures <- cowplot::plot_grid(pLocality, NULL, pBlockVisible, NULL, pGPpred, psuccess, NULL, pNoSuccess, NULL, pWeighted, nrow = 2,
                                          rel_widths = c(1, shrinkFactor, 1, shrinkFactor,1, 1, shrinkFactor,1, shrinkFactor,1))
    pFinal <- cowplot::plot_grid(pSnapshot, pModelFeatures, nrow = 1, rel_widths = c(1,  3), labels = c('a',''))
    
    return(pFinal)
  }
  
}



# pExample <- plotGame(sampleSession = 'session26.json', focusPlayer = 2, repNum = 4, roundProgression = .8)
# pExample

#Good examples
pExample <- plotGame(sampleSession = 'session10.json', focusPlayer = 4, repNum = 4, roundProgression = .9, shrinkFactor=-.5)
#pExample <- plotGame(sampleSession = 'session6.json', focusPlayer = 2, repNum = 4, roundProgression = .8, shrinkFactor=-.4)
#pExample <- plotGame(sampleSession = 'session24.json', focusPlayer = 4, repNum = 4, roundProgression = .8, shrinkFactor=0)
#pExample <- plotGame(sampleSession = 'session30.json', focusPlayer = 4, repNum = 4, roundProgression = .7, shrinkFactor=0)

pExample

ggsave('plots/modelFeatures.pdf', pExample, width = 13, height = 5, units = 'in')

####Other examples


# 
# plotGame(sampleSession = 'session26.json', focusPlayer = 1, repNum = 4, roundProgression = 1, shrinkFactor=0)
# plotGame(sampleSession = 'session3.json', focusPlayer = 3, repNum = 4, roundProgression = 1, shrinkFactor=0)
# plotGame(sampleSession = 'session20.json', focusPlayer =4, repNum = 4, roundProgression = 1, shrinkFactor=0)

# #determine session with median performance
# sessionPerf <- blockDF %>% group_by(session) %>% summarize(totalReward = sum(reward))
# #hist(sessionPerf$totalReward)
# sampleSession <- unlist(sessionPerf[which.max(-abs(sessionPerf$totalReward-median(sessionPerf$totalReward) )),'session']) #Select the session with the closest avg. performance to the median across sessions


