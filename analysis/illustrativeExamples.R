#Trajectories
rm(list=ls()) #house keeping

#load packages
packages <- c('ggplot2', 'trajr', 'pryr', 'rstatix', 'gganimate','pdist','TTR', 'signal', 'zoo', 'cowplot','jsonlite','lubridate', 'ggimage','ggbeeswarm', 
              'ggthemes','ggsignif', 'feather', 'gifski', 'RColorBrewer',  'gmodels', 'Hmisc', 'ggExtra', 'sjstats',
              'tidyverse', 'lme4', 'brms', 'sjPlot', 'sjmisc', 'coefplot', 'infotheo', 'SimilarityMeasures', 'jsonlite', 'viridis')
invisible(lapply(packages, require, character.only = TRUE))


source('statisticalTests.R') #load stats
source('utilities.R') #load stats

theme_set(theme_cowplot()) #set theme
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette



####################################################################################################
# Load screen recording data
####################################################################################################
blockDF <- data.frame()
playerDF <- data.frame()
#read in data

dataFolders <- c('data/recording3/')   #Load screen recording data
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, read_delim(paste0(dataFolder, 'recording3_players.log.csv'), delim=';'))
  blockDF <- rbind(blockDF, read_delim(paste0(dataFolder, 'recording3_blocks.log.csv'), delim=';'))
}
#rename columns
names(playerDF) <- c("time", "name", "x", "z", "xlook", "ylook", "zlook", "session" , '') 
names(blockDF) <- c("time", "name", "x", "z", 'reward', '') 

#Compute angle
# playerDF$xlook <- -playerDF$xlook
playerDF$zlook <- -playerDF$zlook

playerDF$angle <-sapply(1:nrow(playerDF), FUN=function(t){atan2(playerDF$zlook[t], playerDF$xlook[t])}) 
playerDF$time <- as.numeric(playerDF$time)
########################################
#Plot a round from screen recording
#############################################
initial <- expand.grid(x = seq(2,59, length.out = 20), z = seq(2,59, length.out = 20))

snapshot <-89.60 #Which timepoint to use

exampleRound <- ggplot(subset(playerDF, time <=snapshot), aes( x=x,y=z)) +
  geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') +
  geom_point(data = subset(blockDF,time <=snapshot), aes(fill = reward),size=3, shape = 22, color = 'white') +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
  geom_point(data = subset(playerDF, time ==0), aes(color = name), size = 2, shape = 4)+
  geom_path(aes(alpha=time, color = name), size = 1)+
  geom_spoke(data = subset(playerDF, time ==snapshot), aes(angle=angle, color=name), radius = 5, arrow=arrow(length = unit(0.1,"cm")), size = 1)+
  geom_image(data=subset(playerDF, time ==snapshot),  image="../images/minecraft.png", size=.05 , by="height")+
  scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
  #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
  ggtitle('')+
  theme_void()+
  scale_y_reverse()+
  #scale_x_reverse()+
  scale_alpha_continuous(range = c(0.3, 1))+
  coord_fixed()+
  scale_fill_manual(values=c('white', '#0096FF'), name ='Reward')+
  #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
  theme(legend.position = 'none')
exampleRound

ggsave('plots/exampleRound.pdf', width = 4, height = 4, units = 'in')

####################################################################################################
# Load experiment data and preprocess
####################################################################################################
blockDF <- data.frame()
playerDF <- data.frame()
#read in data

dataFolders <- c('data/pilot2/','data/exp/','data/exp2/')   #Pilot data, first 2 sessions, sessions 3-7
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
  blockDF <- rbind(blockDF, read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
}
#add unique ids
blockDF$id <- paste0(blockDF$name, blockDF$session) 
playerDF$id <- paste0(playerDF$name, playerDF$session) 




########################################
#Plot a full round
#############################################
#target session and round
targetSession <- 'session7.json'
exampleDf <-  playerDF %>% dplyr::filter(type=="group" & session== targetSession & env=="smooth")
targetRound <- unique(exampleDf$round)[4]
exampleDf <- exampleDf %>% dplyr::filter(round==targetRound)
#Find out which env it corresponds to
session <- read_json(paste0('../sessions/', targetSession))
env <- read.csv(paste0('../environments/', session$roundData[[targetRound+2]]$envFile), sep = ';') #plus 2 is to account for training rounds

initial <- expand.grid(x = seq(2,59, length.out = 20), z = seq(2,59, length.out = 20))
exampleRound <- ggplot(exampleDf, aes( x=x,y=z)) +
  geom_point(data = initial, size=3, shape = 22, color = 'white', fill = '#027C00') +
  geom_point(data = subset(blockDF,type == 'group' & session == targetSession & round== targetRound), aes(fill = reward),size=3, shape = 22, color = 'white') +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
  geom_path(aes(alpha=time, color = id), size = 1)+
  geom_image( data=subset(exampleDf, time==120),  image="../images/minecraft.png", size=.05 , by="height")+
  scale_colour_manual(values = c('#D81B60','#560353','#FFC107', '#999999'))+
  #scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
  ggtitle('')+
  theme_void()+
  scale_alpha_continuous(range = c(0.3, 1))+
  coord_fixed()+
  scale_fill_manual(values=c('white', '#0096FF'), name ='Reward')+
  #theme(legend.position = 'none', panel.background = element_rect(fill = "black", colour = NA)) #black version
  theme(legend.position = 'none')
exampleRound

#ggsave('plots/exampleRound.pdf', width = 4, height = 4, units = 'in')



####################################################################################################
#Snapshot into individual trajectories
####################################################################################################


soloRandom <- playerDF %>% filter(type=="solo" & id=="MPIB1session13.json" & env=="random" & round ==3) %>%
  ggplot(aes(x=x,y=z)) +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
  geom_point(data = subset(blockDF, round == 3 & type == 'solo' & id == 'MPIB1session13.json' & env =='random'), aes(fill = reward),size=2.5, shape = 22, color = 'white') +
  geom_path(aes(colour=time), size = 1)+
  scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
  ggtitle('Solo Random')+
  scale_fill_manual(values=c('#999999', '#0072B2'), name ='Reward')+
  theme(legend.position='none')
soloRandom


soloSmooth <- playerDF %>% filter(type=="solo" & id=="MPIB1session13.json" & env=="smooth" & round == 11) %>%
  ggplot(aes(x=x,y=z)) +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
  geom_point(data = subset(blockDF,type == 'solo' & id == 'MPIB1session13.json' & env =='smooth' & round == 11), aes(fill = reward),size=2.5, shape = 22, color = 'white') +
  geom_path(aes(colour=time), size = 1)+
  scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
  ggtitle('Solo Smooth')+
  scale_fill_manual(values=c('#999999', '#0072B2'), name ='Reward')+
  theme(legend.position='none')
soloSmooth



groupRandom <- playerDF %>% filter(type=="group" & id=="MPIB1session13.json" & env=="random", round == 7) %>%
  ggplot(aes(x=x,y=z)) +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
  geom_point(data = subset(blockDF, round == 7 & type == 'group' & id == 'MPIB1session13.json' & env =='random'), aes(fill = reward),size=2.5, shape = 22, color = 'white') +
  geom_path(aes(colour=time), size = 1)+
  scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
  ggtitle('Group Random')+
  scale_fill_manual(values=c('#999999', '#0072B2'), name ='Reward')+
  theme(legend.position='none')
groupRandom


groupSmooth <- playerDF %>% filter(type=="group" & id=="MPIB1session13.json" & env=="smooth" & round == 15) %>%
  ggplot(aes(x=x,y=z)) +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60))+
  geom_point(data = subset(blockDF,type == 'group' & id == 'MPIB1session13.json' & env =='smooth' & round == 15), aes(fill = reward),size=2.5, shape = 22, color = 'white') +
  geom_path(aes(colour=time), size = 1)+
  scale_color_gradient2(low = "orange",high = "red", space = "Lab", name = 'Time' )+
  ggtitle('Group Smooth')+
  scale_fill_manual(values=c('#999999', '#0072B2'), name ='Reward')+
  theme(legend.position = c(0.05,1), legend.justification=c(0,1))
groupSmooth

pTraj <- cowplot::plot_grid(soloRandom,soloSmooth,  groupRandom,groupSmooth+theme(legend.position='none') , ncol = 2 )
pTraj


