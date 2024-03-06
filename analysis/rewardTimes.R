#Time betwen rewards (for computational models)
rm(list=ls()) #house keeping
#setwd('analysis')

#load packages
packages <- c('ggplot2', 'trajr', 'tidyverse', 'scales',  'cowplot','jsonlite', 'DescTools', 'ggbeeswarm','brms', 'sjPlot','ggthemes','ggsignif', 'RColorBrewer',  'GGally', 'network', 'sna', 'igraph')
invisible(lapply(packages, require, character.only = TRUE))

source('utilities.R')

# 
# ####################################################################################################
# # Load data and preprocess
# ####################################################################################################
# blockDF <- data.frame()
# playerDF <- data.frame()
# #read in data
# 
# #2020 data
# # dataFolders <- c('data/pilot2/','data/exp/','data/exp2/')   #Pilot data, first 2 sessions, sessions 3-7
# # for (dataFolder in dataFolders){
# #   playerDF <- rbind(playerDF, read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
# #   blockDF <- rbind(blockDF, read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
# # }
# 
# #2021 data
# dataFolders <- c('data/2021batch/')   #Pilot data, first 2 sessions, sessions 3-7
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
# 
# 
# evisDF <- readRDS('simData/evisDF.Rds') #computed in visibilityAnalysis.R
# evisDF$id <- paste0(evisDF$PlayerName, evisDF$session)
# evisDF <- subset(evisDF, PlayerName != TriggerPlayer & eventtype == 'SPL') #restrict to only social observations of reward
# 
# #Compute elapsed time for individual and social rewards
# blockDF$lastIndReward <- NA
# blockDF$lastSocReward <- NA
# 
# for (pid in unique(blockDF$id)){
#   for (r in unique(blockDF$round)){
#     #testing
#     #pid <- 'MPIB2session5.json'
#     #r <- 11
#     subdf <- subset(blockDF, id==pid & round==r)
#     #subdf$reward
#     #compute individual reward times
#     indTimes <- sapply(1:nrow(subdf), FUN=function(k){
#       if (k>1){ #after the first block
#         if (sum(subdf[1:(k-1), 'reward'])<1){ #if no rewards
#           return(0.0) #TODO: check what to default this to
#         }else{
#           subsubdf <- subdf[1:(k-1),]
#           max(subsubdf[subsubdf$reward==TRUE,'time'])
#         }
#       }else{#first block
#         return(0.0) #TODO: check what to default this to
#       }
#     } )
#     #compute social reward times
#     subvis <- subset(evisDF, id==pid & round==r)
#     socTimes <- sapply(1:nrow(subdf), FUN=function(k){
#       if (k>1){ #after the first block
#         return(max(c(subvis[subvis$Time<= subdf$time[k-1],'Time'], 0)))
#       }else{#first block
#         return(0.0) #TODO: check what to default this to
#       }
#     } )
#     #Store data
#     blockDF[blockDF$id == pid & blockDF$round == r, 'lastIndReward'] <- subdf$time - indTimes
#     blockDF[blockDF$id == pid & blockDF$round == r, 'lastSocReward'] <- subdf$time - socTimes
#   }
# }
# 

#saveRDS(blockDF, '../cogmodeling/data/rewardTimes.RDS')
blockDF <- readRDS('../cogmodeling/data/rewardTimes.RDS')
#Look at data


pInd <- ggplot(blockDF, aes(x = lastIndReward, fill = env, color = env))+
  geom_histogram(aes(y = after_stat(count / sum(count))), alpha = 0.7, bins = 30)+
  scale_color_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  scale_fill_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  theme_classic()+
  facet_grid(env~type)+
  scale_y_continuous(labels = scales::percent)+
  xlab('Time (s) Since Last Individual Reward')+
  ylab('Density')+
  theme(legend.position='none', strip.background = element_blank())



pSoc <- ggplot(blockDF, aes(x = lastSocReward, fill = env, color = env))+
  geom_histogram(aes(y = after_stat(count / sum(count))), alpha = 0.7, bins = 30)+
  scale_color_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  scale_fill_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  theme_classic()+
  facet_grid(env~type)+
  scale_y_continuous(labels = scales::percent)+
  xlab('Time (s) Since Last Social Reward')+
  ylab('Density')+
  theme(legend.position='none', strip.background = element_blank())

p <- cowplot::plot_grid(pInd,pSoc, labels = 'auto')
ggsave('plots/rewardTimes.pdf', p, width = 8, height = 3, units = 'in')

# 
# pInd <- ggplot(blockDF, aes(x = time, y = lastIndReward, color = env, fill =env, linetype = type))+
#   #stat_summary(fun.data = mean, geom='line')+
#   #stat_summary(fun.data = mean_cl_boot, geom= 'ribbon', color = NA, alpha = 0.2)+ #Almost no variance
#   geom_smooth(alpha = 0.2)+
#   theme_classic()+
#   scale_color_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
#   scale_fill_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
#   scale_linetype(name='Condition')+
#   scale_x_continuous(breaks=seq(0,120,length.out=5))+
#   guides(linetype = guide_legend(override.aes= list( fill = NA, color = 'black')))+
#   xlab('Time (s)')+ ylab('Time Since Ind Reward')+
#   theme(legend.position=c(0.9,0.05), legend.justification=c(1,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pInd
# 
# 
# pSoc <- ggplot(subset(blockDF, type=='group'), aes(x = time, y = lastSocReward, color = env, fill =env, linetype = type))+
#   #stat_summary(fun.data = mean, geom='line')+
#   #stat_summary(fun.data = mean_cl_boot, geom= 'ribbon', color = NA, alpha = 0.2)+ #Almost no variance
#   geom_smooth(alpha = 0.2)+
#   theme_classic()+
#   scale_color_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
#   scale_fill_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
#   scale_linetype(name='Condition')+
#   scale_x_continuous(breaks=seq(0,120,length.out=5))+
#   guides(linetype = guide_legend(override.aes= list( fill = NA)))+
#   xlab('Time (s)')+ ylab('Time Since Soc Reward')+
#   theme(legend.position='none')
# pSoc

