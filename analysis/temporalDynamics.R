
rm(list=ls()) #house keeping


#load packages
packages <- c('tidyverse','zoo',  'DescTools','brms','arrow', 'reshape2')
invisible(lapply(packages, require, character.only = TRUE))

source('statisticalTests.R')
source('utilities.R')


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


####################################################################################################
# Individual level Analyses: run in individualDynamics.R
####################################################################################################
# #Compile individual dataframes from each time offset
# indDynamicsDF <- data.frame()
# dataFolder <- list.files('dynamicData/individual/')   
# for (file in dataFolder){
#   subdf <- readRDS(paste0('dynamicData/individual/', file))
#   indDynamicsDF <- rbind(indDynamicsDF, subdf)  
#   
# }
# 
# #add factors
# indDynamicsDF$id <- factor(indDynamicsDF$id)
# indDynamicsDF$session <- factor(indDynamicsDF$session)
# indDynamicsDF$env <- factor(indDynamicsDF$env)
# indDynamicsDF$type <- factor(indDynamicsDF$type)
#
# saveRDS(indDynamicsDF, 'dynamicData/individualDynamics.Rds')
# 
# # ####################################################################################################
# # # Perform permutation test to find threshold for random clusters
# # ####################################################################################################
# indDynamicsDF <- readRDS('dynamicData/individualDynamics.Rds')
# mainComparisons <- c('rewardVisOut', 'rewardVisIn', 'rewardProx', 'proxVisIn', 'proxVisOut', 'visInOut' )
# 
# for (gameType in c('solo', 'group')){
#   for (envType in c('random', 'smooth')){ #envs
#     for (comp in mainComparisons){ #comparisons
#       #DEBUG
#       #envType <- 'smooth'
#       #comp <- 'rewardVisOut'
#       timeSeries <- indDynamicsDF %>% filter(env == envType & type == gameType) %>% mutate(idround = paste0(id, round)) %>%  group_by(idround, offset) %>% select(idround, offset, all_of(comp)) %>%  acast(idround~offset, value.var=comp)
#       signifMat <- matrix(rep(NA,permNum*n_offset), nrow = permNum, ncol =n_offset) #set up a significance matrix
#       #Iterate through 10k permutations
#       for (i in 1:permNum){
#         permVec <- sample(c(-1,1), size=nrow(timeSeries), replace=TRUE)  #randomly sign flip different time series
#         permDat <-  sweep(timeSeries, MARGIN=1,  permVec, '*') #matrix multiplcation
#         signifMat[i,]  <- sapply(1:n_offset, FUN=function(t_ind){ifelse(t.test(permDat[,t_ind])$p.value<.05, 1, 0)})
#       }
#       #Find the distribution over the largest cluster sizes
#       #signifMat <- matrix(sample(c(0,1), size = permNum*n_offset, replace=TRUE), nrow = permNum, ncol =n_offset)#DEBUG create fake data for testing
#       clusterLengths <- sapply(1:permNum, FUN=function(i){
#         runs <- rle(signifMat[i,])
#         ifelse(1 %in% runs$values, max(runs$lengths[runs$values == 1]), 0)
#       })
#       clusterLimit <- t.test(clusterLengths)$conf.int[2] #set lower limit on cluster size based on the upper 95% CI of the cluster distribution from the randomly permuted data
#       permDF <- rbind(permDF, data.frame(env = envType, type = gameType, comparison = comp, clusterLim = clusterLimit))
#     }
#   }
# }
# 
# saveRDS(permDF, 'dynamicData/indPermutations.Rds')
# #
# # # ####################################################################################################
# # # # Look for significant clusters in the real data
# # ####################################################################################################
# 
# # Find clusters
# clusterDF <- data.frame()
# offsets <-sort(unique(indDynamicsDF$offset))
# mainComparisons <- c('rewardVisOut', 'rewardVisIn', 'rewardProx', 'proxVisIn', 'proxVisOut', 'visInOut' )
# 
# for (gameType in c('solo', 'group')){
#   for (envType in c('random', 'smooth')){ #envs
#     for (comp in mainComparisons){ #comparisons
#       #find significant time points
#       # envType <- 'smooth' #DEDUBG
#       # comp <- 'rewardVisOut'
#       signif <- sapply(1:length(offsets), FUN=function(t){
#         dat <- indDynamicsDF %>% filter(env ==envType & type == gameType & offset == offsets[t]) %>% pull(comp) #extract data as a vector
#         ifelse(t.test(dat)$p.value<.05, 1, 0)
#       })
#       clusterDF <- rbind(clusterDF,data.frame(comparison = comp, env = envType,type = gameType, signif = signif, offset = offsets))
#     }
#   }
# }
# saveRDS(clusterDF, 'dynamicData/individualClusters.Rds')
# #

####################################################################################################
# Plot time-lagged correlations (Individual level level) run in individualDynamics.R
####################################################################################################

indDynamicsDF <- readRDS('dynamicData/individualDynamics.Rds')
permDF <- readRDS('dynamicData/indPermutations.Rds')
clusterDF <- readRDS('dynamicData/individualClusters.Rds')



#Compute permutation corrected clusters
clusterDF$signifCorrected <- NA
mainComparisons <- c('rewardVisOut', 'rewardVisIn', 'rewardProx', 'proxVisIn', 'proxVisOut', 'visInOut' )

#Remove cluster signif that is shorter than the permutation length
for (gameType in c('solo', 'group')){
  for (envType in c('random', 'smooth')){ #envs
    for (comp in mainComparisons){ #comparisons
      #find significant time points
      # envType <- 'smooth' DEBUG
      # comp <- 'rewardVisOut'
      subCluster <-  clusterDF %>% filter(env ==envType & type == gameType & comparison == comp) %>% pull(signif)
      threshold <- permDF %>% filter(env ==envType & type == gameType & comparison==comp) %>% pull(clusterLim) %>% round( digits=0)
      #threshold <- 0 #DEBUG
      signifCorrected <- rep(FALSE, length(subCluster)) #initialize dummy variable
      #run length encoding
      clusters <- rle(subCluster)
      validClusters <- which(clusters$lengths >= threshold & clusters$values == 1)
      if (1 %in% validClusters){
        startPoints <- c(1, cumsum(clusters$lengths)[validClusters-1]+1)
      }else{
        startPoints <- cumsum(clusters$lengths)[validClusters-1]+1 #where each survivng cluster starts
      }
      endPoints <- startPoints + clusters$lengths[validClusters] -1
      if (length(validClusters)>0){
        for (vc in 1:length(validClusters)){
          start <- startPoints[vc]
          end <- endPoints[vc]
          #cat(subCluster[start:end]) #DEBUG
          signifCorrected[start:end] <- TRUE
        }
      }
      #add to dataframe
      clusterDF[clusterDF$env ==envType & clusterDF$type == gameType & clusterDF$comparison == comp,'signifCorrected' ] <- signifCorrected
    }
  }
}

# Plotting dataframe
plotDF <- indDynamicsDF%>% pivot_longer(mainComparisons, names_to = 'comparison')


plotDF <- plotDF %>% group_by(env,type, offset, comparison) %>%
  dplyr::summarize(zscore = mean(value, na.rm=TRUE), ssd = sd(value, na.rm=TRUE), count = n()) %>%
  mutate(se = ssd/sqrt(count),
         lower_ci = zscore - (qnorm(0.975)*se),
         upper_ci = zscore + (qnorm(0.975)*se))

#Add plottable cluster significance
plotDF$signif_alpha <- NA

#Remove cluster signif that is shorter than the permutation length
for (gameType in c('solo', 'group')){
  for (envType in c('random', 'smooth')){ #envs
    for (comp in mainComparisons){ #comparisons
      plotDF[plotDF$env == envType & plotDF$type == gameType & plotDF$comparison == comp, 'signif_alpha'] <- as.numeric( clusterDF[clusterDF$env ==envType & clusterDF$type == gameType & clusterDF$comparison == comp,'signifCorrected' ])
    }
  }
}
plotDF$comparison <- factor(plotDF$comparison, levels =  c('rewardProx', 'rewardVisOut', 'rewardVisIn', 'proxVisIn', 'proxVisOut', 'visInOut' ),
                            labels = c('Reward ~ Prox', 'Reward ~ OutVis', 'Reward ~ InVis', 'Prox ~ InVis', 'Prox ~ OutVis', 'InVis ~ OutVis'))




pInd <- ggplot(plotDF, aes(x = offset, y = zscore, color = env, alpha = signif_alpha))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  #geom_line(plotDF, mapping=aes(x = offset, y = signif_y, color = env), na.rm = TRUE, size = 1)+
  #facet_grid(~comparison)+
  facet_wrap(comparison~type,scales='free')+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('Temporal Correlation (z-score)')+
  xlab('Offset (s)')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  #scale_alpha_manual(values=c(0.2,0.8)) +
  scale_alpha(guide = 'none')+
  theme(legend.position=c(0,1),legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pInd


#ggsave('plots/fullIndDynamics.pdf', pInd, width = 14, height = 8, units= 'in' )

####################################################################################################
#  Group rounds only 
####################################################################################################

pIndMainRewardProx <- ggplot(subset(plotDF, comparison =='Reward ~ Prox' & type == 'group'), aes(x = offset, y = zscore, color = env, alpha = signif_alpha))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  #facet_grid(~comparison)+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('Temporal Correlation (z-score)')+
  xlab('Offset (s)')+
  ggtitle('Reward ~ Spatial Proximity (1/Distance)')+
  coord_cartesian(xlim = c(-20, 20), ylim = c(-.008, .008), expand = FALSE)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_alpha(guide = 'none')+
  theme(legend.position = c(0,1), legend.justification = c(0, 1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pIndMainRewardProx

sort(subset(plotDF, comparison =='Reward ~ Prox' & type == 'group' & signif_alpha == 1 & env == 'smooth')$offset)


pIndMainOut <- ggplot(subset(plotDF, comparison =='Reward ~ OutVis' & type == 'group'), aes(x = offset, y = zscore, color = env, alpha = signif_alpha))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  #facet_grid(~comparison)+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('Temporal Correlation (z-score)')+
  xlab('Offset (s)')+
  coord_cartesian(xlim = c(-5, 15),ylim=c(-.0075,.0035), expand = FALSE)+
  ggtitle('Reward ~ Visible Peers (Out-degree)')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_alpha(guide = 'none')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pIndMainOut

sort(subset(plotDF, comparison =='Reward ~ OutVis' & type == 'group' & signif_alpha == 1 & env == 'smooth')$offset)
plot(sort(subset(plotDF, comparison =='Reward ~ OutVis' & type == 'group' & signif_alpha == 1 & env == 'random')$offset))

pIndMainIn <- ggplot(subset(plotDF, comparison =='Reward ~ InVis' & type == 'group'), aes(x = offset, y = zscore, color = env, alpha = signif_alpha))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  #facet_grid(~comparison)+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('Temporal Correlation (z-score)')+
  xlab('Offset (s)')+
  coord_cartesian(xlim = c(-5, 15), ylim=c(-.014,.007),expand = FALSE)+
  ggtitle('Reward ~ Observers (In-degree)')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_alpha(guide = 'none')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pIndMainIn
plot(sort(subset(plotDF, comparison =='Reward ~ InVis' & type == 'group' & signif_alpha == 1 & env == 'smooth')$offset))


p <- cowplot::plot_grid( pIndMainRewardProx, pIndMainOut, pIndMainIn, nrow = 1, labels = c('g', 'h','i'))
p

ggsave('plots/dynamicsMain.pdf',p, width = 12, height = 3, units = 'in')


####################################################################################################
# Solo rounds
####################################################################################################

pIndMainRewardProxsolo <- ggplot(subset(plotDF, comparison =='Reward ~ Prox' & type == 'solo'), aes(x = offset, y = zscore, color = env))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  geom_line(subset(plotDF, comparison =='Reward ~ Prox'& type == 'solo'), mapping=aes(x = offset, y = signif_y, color = env), na.rm = TRUE, size = 1)+
  #facet_grid(~comparison)+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('z-score')+
  xlab('Offset (s)')+
  ggtitle('Reward ~ Spatial Proximity')+
  coord_cartesian(xlim = c(-20, 20), ylim = c(-.008, .008), expand = FALSE)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  #scale_alpha(guide = 'none')+
  theme(legend.position = c(0,1), legend.justification = c(0, 1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pIndMainRewardProxsolo


pIndMainOutsolo <- ggplot(subset(plotDF, comparison =='Reward ~ OutVis' & type == 'solo'), aes(x = offset, y = zscore, color = env))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  geom_line(subset(plotDF, comparison =='Reward ~ OutVis' & type == 'solo'), mapping=aes(x = offset, y = signif_y, color = env), na.rm = TRUE, size = 1)+
  #facet_grid(~comparison)+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('z-score')+
  xlab('Offset (s)')+
  coord_cartesian(xlim = c(-20, 20),ylim=c(-.0095,.007), expand = FALSE)+
  ggtitle('Reward ~ Visible Peers (Out-degree)')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  #scale_alpha(guide = 'none')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pIndMainOutsolo

pIndMainInsolo <- ggplot(subset(plotDF, comparison =='Reward ~ InVis' & type == 'solo'), aes(x = offset, y = zscore, color = env))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  geom_line(subset(plotDF, comparison =='Reward ~ InVis' & type == 'solo'), mapping=aes(x = offset, y = signif_y, color = env), na.rm = TRUE, size = 1)+
  #facet_grid(~comparison)+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('z-score')+
  xlab('Offset (s)')+
  coord_cartesian(xlim = c(-20, 20), ylim=c(-.0185,.007),expand = FALSE)+
  ggtitle('Reward ~ Observers (In-degree)')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  #scale_alpha(guide = 'none')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pIndMainInsolo



pSI <- cowplot::plot_grid(pIndMainRewardProxsolo, pIndMainOutsolo, pIndMainInsolo, nrow = 1, labels = 'auto')
pSI

ggsave('plots/dynamicsSolo.pdf',pSI, width = 12, height = 3, units = 'in')




# ###################################################################################################
# # Load behavioral data
# ####################################################################################################
# 
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
# #Convert playerDF into long format
# visDF <- playerDF %>% dplyr::filter(type == 'group') %>% pivot_longer(cols = MPIB4_visible:MPIB1_visible,  names_to = 'target', values_to = 'serverVis')
# visDF$target <- factor(visDF$target)
# levels(visDF$target) <- c("MPIB1","MPIB2", "MPIB3", "MPIB4")
# 
# #Visibility info
# pvisDF <- readRDS('simData/pvisDF.Rds')
# #evisDF <- readRDS('simData/evisDF.Rds')
# 
# #Distance computations
# distanceDF <- readRDS('trajectories/pairwiseDistances.Rds') #Computed in pullAnalysis.R
# 
# 
# ####################################################################################################
# # Compute time-lagged correlations (Group level)
# ####################################################################################################
# 
# timeSeq <- seq(0,120, by=.05)
# offsets <- seq(-20,20, by=.05)
# numPermutations <- 100
# 
# sessionList <- seq(1,32)
# dataFolder <- 'data/2021batch/'
# groupDynamicsDF <- data.frame()
# 
# for (s in sessionList){
#   #s <- 6
#   cat(s)
#   rounds <- unique(subset(visDF, session==paste0('session', s, '.json'))$round)
#   for (r in rounds){ #skip training rounds
#     #r <-8
#     #Load behavioral data
#     psub <- subset(playerDF, session ==paste0('session', s, '.json') & type == 'group' & round == r)
#     bsub <- subset(blockDF, session ==paste0('session', s, '.json') & type == 'group' & round == r)
#     #Player visibility
#     pvis <- read.csv(paste0(dataFolder, 'session', s,'_ge_',r+2, '_pvis.log.csv'), header = T, sep=';') #Replace visibility in playerDF with this visibility value
#     pvis <- pvis %>% pivot_longer(cols = P1:P4, names_to = 'target', values_to = 'unityVis')
#     pvis$round <- r
#     pvis$session <- paste0('session', s, '.json')
#     #Pairwise distances
#     dist <- subset(distanceDF, session ==  paste0('session', s, '.json') & round == r)
#     #global network properties
#     visinfoDF <- pvis %>% group_by(Time) %>% dplyr::summarize(connections = sum(unityVis>0)) #count number of connections
#     distDF <- dist %>% group_by(time) %>% dplyr::summarize(efficiency = mean(1/distance))
#     #Global behavior properties
#     behavDF <- data.frame(time = round(timeSeq, 2))
#     behavSum<- bsub %>% group_by(time) %>% dplyr::summarize(rewardRate = sum(reward==TRUE)/4, foragingRate = n()/4)
#     behavSum$time <- round(behavSum$time, digits = 2)
#     behavDF <- merge(behavDF, behavSum, by = 'time', all = TRUE)
#     behavDF[is.na(behavDF)] <- 0
# 
#     for (rewardOffset in offsets){
#       #rewardOffset <- -15
#       #initialize dataframe
#       corDF <- data.frame(round = r, session = paste0('session', s, '.json'), env = unique(bsub$env), offset = rewardOffset)
# 
#       #reward ~ vis
#       corDF$rewardVis <- FisherZ(cor.test(lag_func(behavDF$rewardRate, as.integer(rewardOffset/.05)),visinfoDF$connections)$estimate) - mean(sapply(1:numPermutations, FUN=function(i) {
#         FisherZ(cor.test(lag_func(behavDF$rewardRate, as.integer(rewardOffset/.05)),sample(visinfoDF$connections))$estimate)}), na.rm=TRUE)
# 
#       #Vis ~ spatial
#       corDF$visSpatial <- FisherZ(cor.test(lag_func(visinfoDF$connections, as.integer(rewardOffset/.05)),distDF$efficiency)$estimate) -  mean(sapply(1:numPermutations, FUN=function(i) {
#         FisherZ(cor.test(lag_func(visinfoDF$connections, as.integer(rewardOffset/.05)),sample(distDF$efficiency))$estimate)}), na.rm=TRUE)
# 
#       #reward ~ spatial
#       corDF$rewardSpatial <- FisherZ(cor.test(lag_func(behavDF$rewardRate, as.integer(rewardOffset/.05)),distDF$efficiency)$estimate) - mean(sapply(1:numPermutations, FUN=function(i) {
#         FisherZ(cor.test(lag_func(behavDF$rewardRate, as.integer(rewardOffset/.05)),sample(distDF$efficiency))$estimate)}), na.rm=TRUE)
# 
# 
#       #vis ~ foraging
#       corDF$visForaging <- FisherZ(cor.test(lag_func(visinfoDF$connections, as.integer(rewardOffset/.05)),behavDF$foragingRate)$estimate) - mean(sapply(1:numPermutations, FUN=function(i) {
#         FisherZ(cor.test(lag_func(visinfoDF$connections, as.integer(rewardOffset/.05)),sample(behavDF$foragingRate))$estimate)}), na.rm=TRUE)
# 
# 
#       #spatial ~ foraging
#       corDF$spatialForaging <- FisherZ(cor.test(lag_func(distDF$efficiency, as.integer(rewardOffset/.05)),behavDF$foragingRate)$estimate) - mean(sapply(1:numPermutations, FUN=function(i) {
#         FisherZ(cor.test(lag_func(distDF$efficiency, as.integer(rewardOffset/.05)),sample(behavDF$foragingRate))$estimate)}), na.rm=TRUE)
# 
#       #foraging ~ reward
#       corDF$foragingReward <- FisherZ(cor.test(lag_func(behavDF$rewardRate, as.integer(rewardOffset/.05)),behavDF$foragingRate)$estimate) - mean(sapply(1:numPermutations, FUN=function(i) {
#         FisherZ(cor.test(lag_func(behavDF$rewardRate, as.integer(rewardOffset/.05)),sample(behavDF$foragingRate))$estimate)}), na.rm=TRUE)
#       #put into data frame
#       groupDynamicsDF <- rbind(groupDynamicsDF, corDF)
# 
#     }
#   }
# }
# 
# saveRDS(groupDynamicsDF, 'dynamicData/groupDynamics.Rds')
# 
# ####################################################################################################
# # Perform permutation test to find threshold for random clusters
# ####################################################################################################
# 
# mainComparisons <- c('rewardVis', 'visSpatial', 'rewardSpatial')
# 
# #Cluster analysis
# permDF <- data.frame()
# permNum <-10000 #number of permutations
# n_offset <- length(unique(groupDynamicsDF$offset))
# 
# for (envType in c('random', 'smooth')){ #envs
#   for (comp in mainComparisons){ #comparisons
#     #DEBUG
#     #envType <- 'smooth'
#     #comp <- 'rewardVis'
#     timeSeries <- groupDynamicsDF %>% filter(env == envType) %>% mutate(id = paste0(session, round)) %>%  group_by(id, offset) %>% select(id, offset, all_of(comp)) %>%  acast(id~offset, value.var=comp)
#     signifMat <- matrix(rep(NA,permNum*n_offset), nrow = permNum, ncol =n_offset) #set up a significance matrix
#     #Iterate through 10k permutations
#     for (i in 1:permNum){
#       permVec <- sample(c(-1,1), size=nrow(timeSeries), replace=TRUE)  #randomly sign flip different time series
#       permDat <-  sweep(timeSeries, MARGIN=1,  permVec, '*') #matrix multiplcation
#       signifMat[i,]  <- sapply(1:n_offset, FUN=function(t_ind){ifelse(t.test(permDat[,t_ind])$p.value<.05, 1, 0)})
#     }
#     #Find the distribution over the largest cluster sizes
#     #signifMat <- matrix(sample(c(0,1), size = permNum*n_offset, replace=TRUE), nrow = permNum, ncol =n_offset)#DEBUG create fake data for testing
#     clusterLengths <- sapply(1:permNum, FUN=function(i){
#       runs <- rle(signifMat[i,])
#       ifelse(1 %in% runs$values, max(runs$lengths[runs$values == 1]), 0)
#     })
#     clusterLimit <- t.test(clusterLengths)$conf.int[2] #set lower limit on cluster size based on the upper 95% CI of the cluster distribution from the randomly permuted data
#     permDF <- rbind(permDF, data.frame(env = envType, comparison = comp, clusterLim = clusterLimit))
#   }
# 
# }
# 
# saveRDS(permDF, 'dynamicData/groupPermutations.Rds')

# ####################################################################################################
# # Look for significant clusters in the real data
# ####################################################################################################
# 
# # Find clusters
# clusterDF <- data.frame()
# offsets <-unique(groupDynamicsDF$offset)
# for (envType in c('random', 'smooth')){ #envs
#   for (comp in mainComparisons){ #comparisons
#     #find significant time points
#     # envType <- 'smooth' DEDUBG
#     # comp <- 'rewardVis'
#     signif <- sapply(1:length(offsets), FUN=function(t){
#       dat <- groupDynamicsDF %>% filter(env ==envType & offset == offsets[t]) %>% pull(comp) #extract data as a vector
#       ifelse(t.test(dat)$p.value<.05, 1, 0)
#     })
#     clusterDF <- rbind(clusterDF,data.frame(comparison = comp, env = envType, signif = signif, offset = offsets))
#   }
# }
# saveRDS(clusterDF, 'dynamicData/groupClusters.Rds')
#
# ####################################################################################################
# Plot time-lagged correlations (Group level)
####################################################################################################
#load previously computed data
# groupDynamicsDF <- readRDS('dynamicData/groupDynamics.Rds')
# permDF <- readRDS('dynamicData/groupPermutations.Rds')
# clusterDF <- readRDS('dynamicData/groupClusters.Rds')
# 
# mainComparisons <- c('rewardVis', 'visSpatial', 'rewardSpatial')
# 
# test <- clusterDF %>% filter(env =='smooth' & comparison == 'rewardVis') %>% pull(signif)
# 
# #Compute permutation corrected clusters
# clusterDF$signifCorrected <- NA
# #Remove cluster signif that is shorter than the permutation length
# for (envType in c('random', 'smooth')){ #envs
#   for (comp in mainComparisons){ #comparisons
#     #find significant time points
#     # envType <- 'smooth' DEBUG
#     # comp <- 'rewardVis'
#     subCluster <-  clusterDF %>% filter(env ==envType & comparison == comp) %>% pull(signif)
#     threshold <- permDF %>% filter(env ==envType & comparison==comp) %>% pull(clusterLim) %>% round( digits=0)
#     signifCorrected <- rep(FALSE, length(subCluster)) #initialize dummy variable
#     #run length encoding
#     clusters <- rle(subCluster)
#     validClusters <- which(clusters$lengths >= threshold & clusters$values == 1)
#     if (1 %in% validClusters){
#       startPoints <- c(1, cumsum(clusters$lengths)[validClusters-1]+1)
#     }else{
#       startPoints <- cumsum(clusters$lengths)[validClusters-1]+1 #where each survivng cluster starts
#     }
#     endPoints <- startPoints + clusters$lengths[validClusters] -1
#     if (length(validClusters)>0){
#       for (vc in 1:length(validClusters)){
#         start <- startPoints[vc]
#         end <- endPoints[vc]
#         #cat(subCluster[start:end]) #DEBUG
#         signifCorrected[start:end] <- TRUE
#       }
#     }
#     #add to dataframe
#     clusterDF[clusterDF$env ==envType & clusterDF$comparison == comp,'signifCorrected' ] <- signifCorrected
#   }
# }
# 
# 
# # prepare plotting DF
# plotDF <- groupDynamicsDF%>% pivot_longer(c(rewardVis, visSpatial, rewardSpatial, visForaging, spatialForaging, foragingReward), names_to = 'comparison')
# 
# plotDF <- plotDF %>% group_by(env, offset, comparison) %>%
#   dplyr::summarize(zscore = mean(value, na.rm=TRUE), ssd = sd(value, na.rm=TRUE), count = n()) %>%
#   mutate(se = ssd/sqrt(count),
#          lower_95 = zscore - (qnorm(0.975)*se),
#          upper_95 = zscore + (qnorm(0.975)*se),
#          lower_se = zscore - se,
#          upper_se = zscore + se)
# 
# #Add plottable cluster significance y-values
# plotDF$signif_y <- NA
# #Remove cluster signif that is shorter than the permutation length
# for (envType in c('random', 'smooth')){ #envs
#   for (comp in mainComparisons){ #comparisons
#     plotDF[plotDF$env == envType & plotDF$comparison == comp, 'signif_y'] <- as.numeric( clusterDF[clusterDF$env ==envType & clusterDF$comparison == comp,'signifCorrected' ])
#   }
# }
# 
# 
# plotDF$comparison <- factor(plotDF$comparison, levels = c( 'rewardSpatial', 'rewardVis', 'visSpatial', 'visForaging',  'spatialForaging','foragingReward' ),
#                             labels = c('Reward ~ Proximity', 'Reward ~ Visibility', 'Visibility ~ Proximity',  'Vis ~ Foraging',  'Spatial ~ Foraging', 'Reward ~ Foraging'))
# 
# #adjust y-values of cluster lines
# plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset <0, 'signif_y'] <- plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset <0, 'signif_y'] * -.012
# plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset <0, 'signif_y'] <- plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset <0, 'signif_y'] * -.01
# plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset >=0, 'signif_y'] <- plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset >=0, 'signif_y'] * .01
# plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset >=0, 'signif_y'] <- plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Proximity' & plotDF$offset >=0, 'signif_y'] * .008
# 
# plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset <0, 'signif_y'] <- plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset <0, 'signif_y'] * -.010
# plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset <0, 'signif_y'] <- plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset <0, 'signif_y'] * -.008
# plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset >=0, 'signif_y'] <- plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset >=0, 'signif_y'] * -.012
# plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset >=0, 'signif_y'] <- plotDF[plotDF$env == 'random' & plotDF$comparison == 'Reward ~ Visibility' & plotDF$offset >=0, 'signif_y'] * -.01
# 
# 
# plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset <0, 'signif_y'] <- plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset <0, 'signif_y'] * .25
# plotDF[plotDF$env == 'random' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset <0, 'signif_y'] <- plotDF[plotDF$env == 'random' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset <0, 'signif_y'] * .2
# plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset >=0, 'signif_y'] <- plotDF[plotDF$env == 'smooth' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset >=0, 'signif_y'] * -.2
# plotDF[plotDF$env == 'random' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset >=0, 'signif_y'] <- plotDF[plotDF$env == 'random' & plotDF$comparison == 'Visibility ~ Proximity' & plotDF$offset >=0, 'signif_y'] * -.1
# 
# plotDF["signif_y"][plotDF["signif_y"] == 0] <- NA
# 
# pGroupDynamicsSubset <- ggplot(subset(plotDF,comparison %in%c('Reward ~ Proximity', 'Reward ~ Visibility', 'Visibility ~ Proximity')), aes(x = offset, y = zscore, color = env))+
#   geom_hline(yintercept = 0,  color = 'black')+
#   geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
#   geom_ribbon(aes(ymin = lower_se, ymax = upper_se, fill = env), alpha = 0.3, color = NA)+
#   geom_line()+ 
#   geom_line(subset(plotDF,comparison %in%c('Reward ~ Proximity', 'Reward ~ Visibility', 'Visibility ~ Proximity')), mapping=aes(x = offset, y = signif_y, color = env), na.rm = TRUE)+
#   #facet_grid(~comparison)+
#   facet_wrap(comparison~., ncol = 1, scales='free_y')+
#   #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
#   theme_classic()+
#   ylab('z-score')+
#   xlab('Offset (s)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   #scale_alpha(guide = 'none')+
#   theme(legend.position=c(0,1),legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# 
# pGroupDynamicsSubset
# 
# 
# 
# 
# pGroupDynamicsMain <- ggplot(subset(plotDF,comparison %in%c('Reward ~ Proximity')), aes(x = offset, y = zscore, color = env))+
#   geom_hline(yintercept = 0,  color = 'black')+
#   geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
#   geom_ribbon(aes(ymin = lower_se, ymax = upper_se, fill = env), alpha = 0.3, color = NA)+
#   geom_line()+ 
#   geom_line(subset(plotDF,comparison %in%c('Reward ~ Proximity')), mapping=aes(x = offset, y = signif_y, color = env), na.rm = TRUE)+
#   #facet_grid(~comparison)+
#   #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
#   theme_classic()+
#   ylab('z-score')+
#   xlab('Offset (s)')+
#   ggtitle('Reward ~ Proximity (Group level)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   #scale_alpha(guide = 'none')+
#   theme(legend.position=c(0,1),legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# 
# pGroupDynamicsMain


# pGroupDynamicsForaging<- ggplot(subset(plotDF,!(comparison %in%c('Vis ~ Spatial', 'Vis ~ Reward', 'Spatial ~ Reward')) & offset^2 > .05), aes(x = offset, y = zscore, color = env))+
#   geom_hline(yintercept = 0,  color = 'black')+
#   geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
#   geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
#   geom_line()+
#   #facet_grid(~comparison)+
#   facet_wrap(comparison~., ncol = 1, scales='free_y', )+
#   #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
#   theme_classic()+
#   ylab('z-score')+
#   xlab('Offset (s)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   #scale_alpha(guide = 'none')+
#   theme(legend.position=c(1,1),legend.justification=c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# 
# pGroupDynamicsForaging



