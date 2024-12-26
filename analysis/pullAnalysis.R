#analysis of Pull events
# Charley Wu
rm(list=ls()) #house keeping


#load packages
packages <- c('ggplot2', 'trajr', 'sjPlot', 'dplyr', 'gganimate','pdist','TTR', 'signal', 'viridis','cowplot','jsonlite', 'ggimage','ggbeeswarm', 'ggthemes','ggsignif', 'feather', 'gifski', 'RColorBrewer',  'animation', 'plyr')
lapply(packages, require, character.only = TRUE)

source('statisticalTests.R')
source('utilities.R')
source('extract_pulls.Strandburg-Peshkin.r') # Code from Strandburg-Peshkin et al., (Nature 2015)

#Parameters
window.width <- 60*20 #1 minute window * 20hz sampling rate
noise.threshold <- 0.5 #Threshold on peak/valley finder

theme_set(theme_classic()) #set theme
leaderPal <-c('#D81B60','#560353','#FFC107', '#999999')
virPal <- viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,option = "D")

####################################################################################################
#Load data and preprocess
####################################################################################################
blockDF <- data.frame()
playerDF <- data.frame()
#read in data
# 
# #2019 data
# dataFolders <- c('data/pilot2/','data/exp/','data/exp2/')   #Pilot data, first 2 sessions, sessions 3-7
# for (dataFolder in dataFolders){
#   playerDF <- rbind(playerDF, read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
#   blockDF <- rbind(blockDF, read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
# }


#2021 data
dataFolders <- c('data/2021batch/')   
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
  blockDF <- rbind(blockDF, arrow::read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
}

#add unique ids
blockDF$id <- paste0(blockDF$name, blockDF$session) 
playerDF$id <- paste0(playerDF$name, playerDF$session) 

####################################################################################################
#Compute Trajectories
####################################################################################################
#VERY SLOW
# trajDF <- data.frame()
# for (s in unique(playerDF$session)){ #slow
#   for (r in unique(playerDF$round)){
#     for (pid in c("MPIB1", "MPIB2", "MPIB3", "MPIB4")){
#       singleDat <- subset(playerDF, session == s  & round==r & name==pid)
#       coords <- data.frame(x = singleDat$x, y = singleDat$z, times = singleDat$time)
#       trj <- TrajFromCoords(coords, spatialUnits = 'blocks', timeCol = 'times') #polar and displacement are complex numbers, where Mod(x) gives you the length of the vector and Arg(x) is the angle
#       trj <- cbind(trj, sessions = s, round=r, type = singleDat$type, env = singleDat$env, name = pid, meanStraightness = Mod(TrajMeanVectorOfTurningAngles(trj)) )
#       trajDF <- rbind(trajDF, trj)
#     }
#   }
# }
# saveRDS(trajDF, 'trajectories/trajectories.Rds')
trajDF <- readRDS('trajectories/trajectories.Rds')

#############################################################################################
#Compute pairwise distances
#############################################################################################
#Compute pairwise distances between all individuals in a game
# distanceDF <- data.frame()
# ts <- unique(playerDF$time)
# for (s in unique(playerDF$session)){ #slow
#   rounds <- unique(subset(playerDF, session == s)$round)
#   for (r in rounds){
#     for (p1 in c("MPIB1", "MPIB2", "MPIB3", "MPIB4")){
#       for (p2 in c("MPIB1", "MPIB2", "MPIB3", "MPIB4")){
#         if (!p1==p2){
#           p1dat <- as.matrix(subset(trajDF, sessions == s  & round==r & name==p1)[,c('x','y')])
#           p2dat <- as.matrix(subset(trajDF, sessions == s  & round==r & name==p2)[,c('x','y')])
#           dists <- sqrt(rowSums((p1dat - p2dat)^2)) #pairwise distances
#           dummy <- data.frame(distance = dists, p1 = p1, p2 =p2, time = subset(trajDF, sessions == s  & round==r & name==p1)$time, session = s, round = r, env = subset(trajDF, sessions == s  & round==r & name==p1)$env[1], type = subset(trajDF, sessions == s  & round==r & name==p1)$type[1] )
#           distanceDF <- rbind(distanceDF, dummy)
#         }
#       }
#     }
#   }
# }
# distanceDF$p1 <- factor(distanceDF$p1, levels = c("MPIB1", "MPIB2", "MPIB3", "MPIB4"))
# distanceDF$p2 <- factor(distanceDF$p2, levels = c("MPIB1", "MPIB2", "MPIB3", "MPIB4"))
# saveRDS(distanceDF, 'trajectories/pairwiseDistances.Rds')
distanceDF <- readRDS('trajectories/pairwiseDistances.Rds') #Cached

####################################################################################################
#Get dyad interactions. Code from Strandburg-Peshkin et al., (Nature 2015)
####################################################################################################
#A priori filters
noise.thresh <- 1 #how much change in dyadic distance is needed in order to consider that one has left the local maximum or minimum. In general, this controls the amount of "jitter" allowed in the dyadic distance before it is counted as an even
min.seq.length <- 60 #Minum length of sequence, where each unit is .05s
# # 
# #Compute events distances between all individuals in a game
# playerNames <- c("MPIB1", "MPIB2", "MPIB3", "MPIB4")
# eventDF <- data.frame()
# for (s in unique(playerDF$session)){ #slow
#   rounds <- unique(subset(playerDF, session == s)$round)
#   for (r in rounds){
#     xs <- matrix(NA, nrow = 4, ncol = 2401)
#     ys <- matrix(NA, nrow = 4, ncol = 2401)
#     for (p in 1:4){
#       xs[p,] <- playerDF %>% dplyr::filter(session==s, round==r, name == playerNames[p]) %>% pull(x)
#       ys[p,] <- playerDF %>% dplyr::filter(session==s, round==r, name == playerNames[p]) %>% pull(z)
#     }
#     #compute events
#     events <- get.all.interactions(xs,ys,c(1,2041),noise.thresh,min.seq.length)
#     #Add round details
#     events$session <- s
#     events$round <- r
#     events$roundtype <- unique(subset(playerDF, session == s  & round == r)$type)
#     events$env <- unique(subset(playerDF, session == s  & round == r)$env)
#     #Join together
#     eventDF <- rbind(eventDF, events)
#   }
# }
# #Convert time to seconds
# eventDF$t1 <- eventDF$t1 * .05
# eventDF$t2 <- eventDF$t2 * .05
# eventDF$t3 <- eventDF$t3 * .05
# saveRDS(eventDF, 'events/pullAnchor.Rds') #unthresholded


eventDF <- readRDS('events/pullAnchor.Rds')

#A posteriori filters
threshold <- 0.1 #0.05, 0.1, 0.15
min.disparity <-threshold #this parameter controls how much one individual has to move more than another in order to differentiate it as the puller (or pullee); it ranges from 0 to 1, where 0 lets in all events, and 1 requires that one individual do all the moving in each time segment of the event
min.strength <- threshold#this parameter controls how big the change in dyadic distance has to be (as compared to the total dyadic distance) in order to count an event; it ranges from 0 to 1, where 0 lets in all events, and 1 requires that the change in dyadic distance be equal to the sum of the dyadic distances for each time segment during the event
hdi(subset(eventDF, type == 'pull')$disparity) #
hdi(subset(eventDF, type == 'pull')$strength)
sd(subset(eventDF, type == 'pull')$t3-subset(eventDF, type == 'pull')$t1)/sqrt(nrow(subset(eventDF, type == 'pull')))


#duration
hist(eventDF$t3-eventDF$t1)
summary(eventDF$t3-eventDF$t1)
pEventDuration <- ggplot(subset(eventDF, type == 'pull'), aes(x = t3-t1, fill = env))+
  geom_histogram(aes(y = (..count..)/(4*length(unique(eventDF$session)))),alpha = 0.7, binwidth = 5, color = 'black')+
  geom_vline(xintercept = min.seq.length*.05, linetype = 'dashed', color = 'black')+
  facet_grid(roundtype~env)+
  ylab('Events per Round')+
  xlab(' Event Duration (s)')+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  theme_classic()+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEventDuration

#disparity
hist(eventDF$disparity)
summary(eventDF$disparity)
pEventDisparity <- ggplot(subset(eventDF, type == 'pull'), aes(x = disparity, fill = env))+
  geom_histogram(aes(y = (..count..)/(4*length(unique(eventDF$session)))),alpha = 0.7, color = 'black')+
  geom_vline(xintercept = min.disparity, linetype = 'dashed', color = 'black')+
  facet_grid(roundtype~env)+
  ylab('Events per Round')+
  xlab('Disparity')+
  coord_cartesian(xlim=c(0,1))+
  theme_classic()+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEventDisparity

#Strength
hist(eventDF$strength)
summary(eventDF$strength)
pEventStrength <- ggplot(subset(eventDF, type == 'pull'), aes(x = strength, fill = env))+
  geom_histogram(aes(y = (..count..)/(4*length(unique(eventDF$session)))),alpha = 0.7,  color = 'black')+
  geom_vline(xintercept = min.strength, linetype = 'dashed', color = 'black')+
  facet_grid(roundtype~env)+
  ylab('Events per Round')+
  xlab('Strength')+
  coord_cartesian(xlim=c(0,1))+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  theme_classic()+
  theme(legend.position='none',strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEventStrength



# pDisStrength <- cowplot::plot_grid(pEventDisparity, pEventStrength,  ncol = 2 )
# pDisStrength
#ggsave('plots/disparityStrength.pdf', pDisStrength, width = 8, height = 3, units = 'in')

#---Threshold events to remove events which do not exceed the thresholds---
eventDF <- eventDF[which(eventDF$disparity >= min.disparity & eventDF$strength >= min.strength),]
nrow(eventDF)
saveRDS(eventDF, paste0('events/pullsFiltered.', threshold, '.Rds'))
####################################################################################################
#Comparison of different thresholds
####################################################################################################
plotlist <- list()

for (threshold in c(0.05, 0.1, 0.15)){
  eventDF <- readRDS(paste0('events/pullsFiltered.', threshold, '.Rds'))
  cat(nrow(eventDF))
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
  #each individual session
  indEventDF <-  allCombs%>% group_by(session, env, roundtype, type) %>% dplyr::summarize(meanEvents = mean(events))
  #Pull events only
  pPulls<- ggplot(subset(meanEventDF, type=='pull'), aes(x = roundtype,y = meanEvents, fill = env))+
    geom_bar(stat = 'identity', color = 'black',position = "dodge2")+
    geom_errorbar(aes(ymin = meanEvents - (qnorm(0.975)*seEvents), ymax = meanEvents + (qnorm(0.975)*seEvents)), width = 0.3,  position = position_dodge(width = 0.9))+
    #facet_grid(~env)+
    geom_quasirandom(data = subset(indEventDF, type == 'pull'), dodge.width = 0.9, alpha = 0.3, size= 0.5)+
    ylab('Pulls per Round')+
    xlab('')+
    ggtitle(paste0('Threshold = ', threshold, '; n = ',nrow(eventDF) ))+
    scale_fill_manual(values= c("#E69F00","#009E73"),'Environment')+
    theme_classic()+
    theme(legend.position=c(0,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
  if (length(plotlist)>0){
    pPulls<- pPulls + theme(legend.position = 'None')
  }
  plotlist[[length(plotlist)+1]] <- pPulls
}

pComparison <- plot_grid(plotlist[[1]], plotlist[[2]], plotlist[[3]], labels = 'auto', nrow = 1)
pComparison
#ggsave('plots/pullThresholdComparison.pdf', pComparison, width = 10, height = 4, unit='in')

####################################################################################################
#Main analyses
####################################################################################################

threshold <- 0.1
eventDF <- readRDS(paste0('events/pullsFiltered.', threshold, '.Rds'))
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

pEventsSimple <- ggplot(subset(meanEventDF, roundtype == 'group' & type=='pull') , aes(x = env,y = meanEvents, fill = env))+
  #geom_bar(aes(y = (..count..)/(4*length(unique(eventDF$session)))), color = 'black',position = "dodge2" )+
  geom_bar(stat = 'identity', color = 'black')+
  geom_errorbar(aes(ymin = meanEvents - seEvents, ymax = meanEvents + seEvents), width = 0.3)+
  #facet_grid(~env)+
  ylab('Pulls per Round ± SE')+
  xlab('')+
  scale_fill_manual(values= c("#E69F00","#009E73"),'')+
  theme_classic()+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEventsSimple

#One dot per session
sessionEventsDF <- allCombs%>% group_by(session, env, roundtype, type) %>% dplyr::summarize(events = sum(events)/4)
sessionEventsDF$env <- factor(sessionEventsDF$env, levels = c('random', 'smooth'))
ttestPretty(subset(sessionEventsDF, roundtype == 'group' & type=='pull' & env == 'smooth')$events,subset(sessionEventsDF, roundtype == 'group' & type=='pull' & env == 'random')$events, paired=T )
pEventsSessions<- ggplot(subset(sessionEventsDF, roundtype == 'group' & type=='pull') , aes(x = env,y = events, color = env))+
  geom_boxplot(color = 'black', width = 0.2, outlier.shape = NA)+
  geom_line(aes(group=session), color = 'black', alpha = 0.2)+
  geom_quasirandom(width=0.1)+
  stat_summary(fun=mean, geom='point', color = 'black', shape = 5, size = 2)+
  #geom_errorbar(aes(ymin = meanEvents - seEvents, ymax = meanEvents + seEvents), width = 0.3)+
  #facet_grid(~env)+
  ylab('Pulls per Round')+
  xlab('')+
  scale_color_manual(values= c("#E69F00","#009E73"),'')+
  theme_classic()+
  geom_signif(comparison=list(c('random', 'smooth')), annotation = 'p < .001', color = 'black')+
  #coord_cartesian(ylim=c(0, 3.3))+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEventsSessions


#One split by rounds
sessionEventsDF <- allCombs%>% group_by(session, env, roundtype, type, round) %>% dplyr::summarize(events = sum(events))
pEventsSessionsRounds<- ggplot(subset(sessionEventsDF, roundtype == 'group' & type=='pull') , aes(x = round,y = events, color = env, fill = env))+
  #geom_boxplot(color = 'black', width = 0.2, outlier.shape = NA)+
  #geom_line(aes(group=session), color = 'black', alpha = 0.2)+
  #geom_quasirandom(width=0.1)+
  stat_summary(fun=mean, geom='line')+
  stat_summary(fun.data=mean_se, geom='ribbon', alpha = 0.2, color = NA)+
  #geom_errorbar(aes(ymin = meanEvents - seEvents, ymax = meanEvents + seEvents), width = 0.3)+
  #facet_grid(~env)+
  ylab('Pulls per Round')+
  xlab('')+
  scale_color_manual(values= c("#E69F00","#009E73"),'')+
  scale_fill_manual(values= c("#E69F00","#009E73"),'')+
  theme_classic()+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEventsSessionsRounds


#All event types and conditions
#meanEventDF$env <- factor(meanEventDF, levels = c('random', 'smooth'))
pEvents <- ggplot(meanEventDF, aes(x = roundtype,y = meanEvents, fill = env))+
  geom_bar(stat = 'identity', color = 'black',position = "dodge2")+
  geom_errorbar(aes(ymin = meanEvents - seEvents, ymax = meanEvents + seEvents), width = 0.3,  position = position_dodge(width = 0.9))+
  facet_grid(~type)+
  ylab('Events per Round ± SE')+
  xlab('Event Type')+
  scale_fill_manual(values= c("#E69F00","#009E73"),'Environment')+
  theme_classic()+
  theme(legend.position=c(0,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pEvents

#Pull events only
indMeans <- sessionEventsDF %>% subset(type=='pull') %>% group_by(session, env, roundtype) %>% dplyr::summarize(meanEvents = mean(events))
pPulls<- ggplot(subset(meanEventDF, type=='pull'), aes(x = roundtype,y = meanEvents, fill = env))+
  geom_bar(stat = 'identity', color = 'black',position = "dodge2")+
  geom_errorbar(aes(ymin = meanEvents - (qnorm(0.975)*seEvents), ymax = meanEvents + (qnorm(0.975)*seEvents)), width = 0.3,  position = position_dodge(width = 0.9))+
  geom_quasirandom(data = indMeans, dodge.width = 0.9, alpha = 0.3, size= 0.5)+
  #facet_grid(~env)+
  ylab('Pulls per Round  ')+
  xlab('')+
  scale_fill_manual(values= c("#E69F00","#009E73"),'Environment')+
  scale_color_manual(values= c("#E69F00","#009E73"),'Environment')+
  theme_classic()+
  theme(legend.position=c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pPulls
#ggsave('plots/pulls.Threshold0.05.pdf', pPulls +ggtitle(paste0('Threshold = ', threshold)), height = 3, width = 3, units = 'in')

#T-Tests
pullEventsDF <- allCombs %>% dplyr::filter(roundtype == 'group' & type=='pull') %>% group_by(session, round, env) %>% dplyr::summarize(pullEvents = sum(events)) #one data point per round
ttestPretty(subset(pullEventsDF, env == 'smooth')$pullEvents, subset(pullEventsDF, env == 'random')$pullEvents, paired=T)
pullEventsDF <- allCombs %>% dplyr::filter(roundtype == 'group' & type=='pull') %>% group_by(session, env) %>% dplyr::summarize(pullEvents = sum(events)) #one data point per session
ttestPretty(subset(pullEventsDF, env == 'smooth')$pullEvents, subset(pullEventsDF, env == 'random')$pullEvents, paired=T)



#Regression
#Only group rounds
pullEventsDF <- allCombs %>% dplyr::filter(roundtype == 'group' & type=='pull') %>% group_by(session, round, env) %>% dplyr::summarize(pullEvents = sum(events))
pullEventsDF$env <- factor(pullEventsDF$env, levels = c('random','smooth'))
mPullGroupOnly <-  run_model(brm(pullEvents~env + (1+env|session), data =pullEventsDF,
                        family = poisson,
                        cores=4,  iter = 4000, warmup = 1000), modelName = 'pullsGroupOnly')

summary(mPullGroupOnly)
#tab_model(mPullGroupOnly)
#plot_model(mPullGroupOnly,  sort.est=TRUE)
post <- posterior_samples(mPullGroupOnly)
sum(post['b_envsmooth']>0)/length(unlist(post['b_envsmooth'])) #Probability of coeffiient > 0


#Regression including solo rounds
pullEventsDF <- allCombs %>% dplyr::filter( type=='pull') %>% group_by(session, round, roundtype, env) %>% dplyr::summarize(pullEvents = sum(events))
pullEventsDF$env <- factor(pullEventsDF$env, levels = c('random','smooth'))
pullEventsDF$roundtype <- factor(pullEventsDF$roundtype, levels = c('solo','group'))
mPull<-  run_model(brm(pullEvents~env*roundtype + (1+env+roundtype|session), data =pullEventsDF,
                                 family = poisson,
                                 cores=4,  iter = 4000, warmup = 1000,  control = list(adapt_delta = 0.99)), modelName = 'pulls')

summary(mPull)

pPullReg <- plot_model(mPull, axis.labels =c('smooth:group', 'group', 'smooth'), transform=NULL,  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  value.offset = .4 ,value.size =3) +
  theme_classic()+
  #ylim(c(0.25, 7.7))+
  ylim(c(-1.2, 2))+
  ylab('Estimates')+
  ggtitle('Pull Events')

pPullReg

pPull <-  cowplot::plot_grid(pPulls,pPullReg, nrow= 2, rel_heights = c(1.3,1))
#ggsave('plots/pullWithReg.pdf', height = 5, width = 4, units = 'in')
 #########################################################
# Plot Example
#########################################################
#example <- eventDF[sample(nrow(eventDF), 1),] #sample randomly

#Sample based on criterion
pullID <-16 #good pull events: 10, 13, 14, 15, 16, ...
strongestPulls <- eventDF[eventDF$type == 'pull'&  eventDF$env == 'smooth'& eventDF$strength >=.15 & eventDF$disparity >=.15 &  (eventDF$t3 -  eventDF$t1)>20 & eventDF$roundtype=='group', ]  #define some criterion
example <- strongestPulls[pullID,] #Choose one 
sequenceDF <- subset(distanceDF, session == as.character(example$session) & round == example$round & p1==paste0('MPIB', min(example$leader, example$follower)) & p2 == paste0('MPIB', max(example$leader, example$follower)))

#Dyadic Distance
virPal <- viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,option = "D")
pPullDist <- ggplot(sequenceDF, aes(x = time, y = distance)) +
  geom_line()+
  #geom_point(data=subset(sequenceDF,  peak==TRUE), shape = 24, fill = '#99d8c9')+
  #geom_point(data=subset(sequenceDF,  valley==TRUE), shape = 25, fill = '#e34a33')+
  geom_vline(data = example, aes(xintercept =t1), linetype = 'dashed' , color ='black')+
  geom_vline(data = example, aes(xintercept =t2), linetype = 'dashed', color ='black')+
  geom_vline(data = example, aes(xintercept =t3), linetype = 'dashed', color ='black')+
  annotate(geom='text', x=example$t1 - 6, y = 47, label = 't[1]', parse=T, color ='black')+
  annotate(geom='text', x=example$t2 - 6, y = 47, label = 't[2]', parse=T, color ='black')+
  annotate(geom='text', x=example$t3 - 6, y = 47, label = 't[3]', parse=T,color ='black')+
  #scale_color_manual(values = virPal(3), name = '')+
  ylab('Dyadic Distance')+
  xlab('Time (s)')+
  #coord_cartesian(ylim = c(0 ,40))+
  theme_classic()+
  theme(legend.position='none')
pPullDist

#player position and block patterns
p1 <- subset(trajDF, sessions == as.character(example$session) & round == example$round & name ==paste0('MPIB', example$leader))
p2 <- subset(trajDF, sessions == as.character(example$session) & round == example$round & name ==paste0('MPIB', example$follower))
exampleBlocks <- subset(blockDF, session == as.character(example$session) & round == example$round & time<=example$t3)

initial <- expand.grid(x = seq(2,59, length.out = 20), z = seq(2,59, length.out = 20))

pPullTraj <- ggplot(p1, aes(x=x,y=y))+
  geom_point(data = initial,  aes(x = x, y = z), size=3, shape = 22, color = 'white', fill = '#027C00') +
  geom_point(data = exampleBlocks, aes(x = x, y = z, fill = reward), size=3, shape = 22, color = 'white')+
  scale_fill_manual(values=c("white", '#0096FF'), name ='Reward')+
  #coord_cartesian(xlim = c(5,30), ylim = c(5,30))+
  #Leader
  geom_path(data = subset(p1, time>example$t1 & time < example$t3), aes(alpha = time, color = 'leader'), size = 1)+
  geom_label(data=subset(p1, dplyr::near(time, example$t1)), label = 't[1]',parse=TRUE, color = leaderPal[2], fontface = "bold", alpha = .7)+ #NOTE: dplyr::near is due to a floating point issue, where the same apparent number isn't shown as identical
  geom_label(data=subset(p1,  dplyr::near(time, example$t2)), label = 't[2]',parse=TRUE,color = leaderPal[2], fontface = "bold", alpha = .7)+
  geom_label(data=subset(p1,  dplyr::near(time, example$t3)),  label = 't[3]',parse=TRUE, color = leaderPal[2], fontface = "bold", alpha = .7)+
  #Target
  geom_path(data = subset(p2, time>example$t1 & time < example$t3), aes(alpha = time, color = 'follower'), size =1)+
  geom_label(data=subset(p2,  dplyr::near(time, example$t1)), label = 't[1]',parse=TRUE, color = leaderPal[1], fontface = "bold", alpha = .7)+
  geom_label(data=subset(p2,  dplyr::near(time, example$t2)),  label = 't[2]',parse=TRUE,color = leaderPal[1], fontface = "bold", alpha = .7)+
  geom_label(data=subset(p2,  dplyr::near(time, example$t3)),  label = 't[3]',parse=TRUE,color = leaderPal[1], fontface = "bold", alpha = .7)+
  scale_color_manual(values = leaderPal, name='Player')+
  #theme_bw()+
  theme_void()+
  guides(fill=FALSE)+
  #coord_cartesian(ylim=c(3,40), xlim=c(3,40))+
  scale_alpha(range = c(0.3, 1), guide = 'none')+
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) #panel.background = element_rect(colour = "black", size=1), 
pPullTraj

pAggLeft<-  cowplot::plot_grid( pPullDist, pPullTraj, nrow = 2, labels = c('a', ''), rel_heights = c(1,2))


#########################################################
# Leadership (only in smooth:group rounds)
#########################################################

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
  # leaderProb <- leaderCounts / rowSums(leaderCounts)
  # followerProb <- followerCounts / rowSums(followerCounts)
  # leaderIndex = leaderProb - followerProb
  leaderDF <- as.data.frame(leaderShip) #convert to dataframe
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

#Regression: Does leadership predict rewards?
mRewardLeadership <- run_model(brm(avgScore~leadership*env + (1+leadership+env|session), data = leaderShipDF,   cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'rewardLeadership')
summary(mRewardLeadership)

p_rewardLeadership <- plot_model(mRewardLeadership,  axis.labels =c('leadership:smooth', 'smooth', 'leadership'),  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ggtitle('Avg. Score')+
  ylim(c(-.7, 5))
p_rewardLeadership


post <- mRewardLeadership %>% posterior_samples()
formatHDI(unlist(post$b_leadership),1)
formatHDI(unlist(post$b_leadership) +unlist(post$`b_leadership:envsmooth`) ,1)

#Now generate posterior predictions from model
newdat <-expand.grid(leadership = seq(min(leaderShipDF$leadership), max(leaderShipDF$leadership), length.out = 20), env = c('random', 'smooth'))
preds <- fitted(mRewardLeadership, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$avgScore <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

#corTestPretty(leaderShipDF$leadership, leaderShipDF$relScore)
#corTestPretty(leaderShipDF$leadership, leaderShipDF$relScore, method = 'kendall')

p1 <- ggplot(leaderShipDF, aes(x = leadership, y = avgScore, color = env, fill = env))+
  #geom_smooth(method= 'lm', fullrange = T, alpha = 0.2)+
  geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper),color = NA, alpha = 0.4)+ #posterior model predictions
  geom_line(data=fixedDF, size = 1)+
  geom_point(alpha = 0.4)+
  #facet_grid(~env)+
  ylab('Average Score')+
  xlab('Leadership')+
  scale_fill_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  #theme(legend.position = c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
p1

#Load visibility nets and see if leadership is related to in vs. out degree
visNetworkDF <- readRDS('networks/visNet.Rds')
playerVisDF <- visNetworkDF  %>% dplyr::filter(env == 'smooth') %>%  group_by(session, player) %>% dplyr::summarize(inWeight = mean(inWeight), outWeight = mean(outWeight), EC = mean(EC), score = mean(score))
playerVisDF <- dplyr::rename(playerVisDF, name = player)
#merge
visLeaderDF <-merge(leaderShipDF, playerVisDF, by=c('session', 'name'))

#cor.test(visLeaderDF$leadership, visLeaderDF$inWeight, method = 'kendall')
#Bayesian model
mVisInLeadership <- run_model(brm(inWeight~leadership+ (1+leadership|session), data = subset(visLeaderDF, env == 'smooth'),    cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'VisInLeadership')
summary(mVisInLeadership)

#Now generate posterior predictions from model
newdat <-data.frame(leadership =seq(from=min(visLeaderDF$leadership),to=max(visLeaderDF$leadership), length.out = 100 ))
preds <- fitted(mVisInLeadership, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$inWeight <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

p2 <- ggplot(visLeaderDF, aes(leadership, inWeight))+
      geom_point(alpha = 0.7, color =  "#009E73")+
      geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper),  color = 'NA',  fill = "#009E73", alpha = 0.4)+ #posterior model predictions
      geom_line(data=fixedDF, color =  "#009E73", size = 1)+
      #annotate(geom='text', label="paste(italic(r)[tau], \" = .22, \", italic(p), \" <.001\")", x = 0, y  = 1.2, parse = TRUE)+
      annotate(geom='text', label="0.02 [0.01,0.03]", x = 0, y  = 1.15)+
      ylab('In-degree (VisNet)')+
      xlab('Leadership')+
      #annotate(geom='text', label="paste(italic(r)[tau], \" = .49, \", italic(p), \" < .001\")", x = 5, y  = 1.1, parse = TRUE)
      scale_fill_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
      scale_color_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
      theme(legend.position = c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
p2

#corTestPretty(visLeaderDF$leadership, visLeaderDF$outWeight, method = 'kendall')
mVisOutLeadership <- run_model(brm(outWeight~leadership + (1+leadership|session), data = subset(visLeaderDF, env == 'smooth'),    cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'VisOutLeadership')
summary(mVisOutLeadership)
#plot_model(mVisInLeadership) + ylim(c(-0.07, .05))
fixef(mVisOutLeadership)

#Now generate posterior predictions from model
newdat <-data.frame(leadership =seq(from=min(visLeaderDF$leadership),to=max(visLeaderDF$leadership), length.out = 100 ))
preds <- fitted(mVisOutLeadership, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$outWeight <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  
p3 <- ggplot(visLeaderDF, aes(leadership, outWeight))+
  geom_point(alpha = 0.7, color =  "#009E73")+
  geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper),  color = 'NA',  fill = "#009E73", alpha = 0.4)+ #posterior model predictions
  geom_line(data=fixedDF, color =  "#009E73", size = 1)+
  #annotate(geom='text', label="paste(italic(r)[tau], \" = .22, \", italic(p), \" <.001\")", x = 0, y  = 1.2, parse = TRUE)+
  annotate(geom='text', label="-0.02 [-0.04,-0.0002]", x = 0, y  = 1.5)+
  ylab('Out-degree (VisNet)')+
  xlab('Leadership')+
  #annotate(geom='text', label="paste(italic(r)[tau], \" = .49, \", italic(p), \" < .001\")", x = 5, y  = 1.1, parse = TRUE)
  scale_fill_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position = c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
p3

#corTestPretty(visLeaderDF$leadership, visLeaderDF$inWeight - visLeaderDF$outWeight, method='kendall')
visLeaderDF$visDiff <- visLeaderDF$inWeight - visLeaderDF$outWeight
mVisDiffLeadership <- run_model(brm(visDiff~leadership + (1+leadership|session), data = subset(visLeaderDF, env == 'smooth'),    cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'VisDiffLeadership')
summary(mVisDiffLeadership)
#plot_model(mVisInLeadership) + ylim(c(-0.07, .05))
fixef(mVisDiffLeadership)

#Now generate posterior predictions from model
newdat <-data.frame(leadership =seq(from=min(visLeaderDF$leadership),to=max(visLeaderDF$leadership), length.out = 100 ))
preds <- fitted(mVisDiffLeadership, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$visDiff <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

p4 <- ggplot(visLeaderDF, aes(leadership,visDiff))+
  geom_point(alpha = 0.7, color =  "#009E73")+
  geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper),  color = 'NA',  fill = "#009E73", alpha = 0.4)+ #posterior model predictions
  geom_line(data=fixedDF, color =  "#009E73", size = 1)+
  #annotate(geom='text', label="paste(italic(r)[tau], \" = .22, \", italic(p), \" <.001\")", x = 0, y  = 1.2, parse = TRUE)+
  annotate(geom='text', label="0.04 [0.01,0.06]", x = 0, y  = 1)+
  ylab('In-degree - Out-degree\n (VisNet)')+
  xlab('Leadership')+
  #annotate(geom='text', label="paste(italic(r)[tau], \" = .49, \", italic(p), \" < .001\")", x = 5, y  = 1.1, parse = TRUE)
  scale_fill_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position = c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
p4


#Load distance nets and see if leadership is related to in centrality
distanceNetworkDF <- readRDS('networks/distNet.Rds')
#Calculate average per participant
distPlayerDF <- distanceNetworkDF %>%  dplyr::filter(env == 'smooth') %>% group_by(player, session, env) %>% dplyr::summarize(EC = mean(EC), score = mean(score)) 
playerDistDF <- dplyr::rename(distPlayerDF, name = player)
#merge
distLeaderDF <-merge(subset(leaderShipDF, env == 'smooth'), playerDistDF, by=c('session', 'name'))
#Bayesian model
mSpatialLeadership <- run_model(brm(EC~leadership + (1+leadership|session), data = distLeaderDF,    cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'SpatialLeadership')
summary(mSpatialLeadership)
#plot_model(mSpatialLeadership) + ylim(c(-0.07, .05))
fixef(mSpatialLeadership)

#Now generate posterior predictions from model
newdat <-data.frame(leadership =seq(from=min(visLeaderDF$leadership),to=max(visLeaderDF$leadership), length.out = 100 ))
preds <- fitted(mSpatialLeadership, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$EC <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

p5 <- ggplot(distLeaderDF, aes(leadership, EC))+
  geom_point(alpha = 0.7, color =  "#009E73")+
  geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper),  color = 'NA',  fill = "#009E73", alpha = 0.4)+ #posterior model predictions
  geom_line(data=fixedDF, color =  "#009E73", size = 1)+
  #annotate(geom='text', label="paste(italic(r)[tau], \" = .22, \", italic(p), \" <.001\")", x = 0, y  = 1.2, parse = TRUE)+
  annotate(geom='text', label="-0.01 [-0.02,-0.0004]", x = 0, y  = 1.05)+
  ylab('Centrality (ProximityNet)')+
  xlab('Leadership')+
  theme(legend.position = c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
p5


#Do leaders have higher instantenous reward rates?
timepoints <- eventDF %>% dplyr::filter(roundtype == 'group' & env == 'smooth' & type == 'pull')
rewardDifferential <- data.frame()
for (event in 1:nrow(timepoints)){
  #event <- 100 #DEBUG for testing
  #timepoints[event,]
  roundDF <- subset(blockDF, session==timepoints[event,'session'] & round == timepoints[event,'round'])
  t1 <- timepoints[event,'t1'] #timepoints
  t2 <- timepoints[event,'t2']
  t3 <- timepoints[event,'t3']
  leader <- paste0('MPIB', timepoints[event,'leader']) #roles
  follower <- paste0('MPIB', timepoints[event,'follower'])
  leaderRewards <- sum(subset(roundDF, name == leader & time>=t1 & time<=t3)$reward)
  followerRewards <- sum(subset(roundDF, name == follower & time>=t1 & time<=t3)$reward)
  rewardDifferential <- rbind(rewardDifferential, data.frame(t1 =t1, t3 = t3, leader = leader, follower = follower, session = timepoints[event,'session'], strength = timepoints[event,'strength'], disparity = timepoints[event,'disparity'], 
                                                             rewards  = c(leaderRewards, followerRewards), role = c('Leader', 'Follower'), eventId = event))
}


mRewardDiff <- run_model(brm(rewards~role + (1+role|eventId/session), data = rewardDifferential, family = poisson,   cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'pullRewardDiff')
summary(mRewardDiff)
#plot_model(mRewardDiff) + ylim(c(-0.07, .05))

post <- mRewardDiff %>% posterior_samples()
formatHDI(unlist(post$b_roleLeader),1)

#Now generate posterior predictions from model
newdat <-data.frame(role = c('Leader', 'Follower'))
preds <- fitted(mRewardDiff, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$rewards <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

pLeaderRewards <- ggplot(rewardDifferential, aes(x=role, y = rewards, color=role))+
  #geom_line(aes(group=eventId), color = 'black', alpha = 0.05)+
  geom_quasirandom(alpha = 0.2)+
  #stat_summary(fun.y=mean, geom='point')+
  geom_point(data = fixedDF)+
  labs(x= '', y = 'Rewards\n During Pull')+
  scale_color_manual(values=leaderPal)+
  geom_errorbar(data = fixedDF, aes(ymin = lower, ymax = upper), width = 0.2)+
  #theme(legend.position = 'none', axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  theme(legend.position = 'none')
  
pLeaderRewards



#How well can we predict score from 

mergedDF<-merge(visLeaderDF,playerDistDF, by = c('session', 'name', 'env', 'score') ) 
mRewardSoc <- run_model(brm(score~inWeight+outWeight+EC.y+leadership + (1+inWeight+outWeight+leadership+EC.y|session), data = subset(mergedDF, env=='smooth'),  cores=4,  iter = 4000, warmup = 1000,control = list(adapt_delta = 0.99),  backend = 'rstan'), modelName = 'rewardSoc')
summary(mRewardSoc)

pScoreSoc <-plot_model(mRewardSoc, axis.labels =c('leadership', 'Centrality',  'outDegree', 'inDegree'),  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ggtitle('Avg. Rewards (Smooth only)')+
  ylim(c(-5, 20))

pScoreSoc
ggsave('plots/scoreSocFeatures.pdf', pScoreSoc, width = 4.2, height = 3, units = 'in')


#########################################################
# Final plot
#########################################################

#main text figure
pAggLeft <- cowplot::plot_grid( NULL,  pPullDist, pPullTraj, nrow = 3, labels = c('a', '', ''), rel_heights = c(.2,1,2))
pAggBottomRight <- cowplot::plot_grid(p1+ylab('Avg. Reward'), pLeaderRewards, nrow = 1, labels = c('d', 'e'), align = 'v')
#pPullAggRight<- cowplot::plot_grid(pPulls,NULL,pPullReg, pAggBottomRight, rel_heights = c(1.15, -.1, .8, 1),  ncol = 1, labels  = c('b','', 'c', ''))
pPullAggRight<- cowplot::plot_grid(pPulls + theme(legend.position=c(0.05, 1.05), plot.margin = unit(c(5.5, 5,5, 5.5, 0), "pt")), p1+ylab('Avg. Reward') + theme(plot.margin = unit(c(0, 5,5, 5.5, 5), "pt")),  NULL, pLeaderRewards + theme(plot.margin = unit(c(5, 5,5, 5.5, -10), "pt")),  rel_heights = c(1.2, 1,.1, .7), ncol = 1, labels  = c('b', 'c', '', 'd'), hjust = -.1, vjust = c(1.5, .5,0,  0))

pPullAgg <- cowplot::plot_grid(pAggLeft, pPullAggRight, ncol = 2)
pPullAgg

ggsave('plots/pullMainText.pdf', pPullAgg, width = 6, height = 5, units = 'in')


p <- cowplot::plot_grid(p2,p3,p4,p5, labels = 'auto')
p

ggsave('plots/pullLeadership.pdf', p, width = 8, height = 5, units = 'in')


#########################################################
# Grave Yard
#########################################################

#########################################################
# Illustration of an anchor
#########################################################
# #example <- eventDF[sample(nrow(eventDF), 1),] #sample randomly
# anchors <- eventDF[eventDF$type == 'anchor' & (eventDF$t3 -  eventDF$t1)>20 & eventDF$roundtype=='group' & eventDF$env == 'smooth', ]  #define some criterion
# example <- anchors[2,] #Choose the first one 
# sequenceDF <- subset(distanceDF, session == as.character(example$session) & round == example$round & p1==paste0('MPIB', min(example$leader, example$follower)) & p2 == paste0('MPIB', max(example$leader, example$follower)))
# 
# #Dyadic Distance
# pAnchorDist <- ggplot(sequenceDF, aes(x = time, y = distance)) +
#   geom_line()+
#   #geom_point(data=subset(sequenceDF,  peak==TRUE), shape = 24, fill = '#99d8c9')+
#   #geom_point(data=subset(sequenceDF,  valley==TRUE), shape = 25, fill = '#e34a33')+
#   geom_vline(data = example, aes(xintercept =t1, color = 't1'), linetype = 'dashed')+
#   geom_vline(data = example, aes(xintercept =t2, color = 't2'), linetype = 'dashed')+
#   geom_vline(data = example, aes(xintercept =t3, color = 't3'), linetype = 'dashed')+
#   scale_color_manual(values = virPal(3), name = '')+
#   ylab('Pairwise Distance (blocks)')+
#   xlab('Time (s)')+
#   theme_classic()+
#   theme(legend.position = c(1,1), legend.justification = c(1,1))
# pAnchorDist
# 
# #player position and block patterns
# p1 <- subset(trajDF, sessions == as.character(example$session) & round == example$round & name ==paste0('MPIB', example$leader))
# p2 <- subset(trajDF, sessions == as.character(example$session) & round == example$round & name ==paste0('MPIB', example$follower))
# exampleBlocks <- subset(blockDF, session == as.character(example$session) & round == example$round & time<=example$t3)
# 
# pAnchorTraj <- ggplot(p1, aes(x=x,y=y))+
#   geom_point(data = exampleBlocks, aes(x = x, y = z, fill = reward), size=3, shape = 22, color = 'white')+
#   scale_fill_manual(values=c('#999999', '#0072B2'), name ='Reward')+
#   #coord_cartesian(xlim = c(5,30), ylim = c(5,30))+
#   #Leader
#   geom_path(data = subset(p1, time>example$t1 & time < example$t3), aes(alpha = time, color = 'more active'), size = 1.5)+
#   geom_label(data=subset(p1, dplyr::near(time, example$t1)), label = 1, color = cbPalette[7], fontface = "bold", alpha = .8)+ #NOTE: dplyr::near is due to a floating point issue, where the same apparent number isn't shown as identical
#   geom_label(data=subset(p1,  dplyr::near(time, example$t2)),  label = 2,color = cbPalette[7], fontface = "bold", alpha = .8)+
#   geom_label(data=subset(p1,  dplyr::near(time, example$t3)),  label = 3, color = cbPalette[7], fontface = "bold", alpha = .8)+
#   #Target
#   geom_path(data = subset(p2, time>example$t1 & time < example$t3), aes(alpha = time, color = 'less active'), size = 1.5)+
#   geom_label(data=subset(p2,  dplyr::near(time, example$t1)), label = 1, color = cbPalette[8], fontface = "bold", alpha = .8)+
#   geom_label(data=subset(p2,  dplyr::near(time, example$t2)),  label = 2, color = cbPalette[8], fontface = "bold", alpha = .8)+
#   geom_label(data=subset(p2,  dplyr::near(time, example$t3)),  label = 3, color = cbPalette[8], fontface = "bold", alpha = .8)+
#   scale_color_manual(values = rev(cbPalette[7:8]), name='Player')+
#   theme_classic()+
#   scale_alpha(range = c(0.4, 1), guide = 'none')
# pAnchorTraj
# 
# pAnchor <- plot_grid(pAnchorDist, pAnchorTraj, rel_widths = c(.8,1.2), labels = 'auto')
# pAnchor
# 
# ggsave(filename = 'plots/anchorExample.pdf', pAnchor, width = 8, height = 3.5, units = 'in')
# 



#Compute local minimum and maxima
#Finds peaks within a specified time window and where the change in distance is ≥ threshold
# find_peaks <- function (x, window = 1, threshold = 0){ #use -x to find valleys
#   shape <- diff(sign(diff(x, na.pad = FALSE))) #2nd order derivative
#   pks <- sapply(which(shape < 0), FUN = function(i){
#     z <- i - threshold + 1 #start of time window
#     z <- ifelse(z > 0, z, 1)
#     w <- i + threshold + 1 #end of time window
#     w <- ifelse(w < length(x), w, length(x))
#     if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1]) & x[i + 1] >= mean(x[c(z : i, (i + 2) : w)]) + threshold){ #1. point needs to be highest within window and #2. It needs to be larger than the average distance (within the window) by the threshold
#       return(i + 1)
#     }else{
#       return(numeric(0))
#     }
#   })
#   return(unlist(pks))
# }


# 
# distanceDF$p1 <- factor(distanceDF$p1, levels = c("MPIB1", "MPIB2", "MPIB3", "MPIB4"))
# distanceDF$p2 <- factor(distanceDF$p2, levels = c("MPIB1", "MPIB2", "MPIB3", "MPIB4"))
# saveRDS(distanceDF, 'analysis/trajectories/pilot2Distances.Rds')
# distanceDF <- readRDS('trajectories/pilot2Distances.Rds')
# 
# #Single comparison
# ggplot(subset(distanceDF, session == 'session13.json' & round == 14 & p1=='MPIB1' & p2 == 'MPIB2'), aes(x = time, y = distance))+
#   geom_line()+
#   geom_point(data=subset(distanceDF, session == 'session13.json' & round == 14 & p1=='MPIB1' & p2 == 'MPIB2' & peak==TRUE), shape = 24, fill = '#99d8c9')+
#   geom_point(data=subset(distanceDF, session == 'session13.json' & round == 14 & p1=='MPIB1' & p2 == 'MPIB2' & valley==TRUE), shape = 25, fill = '#e34a33')+
#   theme_classic()
# 
# 
# #multiple comparison
# ggplot(subset(distanceDF, session == 'session13.json' & round == 14), aes(x = time, y = distance))+
#   geom_line()+
#   facet_grid(p1~p2)+
#   geom_point(data=subset(distanceDF, session == 'session13.json' & round == 14  & peak==TRUE), shape = 24, fill = '#99d8c9', alpha = 0.7)+
#   geom_point(data=subset(distanceDF, session == 'session13.json' & round == 14  & valley==TRUE), shape = 25, fill = '#e34a33', alpha = 0.7)+
#   theme_classic()
  


####################################################################################################
#Push and Pull sequences
####################################################################################################
#Requirements
# distanceThreshold <- 1 #minimum change in distance between players (blocks)
# minSeqLength <- 20 #1 second as a minimum length of a sequence
# disparityThreshold <- 0.1
# strengthThreshold <- 0.1
# #Aso require that one individual (target) move less during the first segment and more during the second than the other (influencer)
# 
# #compute disparity (difference in movement patterns)
# disparityFun <- function(timestamps, p1, p2){
#   p1pos <- subset(p1, time %in% ts[timestamps])[,c('x','y')]
#   p2pos <- subset(p2, time %in% ts[timestamps])[,c('x','y')]
#   dist1 <- dist(p1pos)[c(1,3)] #compute distances between t1 and t2, and t2 and t3
#   dist2 <- dist(p2pos)[c(1,3)]
#   disparity <- (abs(dist1[1] - dist2[1]) * abs(dist1[2] - dist2[2])) / (abs(dist1[1] + dist2[1]) * abs(dist1[2] + dist2[2]))
#   disparity <- ifelse(is.na(disparity), 0, disparity) #replace NaN with -
#   return(disparity)
# }
# 
# #compute strength (magnitude of distance change as a function of baseline distance between individuals)
# strengthFun <- function(timestamps, distDF){
#   dValues <- dat[c(v1,p, v2),'distance'] #distances at each timepoint
#   strength <- (abs(dValues[2] - dValues[1]) * abs(dValues[3] - dValues[2]) ) / (abs(dValues[2] + dValues[1]) * abs(dValues[3] + dValues[2]) ) 
#   strength <- ifelse(is.na(strength), 0, strength)  #replace NaN with -
#   return(strength)
# }
# 
# #Compute sequences
# sequenceDF <- data.frame()
# distanceDF$dyad <- paste0(distanceDF$p1,'-',distanceDF$p2) #add dyad as column

# #Incredibly slow
# for (s in unique(playerDF$session)){ #slow
#   rounds <- unique(subset(playerDF, session == s & type == 'group')$round)
#   for (r in rounds){
#     for (pair in unique(distanceDF$dyad)){
#       dat <- subset(distanceDF, session == s  & round==r & dyad ==pair)
#       p1 <- subset(trajDF, sessions ==s & round == r & name ==as.character(dat$p1[1]))
#       p2 <- subset(trajDF, sessions ==s & round == r & name ==as.character(dat$p2[1]))
#       valleys <- which(dat$valley == TRUE)
#       peaks <- which(dat$peak == TRUE)
#       vValues <- dat[valleys,'distance']
#       plot(valleys, vValues)
#       #search for candidate events
#       for (v1 in valleys){ #loop through candidate starting points
#         
#       }
#       for (t in ts){ #loop through timesteps
#         #look for pulls
#         if (t %in% valleys){ #If this time point is a valley
#           v1 <- t
#           found <- F #search until found or options exhausted
#           while (found==F){
#             if (length(peaks[peaks>v1])>0){
#               for (p in peaks[peaks>v1]){#look for the next peak 
#                 if (length(valleys[valleys>p]>0)){
#                   for (v2 in valleys[valleys>p]){# look for the final valley
#                     deltaDistance <- abs(diff(dat[c(v1,p, v2),'distance'])) 
#                     if (max(deltaDistance)>=distanceThreshold){ #distance threshold check %TODO: Maybe this needs to be a min distance
#                       disparity <- disparityFun(c(v1,p,v2), p1, p2)
#                       if (abs(disparity>=disparityThreshold)){#disparity passed
#                         strength <- strengthFun(c(v1,p,v2), dat)
#                         if (strength >=strengthThreshold){#strength check passed
#                           #Now define roles
#                           p1pos <- subset(p1, time %in% ts[c(v1,p,v2)])[,c('x','y')]
#                           p2pos <- subset(p2, time %in% ts[c(v1,p,v2)])[,c('x','y')]
#                           dist1 <- dist(p1pos)[c(1,3)] #compute distances between t1 and t2, and t2 and t3
#                           dist2 <- dist(p2pos)[c(1,3)]
#                           roles <- sign(dist1-dist2) #negative if p1 moved less, positive if p1 moved more 
#                           if (sum(roles)==0){#one player needs to have moved less in one segment and more in the other
#                             influencer <- ifelse(roles[1]<0, p2$name[1], p1$name[1]) #if p1 moved less in segment one, then p2 is the influencer
#                             target <- ifelse(roles[1]<0, p1$name[1], p2$name[1])
#                             found <- T #finally we've found a pull
#                             eventDF <- rbind(eventDF, data.frame(type='pull', start = ts[v1], midpoint = ts[p], end = ts[v2], length = ts[v2]-ts[v1], influencer=influencer, target = target, round = r, session = s ))
#                           }
#                         }
#                       }
#                     }  
#                   }
#                 }else{ #no second valley to be found
#                   break
#                 }
#               }
#               }else{ #no more peaks to be found
#                 break
#               }
#             break
#           }}}}}}
# 
# saveRDS(eventDF, 'trajectories/pilot2Events.Rds')

#eventDF <- readRDS('trajectories/pilot2Events.Rds')


