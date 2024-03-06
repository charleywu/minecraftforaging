#Behavioral patterns
#Charley Wu, 2023
rm(list=ls()) #house keeping

#load packages
packages <- c('tidyverse', 'trajr',  'signal','arrow',  'zoo', 'cowplot','jsonlite','lubridate', 'ggimage','ggbeeswarm', 'ggthemes','ggsignif', 'arrow', 
              'RColorBrewer', 'sjstats','lme4', 'brms', 'sjPlot', 'sjmisc',  'tidybayes')
invisible(lapply(packages, require, character.only = TRUE))
#setwd('analysis')

source('statisticalTests.R') #load stats
source('utilities.R') #load stats

theme_set(theme_classic()) #set theme
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette

####################################################################################################
# Load data and preprocess
####################################################################################################
blockDF <- data.frame()
playerDF <- data.frame()
#read in data

#2020 data
# dataFolders <- c('data/pilot2/','data/exp/','data/exp2/')   #Pilot data, first 2 sessions, sessions 3-7
# for (dataFolder in dataFolders){
#   playerDF <- rbind(playerDF, read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
#   blockDF <- rbind(blockDF, read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
# }

#2021 data
dataFolders <- c('data/2021batch/')   #Pilot data, first 2 sessions, sessions 3-7
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
  blockDF <- rbind(blockDF, arrow::read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
}


#add unique ids
blockDF$id <- paste0(blockDF$name, blockDF$session) 
playerDF$id <- paste0(playerDF$name, playerDF$session) 

####################################################################################################
# Demographic
####################################################################################################
filename <- 'data/demographics/Demographics_Minecraft_120122.csv' #Update
headers = read_delim(filename, col_names = FALSE, n_max = 1, delim=';')
demoDat = read_delim(filename, col_names = FALSE, delim=';')
colnames(demoDat)= headers
# demoDat <- demoDat %>% filter(group !='group')
nrow(demoDat)
#Age
mean(as.numeric(demoDat$age))
sd(as.numeric(demoDat$age))
#gender
table(demoDat$gender)

#Experience with Minecraft
table(demoDat['Minecraft experience']) # 1=No experience, 2=Minimal, 3=A bit , 4=A lot


#TODO: Add this to playerDF

#Earnings
scoreVec <- blockDF %>% group_by(id) %>% dplyr::summarize(points = sum(reward)) %>% pull(points)
euroVec <- 12 + scoreVec*.03
mean(euroVec)
sd(euroVec)



####################################################################################################
#Compute smoothed reward rate, cumultative reward rate, and blocks/rewards remaining
#Commented out because of long run time
####################################################################################################
# smoothingWindow <- 20 / .05 #for computing reward rate, in units of .05s
# rewardDF <- data.frame()
# timeseq <- seq(0,120, by=0.05)
# for (s in unique(blockDF$session)){
#   for (r in unique(blockDF$round)){
#     for (n in unique(blockDF$name)){
#       subdf <- subset(blockDF, session == s & round == r & name==n) #subset data
#       rewards <- data.frame(time = timeseq, session = s, round = r, name = n, env = subdf$env[1], type = subdf$type[1], reward = 0, block = 0) #establishes dataframe
#       #Compute reward rate, cumulative number of rewards, and rewards remaining
#       rewardTimes <- plyr::round_any(as.vector(unlist(subdf[subdf$reward==T,'time'])), .05) #vector of reward events, rounded to the nearest .05
#       rewardindx <- sapply(rewardTimes, FUN=function(x) which(x == timeseq)) #get index
#       rewards[rewardindx,'reward'] <- 1 #add reward
#       rewards$rewardRate <- rollmean(rewards$reward,k =  smoothingWindow, na.pad = T)*20 #computes the reward rate per second
#       rewards$cumSumReward <- cumsum(rewards$reward)
#       #Compute blocks destroyed, regardless of reward
#       blockTimes <- plyr::round_any(as.vector(unlist(subdf[,'time'])), .05)
#       blockindx <- sapply(blockTimes, FUN=function(x) which(x == timeseq)) #get index
#       rewards[blockindx,'block'] <- 1 #add block event
#       rewards$cumSumBlock <- cumsum(rewards$block)
#       #Compute rewards/blocks remaining for solo rounds
#       if (subdf$type[1] == 'solo'){ #compute solo rewards remaining
#         rewards$rewardsRemaining <- 100 - rewards$cumSumReward
#         rewards$blocksRemaining <- 400 - rewards$cumSumBlock
#       }else{ #Copmute at the group level
#         rewards$rewardsRemaining <- NA
#         rewards$blocksRemaining  <- NA
#       }
#       rewardDF <- rbind(rewardDF, rewards)
#     }
#     if (subdf$type[1] == 'group'){ #compute group level statistics for social rounds
#       subRound <- subset(rewardDF, session == s & round == r) #subset round
#       #Rewards
#       groupCumRewards <- subRound %>% group_by(time) %>% dplyr::summarize(groupCumSumReward = sum(cumSumReward)) %>% pull(groupCumSumReward)
#       groupRewardsRemaining <- 100 - groupCumRewards
#       rewardDF[rewardDF$session == s & rewardDF$round == r, 'rewardsRemaining'] <- rep(groupRewardsRemaining,4)
#       #Blocks
#       groupCumBlocks <- subRound %>% group_by(time) %>% dplyr::summarize(groupCumSumBlock = sum(cumSumBlock)) %>% pull(groupCumSumBlock)
#       groupBlocksRemaining <- 400 - groupCumBlocks
#       rewardDF[rewardDF$session == s & rewardDF$round == r, 'blocksRemaining'] <- rep(groupBlocksRemaining,4)
# 
#     }
#   }
# }
# rewardDF$expectedRewardRate <- rewardDF$rewardsRemaining / rewardDF$blocksRemaining
# rewardDF$env <-factor(rewardDF$env, levels =c('random', 'smooth'))
# rewardDF$id <-  paste0(rewardDF$name, rewardDF$session)
# saveRDS(rewardDF, 'simData/rewardDF.Rds')

####################################################################################################
#Rewards
####################################################################################################
#Run in the block above
rewardDF <- readRDS('simData/rewardDF.Rds')

#summarize by round
roundDF <- blockDF %>% group_by(id, env, type, round, session) %>% dplyr::summarize(reward = sum(reward))
roundDF$round_base <- rep(seq(1,4), nrow(roundDF)/4)
roundDF$env <- factor(roundDF$env, levels = c('random', 'smooth'))
roundDF$type <- factor(roundDF$type, levels = c('solo', 'group'))

#mean reward (over individuals x condition)
condDF <- blockDF %>% group_by(id, env, type, session) %>% dplyr::summarize(reward = sum(reward))
#Factor
condDF$env <- factor(condDF$env, levels = c('random', 'smooth'))
condDF$type <- factor(condDF$type, levels = c('solo', 'group'))

#Hierarchical regression model
mRewardCondition <- run_model(brm(reward~env*type + (1+env+type|id/session) , data =roundDF,
                                  family = poisson,cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'rewardCond')

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('group','solo'))
preds <- fitted(mRewardCondition, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$reward <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

post <- mRewardCondition %>% posterior_samples() #posterior samples
contrasts <- c(formatHDI(post$b_envsmooth, signDig=1), formatHDI( (post$b_envsmooth + unlist(post['b_envsmooth:typegroup'])) -  post$b_typegroup , signDig=1) )
formatHDI(unlist(post['b_envsmooth:typegroup']), signDig=1)

pCoeff_Reward <- plot_model(mRewardCondition,axis.labels =c('smooth:group', 'group', 'smooth'),  transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ggtitle('Reward')+
  ylim(c(-.25, .6))

pCoeff_Reward


pReward <-ggplot(condDF, aes(x=type, y= reward/4, color = env, fill = env))+
  geom_quasirandom(alpha = 0.3, dodge.width = .5)+
  geom_point(data = fixedDF,  aes(y = reward), position=position_dodge(width=0.5), color = 'black')+
  geom_errorbar(data = fixedDF, aes(ymin = lower, ymax = upper, width = 0.2), color = 'black', position=position_dodge(width=0.5))+
  #stat_summary(fun.y=mean, geom='point')+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  #stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  xlab('')+
  theme_classic()+
  guides(fill="none")+
  ylab('Avg Reward')+
  coord_cartesian(ylim=c(0,32))+
  geom_signif(y_position = c(25, 20), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = contrasts, tip_length = 0, color = 'black', vjust=-.5)+ 
  #geom_signif(comparisons = list(c('group', 'solo')), color = 'black', annotation = c("NS"))+ 
  #ggtitle('Rewards')+
  theme(legend.position=c(1,1.1), legend.justification = c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pReward


pRewardInset <-ggplot(condDF, aes(x=type, y= reward/4, color = env, fill = env))+
  #geom_quasirandom(alpha = 0.3, dodge.width = .5)+
  geom_point(data = fixedDF,  aes(y = reward), position=position_dodge(width=0.5))+
  geom_errorbar(data = fixedDF, aes(ymin = lower, ymax = upper, width = 0.2), position=position_dodge(width=0.5))+
  #stat_summary(fun.y=mean, geom='point')+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  #stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  xlab('')+
  theme_classic()+
  guides(fill="none")+
  ylab('Avg. Reward')+
  coord_cartesian(ylim=c(8,15))+
  #geom_signif(y_position = c(25, 20), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),annotation = contrasts, tip_length = 0, color = 'black', vjust=-.5)+ 
  #geom_signif(comparisons = list(c('group', 'solo')), color = 'black', annotation = c("NS"))+ 
  #ggtitle('Rewards')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pRewardInset



# #Learning Curves
# smoothingWindow <- 5/0.05 #smoothed over 5 seconds, based on a sampling rate of 0.05s
# curveDF <- blockDF%>% group_by(env, type, time) %>% dplyr::summarize(reward.mean = mean(reward)/4) %>% 
#   mutate(rewardRate.mean = rollmean(reward.mean,k =  smoothingWindow, na.pad = T)) 
# 
# #rolling average  
# pLearningCurve <- ggplot(curveDF, aes(x=time, y = rewardRate.mean, color = env, fill = env, linetype = type))+
#   #geom_ribbon(aes(ymin =rewardRate.lowCI, ymax = rewardRate.highCI), color = NA, alpha = 0.4 )+
#   geom_line()+
#   labs(x='Time (s)', y = 'Rewards per second')+
#   scale_color_manual(values=c("#E69F00","#009E73"))+
#   scale_fill_manual(values=c("#E69F00","#009E73"))
# pLearningCurve

#Smoothed GAM
pRewardRate <- ggplot(blockDF, aes(x=time, y = as.numeric(reward)/4, color = env, linetype = type, fill = env))+
  #geom_point(alpha = 0)+ #needs to be turned on for ggMarginals to work
  geom_smooth(alpha = 0.2, size = .7)+
  labs(x='Time (s)', y = 'Reward Rate')+
  scale_fill_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values=c("#E69F00","#009E73"), name = 'Environment')+
  scale_linetype(name = 'Condition')+
  coord_cartesian(ylim=c(0.06,0.13))+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  guides(linetype = guide_legend(override.aes= list(color = "black", fill = NA)))+
  theme(legend.position=c(0,0.8), legend.justification=c(0,1), legend.box='horizontal', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pRewardRate
#ggMarginal(pRewardRate, groupColour = TRUE, groupFill = TRUE, margins = "x",type = "histogram")


#learning over rounds (no real reliable effect, all interactions overlap with 0)
pRewardRounds <- ggplot(roundDF, aes(x=round_base, y= reward, color = env, fill = env))+
  #geom_quasirandom()+
  facet_grid(~type)+
  stat_summary(fun.y=mean, geom='point')+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
  geom_smooth(method = 'lm', alpha=.2)+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  xlab('Round')+
  ylab('Avg Reward')+
  ggtitle('Rewards')+
  theme(legend.position=c(0.1,1), legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pRewardRounds


#Run Bayesian mixed model
roundReg <- roundDF #clone
roundReg$env <- factor(roundReg$env, levels = c('random', 'smooth'))
roundReg$type <- factor(roundReg$type, levels = c('group', 'solo'))
roundReg$reward <- as.numeric(scale(roundReg$reward))
mRoundReward <- run_model(brm(reward~env*round*type + (1+env+round+type|id/session), data =roundReg,
                                cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'roundReward')



summary(mRoundReward)
#sjPlot::plot_model(mRoundReward)
#tab_model(mRoundReward)
# post <- posterior_samples(mRoundReward)
# sum(post['b_envsmooth:round:typesolo']>0)/length(unlist(post['b_envsmooth:round:typesolo'])) #Probability of coefficient > 0


####################################################################################################
#Reward Heatmap
####################################################################################################
rewardHeatmap <-blockDF %>% group_by(type, env,x,z) %>%  dplyr::summarise(n = n()) %>% mutate(freq = n / sum(n))


heatmapBlocks <- ggplot(rewardHeatmap, aes(x=x, y = z, fill=freq)) +
  geom_tile()+
  theme_classic() +
  facet_grid(type~env)+
  scale_fill_viridis_c(na.value=-1, name='P(block)')+
  coord_equal() +
  ggtitle('Blocks destroyed')+
  theme(strip.background=element_blank(), legend.key=element_rect(color=NA), axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(), panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
heatmapBlocks


#ggsave('plots/blockHeatmap.pdf', heatmapBlocks, width = 6, height =4.5, units = 'in')

####################################################################################################
#Reward Depletion
####################################################################################################
#Compute rewards remaining, expected reward rate, and normalized reward rate, by condition * env
depletionDF <- rewardDF %>% group_by(env, type, time) %>%
  dplyr::summarize(n = n(), rewardsRemaining.mean = mean(rewardsRemaining/100), rewardsRemaining.lowCI = ci(rewardsRemaining/100)[3], rewardsRemaining.highCI = ci(rewardsRemaining/100)[1],
                   expectedRewardRate.mean = mean(expectedRewardRate), expectedRewardRate.lowCI = ci(expectedRewardRate)[3], expectedRewardRate.highCI = ci(expectedRewardRate)[1],
                   normalizedRewardRate = mean(rewardRate/expectedRewardRate), normalizedRewardRate.lowCI = ci(rewardRate/expectedRewardRate)[3], normalizedRewardRate.highCI = ci(rewardRate/expectedRewardRate)[1])
depletionDF$type <- factor(depletionDF$type, levels = c('group', 'solo'))
depletionDF$env <- factor(depletionDF$env, levels = c('random', 'smooth'))
rewardDF$type <- factor(rewardDF$type, levels = c('group', 'solo'))

#Reward depletion
pRewardsRemaining <- ggplot(depletionDF, aes(x = time, y =rewardsRemaining.mean,  fill = env, linetype = type))+
  #geom_ribbon(aes(ymin =rewardsRemaining.lowCI, ymax = rewardsRemaining.highCI), color = NA, alpha = 0.4 )+
  geom_line(aes(color = env))+
  #stat_summary(fun.data = mean_cl_boot, geom= 'ribbon', color = NA, alpha = 0.2)+ #Almost no variance
  scale_color_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  scale_fill_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  scale_linetype(name='Condition')+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  guides(linetype = guide_legend(override.aes= list( fill = NA)))+
  xlab('Time (s)')+ ylab('Rewards Remaining')+
  theme(legend.position=c(0.1,0.1), legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pRewardsRemaining

#Expected Reward Rate
pExpectedRewardrate <- ggplot(depletionDF, aes(x = time, y = expectedRewardRate.mean, color = env, fill = env, linetype = type))+
  #geom_ribbon(aes(ymin =expectedRewardRate.lowCI, ymax = expectedRewardRate.highCI), color = NA, alpha = 0.4 )+
  geom_line(size = .7)+
  scale_color_manual(values = c("#E69F00","#009E73"), name = 'Environment')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_linetype(name='Condition')+
  guides(linetype = guide_legend(override.aes= list( fill = NA)))+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  xlab('Time (s)')+ ylab('Expected Reward Rate\n P(reward | blocks remaining)')+
  theme(legend.position=c(0.1,0.1), legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pExpectedRewardrate


#ANOVA
# normalizedDF <- rewardDF %>% group_by(id, env, type) %>% dplyr::summarize(normalizedReward = sum(reward/expectedRewardRate)/4 ) #divide by 4 rounds
# normalizedDF$type <- factor(normalizedDF$type)
# normalizedDF$env <- factor(normalizedDF$env)
# anova_stats(aov(data = normalizedDF,  normalizedReward ~env*type +  Error(id/(env*type))))
# invisible(bf <- anovaBF(reward ~env*type,  data=perfDF,  whichRandom="id"))
# bf

####################################################################################################
#Normalized reward (controlling for depletion)
####################################################################################################
#Round effects using normalized reward
normalizedrewardDF<- rewardDF %>% group_by(id, env, type, round, session) %>% dplyr::summarize(reward = mean((reward*20)/expectedRewardRate)) #*20 is to account for sampling rate
normalizedrewardDF$round <- (normalizedrewardDF$round %% 4) + 1 #redefine in terms of the 4 sequential rounds of each condition


#Run Bayesian mixed model
normalizedroundReg <- normalizedrewardDF #clone
normalizedroundReg$env <- factor(normalizedroundReg$env, levels = c('random', 'smooth'))
normalizedroundReg$type <- factor(normalizedroundReg$type, levels = c('solo', 'group'))
mRewardNorm <- run_model(brm(reward~env*type + (1+env+type|id/session), data =normalizedroundReg ,
                              cores=4,  iter = 4000, warmup = 1000), modelName = 'rewardNorm') 
summary(mRewardNorm)
#plot_model(mRewardNorm, sort.est = T) 
#marginal_effects(mRewardNorm)
#tab_model(mRewardNorm)
#post <- posterior_samples(mRewardNorm)

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('group','solo'))
preds <- fitted(mRewardNorm, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDFNormed <- newdat
fixedDFNormed$reward <- preds[,1]                   
fixedDFNormed$lower <- preds[,3]  
fixedDFNormed$upper <- preds[,4]  

post <- mRewardNorm %>% posterior_samples() #posterior samples
contrasts <- c(formatHDI(post$b_envsmooth, signDig=2) ,formatHDI(post$b_envsmooth+ unlist(post['b_envsmooth:typegroup']), signDig=2))
formatHDI(post$b_envsmooth, signDig=2)
formatHDI(post$b_typegroup, signDig=3)
formatHDI(post$`b_envsmooth:typegroup`, signDig=2)

pCoeff_Rewardnorm <- plot_model(mRewardNorm,axis.labels =c('smooth:group', 'group', 'smooth'),  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ggtitle('Normalized Reward')+
  ylim(c(-.05, .3))

pCoeff_Rewardnorm


condNormDF <- normalizedroundReg %>% group_by(id, env, type) %>% dplyr::summarize(reward = mean(reward)) #Average over rounds
pRewardNorm <-ggplot(condNormDF, aes(x=type, y= reward, color = env))+
  geom_quasirandom(alpha = 0.3, dodge.width = .5)+
  geom_point(data = fixedDFNormed,  position=position_dodge(width=0.5))+
  geom_errorbar(data = fixedDFNormed, aes(ymin = lower, ymax = upper, width = 0.2),  position=position_dodge(width=0.5))+
  #stat_summary(fun.y=mean, geom='point')+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  #stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  xlab('')+
  coord_cartesian(ylim=c(min(condNormDF$reward),1))+
  ylab('Avg Normalized Reward')+
  geom_signif(y_position = c(.9, 0.8), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
              annotation = contrasts, tip_length = 0, color = 'black', vjust=-.5)+ 
  #geom_signif(comparisons = list(c('group', 'solo')), y_position=1, tip_length = 0, color = 'black', vjust=-.5, annotation=formatHDI(post$b_typegroup, signDig=1))+ 
  #ggtitle('Rewards')+
  theme(legend.position=c(.95,1), legend.justification = c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pRewardNorm


pRewardNormInset <-ggplot(condNormDF, aes(x=type, y= reward, color = env))+
  #geom_quasirandom(alpha = 0.3, dodge.width = .5)+
  geom_point(data = fixedDFNormed,  position=position_dodge(width=0.5))+
  geom_errorbar(data = fixedDFNormed, aes(ymin = lower, ymax = upper, width = 0.2),  position=position_dodge(width=0.5))+
  #stat_summary(fun.y=mean, geom='point')+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  #stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  xlab('')+
  coord_cartesian(ylim=c(.27,.53))+
  ylab('Avg. Norm. Rew.')+
  #geom_signif(y_position = c(.9, 0.8), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),annotation = contrasts, tip_length = 0, color = 'black', vjust=-.5)+ 
  #geom_signif(comparisons = list(c('group', 'solo')), y_position=1, tip_length = 0, color = 'black', vjust=-.5, annotation=formatHDI(post$b_typegroup, signDig=1))+ 
  #ggtitle('Rewards')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pRewardNormInset



#Rate of rewards, normalized by depletion
pRewardRateNormalized<- ggplot(rewardDF, aes(x = time, y = (reward*20)/expectedRewardRate, color = env, fill = env, linetype = type))+
  geom_smooth(alpha = 0.2, size = .7)+
  #stat_summary(fun.y = mean, geom= 'line')+
  #stat_summary(fun.data = mean_cl_boot, geom= 'ribbon', color = NA, alpha = 0.2)+
  scale_fill_manual(values= c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values= c("#E69F00","#009E73"), name = 'Environment')+
  scale_linetype(name = 'Condition')+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  xlab('Time (s)')+ ylab('Normalized Reward Rate')+
  guides(linetype = guide_legend(override.aes= list(color = "black", fill = NA)))+
  #ggtitle('Normalized Reward Rate')+
  theme(legend.position=c(0.1,0.5),legend.justification = c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal")
pRewardRateNormalized

# pRewardNormalized <-rewardDF %>% group_by(id, env, type) %>% dplyr::summarize(reward = mean((reward*20)/expectedRewardRate)) %>% #*20 is to account for sampling rate
#   ggplot(aes(x=type, y= reward, color = env))+
#   #geom_quasirandom()+
#   stat_summary(fun.y=mean, geom='point')+
#   stat_summary(fun.y=mean, aes(group=env), geom='line')+
#   stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
#   scale_fill_manual(values =c("#E69F00","#009E73"), name="")+
#   scale_color_manual(values =c("#E69F00","#009E73"),  name="")+
#   xlab('')+
#   ylab('Avg Reward ± SE')+
#   ggtitle('Normalized Rewards')+
#   theme(legend.position=c(1,0.5), legend.justification = c(1,0.5), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pRewardNormalized

####################################################################################################
#Pairwise distances
####################################################################################################
distanceDF <- readRDS('trajectories/pairwiseDistances.Rds') #Computed in pullAnalysis.R
distanceDF$env <- factor(distanceDF$env, levels = c('random', 'smooth'))
pDist <- distanceDF %>% group_by(session, env, type, p1) %>% dplyr::summarize(avgDist= mean(distance)) %>%
  ggplot(aes(x = type, y = 1/avgDist, color  = env))+
  #geom_beeswarm()+
  stat_summary(fun.y=mean, geom='point')+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  stat_summary(fun.data = mean_se, geom='errorbar', width = 0.1)+
  #scale_x_continuous(breaks=seq(0,120,length.out=5))+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  xlab('')+
  ylab('Avg. Proximity (1/Dist)')+
  theme(legend.position =c(1,1), legend.justification =c(1,1))
pDist

pairDist <- distanceDF %>%  mutate(id = paste0(session, p1)) %>% group_by(env, type, id) %>% dplyr::summarize(avgDist= mean(distance)) %>% as.data.frame()
pairDist$type <- factor(pairDist$type)
pairDist$env <- factor(pairDist$env)
# anova_stats(aov(data = pairDist,  avgDist ~env*type +  Error(id/(env*type))))

#How does distance change over time?
distTimeDF <- distanceDF %>% group_by(env, type, time) %>% dplyr::summarize(avgDist= mean(distance), ssd = sd(distance, na.rm = TRUE), count = n()) %>%
  mutate(se = ssd / sqrt(count),
         lower_ci = avgDist - (qnorm(0.975)*se),
         upper_ci = avgDist + (qnorm(0.975)*se))
distTimeDF$type <- factor(distTimeDF$type, levels=c('group', 'solo'))

pDistTime <- ggplot(distTimeDF, aes(x = time, y = avgDist, color = env, fill = env, linetype = type))+
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci), alpha = 0.2, color = NA)+
  geom_line(size = .7)+
  theme_classic()+
  #stat_summary(fun=mean, geom='line')+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_fill_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_linetype(name = 'Condition')+
  guides(linetype = guide_legend(override.aes= list( fill = NA)))+
  ylab('Avg. Distance')+
  xlab('Time (s)')+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  #coord_cartesian(ylim=c(22,34))+
  theme(legend.position=c(.02,.02),legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal", legend.margin=margin())
pDistTime


#Bayesian regression
distanceReg <- distanceDF %>% group_by(env, type, session, p1, round) %>% dplyr::summarize(avgDist= mean(distance))
distanceReg$type <- factor(distanceReg$type, levels = c('solo', 'group'))
distanceReg$avgDist <- scale(distanceReg$avgDist)
mDist <- run_model(brm(avgDist~env*type + (1+env+type|p1/session), data =distanceReg,
                               cores=4,  iter = 4000, warmup = 1000), modelName = 'playerDistance')

summary(mDist)

#coefficient plot
pCoeff_distance <- plot_model(mDist, axis.labels =c('smooth:group', 'group', 'smooth'), bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  #ylab('Est ± 95% CI')+
  ggtitle('Pairwise Distance')+
  ylim(c(-.8, .6))

pCoeff_distance

fixef(mDist)

post <- mDist %>% posterior_samples() #posterior samples
formatHDI(post$b_typegroup, signDig=2) #group effect in random
formatHDI(post$b_typegroup + post$`b_envsmooth:typegroup`, signDig=3) 
formatHDI(post$b_envsmooth, signDig=1) #group effect in random


####################################################################################################
#Compute the time between block destruction events (Foraging Rate)
####################################################################################################

#COMPUTE THE TIMESTAMP OF THE FIRST AND LAST BLOCK DESTROYED ON EACH ROUND
firstlast <- blockDF %>% group_by(name,session,round,type,env) %>% 
  dplyr::summarize(time_last_hit=max(time), time_first_hit=min(time), num_hits=dplyr::n())
firstlast

#COMPUTE THE TIME DIFFERENCE BETWEEN EACH DESTROYED BLOCK EVENT
foragingRateDF <- blockDF %>% left_join(.,firstlast) %>%  #add first and last block destruction times
  dplyr::mutate(is_first_hit=ifelse(time_first_hit==time,1,0),is_last_hit=ifelse(time_last_hit==time,1,0)) %>% #label first and last hit events
  dplyr::arrange(name,session,round,type,env,time) %>% 
  dplyr::mutate(rownum=row_number()) %>% #add row number to uniquely identify each block destroyed
  dplyr::mutate(time_prev_hit=c(0,time[1:(length(time)-1)])) %>% #add the timestamp for the previous hit
  dplyr::mutate(time_prev_hit=ifelse(is_first_hit,0,time_prev_hit)) %>% #Set the first hit to 0
  dplyr::mutate(time_between_hits=time-time_prev_hit) %>% #compute time between hits
  dplyr::mutate(was_reward_previous=c(FALSE,reward[1:(length(reward)-1)])) 

#FORAGING RATE IS THE INVERSE OF TIME BETWEEN HITS
foragingRateDF$foragingRate <- (foragingRateDF$time_between_hits)^-1 #rate (blocks/s) is the inverse of time between blocks (s/block)


pForagingRate <- ggplot(foragingRateDF, aes(x = type, y = foragingRate, color = env))+
  stat_summary(fun.y = mean, geom= 'point')+
  stat_summary(fun.y = mean, aes(group=env), geom= 'line')+
  stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  xlab('Condition')+ ylab('Foraging Rate (blocks/s)')+
  theme( strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pForagingRate


pForagingRateTime <- ggplot(foragingRateDF, aes(x = time, y = foragingRate,  color = env, fill = env, linetype = type))+
  geom_smooth(linewidth = .7)+
  theme_classic()+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_fill_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_linetype(name = 'Condition')+
  guides(linetype = guide_legend(override.aes= list( fill = NA)))+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  xlab('Time (s)')+ylab('Avg. Foraging Rate (blocks/s)')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal")
pForagingRateTime




foragingRateDF$roundSeq <- (foragingRateDF$round %% 4) + 1
pForagingRateRound <- ggplot(foragingRateDF, aes(x = roundSeq, y = foragingRate, color = env))+
  stat_summary(fun.y = mean, geom= 'point')+
  facet_grid(~type)+
  stat_summary(fun.y = mean, aes(group=env), geom= 'line')+
  stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  xlab('Condition')+ ylab('Foraging Rate (blocks/s)')+
  theme(legend.position='right', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pForagingRateRound


#Bayesian Regression: blocks destroyed

blocksDestroyedDF <- blockDF %>% group_by(id, session, env, type, round) %>% dplyr::summarize(blocksDestroyed = n())
blocksDestroyedDF$type <- factor(blocksDestroyedDF$type, levels = c('solo', 'group'))
mBlocksDestroyed <- run_model(brm(blocksDestroyed~env*type + (1+env+type|id/session), data = blocksDestroyedDF,
                                  family = poisson,
                                  cores=4,  iter = 4000, warmup = 1000), modelName = 'blocksDestroyed')
#summary(blocksDestroyedDF)

p_foragingRateCoeff <- plot_model(mBlocksDestroyed,  axis.labels =c('smooth:group', 'group', 'smooth'), transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ggtitle('Foraging Rate')+
  ylim(c(-.1, .05))
p_foragingRateCoeff



post <- mBlocksDestroyed %>% posterior_samples() #posterior samples
formatHDI(post$b_envsmooth, signDig=2) 
formatHDI(post$`b_envsmooth:typegroup`, signDig=2) 


#tab_model(blocksDestroyedDF)


# ####################################################################################################
# #Compute the reward rate #out of date
# ####################################################################################################
# 
# #COMPUTE THE TIMESTAMP OF THE FIRST AND LAST REWARDS ON EACH ROUND
# firstlastReward <- blockDF %>% filter(reward==TRUE) %>% group_by(name,session,round,type,env) %>% 
#   dplyr::summarize(time_last_reward=max(time), time_first_reward=min(time), num_hits=n())
# firstlastReward
# 
# #COMPUTE THE TIME DIFFERENCE BETWEEN EACH REWARD EVENT
# rewardRateDF <- blockDF %>% filter(reward==TRUE)  %>%  left_join(.,firstlastReward) %>%  #add first and last block destruction times
#   mutate(is_first_reward=ifelse(time_first_reward==time,1,0),is_last_hit=ifelse(time_last_reward==time,1,0)) %>% #label first and last reward events
#   arrange(name,session,round,type,env,time) %>% 
#   mutate(rownum=row_number()) %>% #add row number to uniquely identify each block destroyed
#   mutate(time_prev_reward=c(0,time[1:(length(time)-1)])) %>% #add the timestamp for the previous hit
#   mutate(time_prev_reward=ifelse(is_first_reward,0,time_prev_reward)) %>% #Set the first hit to 0
#   mutate(time_between_reward=time-time_prev_reward) #compute time between hits
# 
# #FORAGING RATE IS THE INVERSE OF TIME BETWEEN HITS
# rewardRateDF$rewardRate <- (rewardRateDF$time_between_reward)^-1 #rate (reward/s) is the inverse of time between rewards (s/reward)
# 
# pRewardRate<- ggplot(rewardRateDF, aes(x = type, y = rewardRate, color = env))+
#   stat_summary(fun = mean, geom= 'point')+
#   stat_summary(fun = mean, aes(group=env), geom= 'line')+
#   stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('Condition')+ ylab('Reward Rate (rewards/s)')+
#   theme(legend.position='right', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pRewardRate
# 
# 
# #average at the individual level; this is the better plot to use once we get more data
# indrewardRateDF <- rewardRateDF %>% group_by(id, type, env) %>% dplyr::summarize(rewardRate = mean(rewardRate)) #average at the individual level
# pIndRewardRate <- ggplot(indrewardRateDF, aes(x = env, y = rewardRate, color = env))+
#   geom_quasirandom()+
#   geom_boxplot(color = 'black', fill = NA, outlier.shape = NA, width = 0.2)+
#   stat_summary(fun.y = mean, geom= 'point', shape = 23, color = 'black', size = 2)+
#   #stat_summary(fun.y = mean, geom= 'line')+
#   #stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
#   facet_grid(~type)+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('')+ ylab('Reward Rate (rewards/s)')+
#   theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pIndRewardRate
# 

####################################################################################################
#Compute the distance between block destruction events (movement rate)
####################################################################################################

distance = function(x,y,a,b){
  y = sqrt( (x-a)^2 + (y-b)^2 )
  return(y)
}
# BASED ON BLOCK DF, DISTANCE BETWEEN BLOCKS THAT WERE HIT (EUCLIDIAN DISTANCE)
blockDistanceDF <- foragingRateDF %>% arrange(name,session,round,type,env,time) %>%
  mutate(previous_x=c(NA,x[1:(length(x)-1)]), previous_z=c(NA,z[1:(length(z)-1)])) %>%
  group_by(name,session,round,type,env,time) %>%
  mutate(euclidian=distance(previous_x,previous_z,x,z)) 
blockDistanceDF

pBlockDistance <- ggplot(blockDistanceDF, aes(x = type, y = euclidian, color = env))+
  #geom_histogram(aes(y = stat(density) * .5), binwidth = .5, color = 'black', alpha = 0.7, size = 0.4) + 
  stat_summary(fun.y = mean, geom= 'point')+
  stat_summary(fun.y = mean, aes(group = env), geom= 'line')+
  stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  xlab('Condition')+ ylab('Distance between blocks' )+
  theme(legend.position=c(1,1), legend.justification=c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pBlockDistance

#Anova stats #out of date
# blockDistanceDFANOVA <- blockDistanceDF %>% group_by(id, env, type) %>% dplyr::summarize(dist = mean(euclidian, na.rm=T))
# blockDistanceDFANOVA$type <- factor(blockDistanceDFANOVA$type)
# blockDistanceDFANOVA$env <- factor(blockDistanceDFANOVA$env)
# anova_stats(aov(data = blockDistanceDFANOVA,  dist ~env*type +  Error(id/(env*type))))

blockDistanceDF$roundSeq <- (blockDistanceDF$round %%4) +1
pBlockDistanceRound <- ggplot(blockDistanceDF, aes(x = roundSeq, y = euclidian, color = env))+
  #geom_histogram(aes(y = stat(density) * .5), binwidth = .5, color = 'black', alpha = 0.7, size = 0.4) + 
  stat_summary(fun.y = mean, geom= 'point')+
  facet_grid(~type)+
  stat_summary(fun.y = mean, aes(group = env), geom= 'line')+
  stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  xlab('Condition')+ ylab('Distance between blocks' )+
  theme(legend.position=c(1,1), legend.justification=c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pBlockDistanceRound


# #average at the individual level; 
# indBlockDistanceDF <- blockDistanceDF %>% group_by(id, type, env) %>% dplyr::summarize(euclidian = mean(euclidian, na.rm=T)) #average at the individual level
# pIndBlockDistance <- ggplot(indBlockDistanceDF, aes(x = env, y = euclidian, color = env))+
#   geom_quasirandom()+
#   geom_boxplot(color = 'black', fill = NA, outlier.shape = NA, width = 0.2)+
#   stat_summary(fun.y = mean, geom= 'point', shape = 23, color = 'black', size = 2)+
#   #stat_summary(fun.y = mean, geom= 'line')+
#   #stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
#   facet_grid(~type)+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('')+ ylab('Distance between blocks' )+
#   theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pIndBlockDistance

####################################################################################################
#Total distance traveled
####################################################################################################
#COMPUTE TRAJECTORIES FROM PLAYERDF
trajDF <- readRDS('trajectories/Trajectories.Rds') #computed in trajectoryAnalysis.R

# summarize data, total euclidian distance
roundsmry <- trajDF %>% group_by(name,sessions,round,type,env) %>%
  dplyr::summarize(total_travel=sum(Mod(displacement),na.rm = T))

roundsmry$type <- factor(roundsmry$type, levels = c('group', 'solo'))
roundsmry$id <- paste0(roundsmry$name,roundsmry$sessions)

pMovementRate <- ggplot(roundsmry, aes(x = type, y = total_travel, color = env))+
  #geom_histogram(aes(y = stat(density) * .5), binwidth = .5, color = 'black', alpha = 0.7, size = 0.4) + 
  stat_summary(fun.y = mean, geom= 'point')+
  stat_summary(fun.y = mean, aes(group=env), geom= 'line')+
  stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = '')+
  xlab('Condition')+ ylab('Total Distance Traveled' )+
  theme(legend.position=c(1,1), legend.justification=c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pMovementRate

#Anova stats
movementDF <- roundsmry %>% group_by(id, env, type) %>% dplyr::summarize(dist = sum(total_travel)/4)
movementDF$type <- factor(movementDF$type)
movementDF$env <- factor(movementDF$env)
# anova_stats(aov(data = movementDF,  dist ~env*type +  Error(id/(env*type))))
# invisible(bf <- anovaBF(dist ~env*type,  data=movementDF,  whichRandom="id"))
# bf

# 
# #individual averages (over rounds)
# indDistanceDF <- roundsmry %>% group_by(name,sessions,type,env) %>% dplyr::summarize(avgDistance=mean(total_travel)) 
# pIndMovementRate <- ggplot(indDistanceDF, aes(x = env, y = avgDistance, color = env))+
#   geom_quasirandom()+
#   geom_boxplot(color = 'black', fill = NA, outlier.shape = NA, width = 0.2)+
#   stat_summary(fun.y = mean, geom= 'point', shape = 23, color = 'black', size = 2)+
#   #stat_summary(fun.y = mean, geom= 'line')+
#   #stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
#   facet_grid(~type)+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('')+ ylab('Total Distance Traveled' )+
#   theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pIndMovementRate
# 


####################################################################################################
# Put it all together: Main behavioral plot
####################################################################################################


#pRewardRate
#Main behavioral plot
pMain <-cowplot::plot_grid(pRewardRateNormalized+ theme(legend.position = c(0.5, 0), legend.justification = c(0.5,0), legend.key.height = unit(-2, "cm")), pDistTime+theme(legend.position='none') + ylab('Avg. Distance to Peers'),NULL, nrow = 1, labels = 'auto') 
#pMain

ggsave('plots/behaviorTop.pdf', pMain, width = 12, height = 3, units = 'in')

#Reward SI
# pRewardSItop <- cowplot::plot_grid(pRewardRate,  pCoeff_Reward, rel_widths = c(1.3,1.7), labels = 'auto')
# pRewardSIbottom <- cowplot::plot_grid(pExpectedRewardrate + theme(legend.position = 'None'),  pCoeff_Rewardnorm, nrow = 1, labels = c('c', 'd', 'e'))                                   
#pRewardSI <- cowplot::plot_grid(pRewardSItop,pRewardSIbottom, nrow =2 )
pRewardSI <- cowplot::plot_grid(pRewardRate + theme(legend.position = c(0.5, 0.1), legend.justification = c(0.5,0), legend.key.height = unit(-2, "cm")),pExpectedRewardrate+ theme(legend.position = 'None'), pCoeff_Reward,  pCoeff_Rewardnorm, nrow =2 , labels = 'auto')
                                    
ggsave('plots/rewardSI.pdf', pRewardSI, width = 12, height = 6, units = 'in')

#Foraging SI
pForagingSI <- cowplot::plot_grid(pCoeff_distance, null, pForagingRateTime, p_foragingRateCoeff, nrow = 2, labels = 'auto')
ggsave('plots/foragingSI.pdf', pForagingSI, width = 12, height = 6, units = 'in')


# #Alt layout
# #Reward SI
# pRewardSItop <-cowplot::plot_grid(pReward  + xlab('Condition')+ theme(legend.position=c(1,1.05)), pRewardRate,  pCoeff_Reward, labels = 'auto', nrow = 1)
# pRewardSIbottom <- cowplot::plot_grid(pExpectedRewardrate + theme(legend.position = 'None'), pRewardRateNormalized + theme(legend.position = 'None'), pCoeff_Rewardnorm, nrow = 1, labels = c('d', 'e', 'f'))                                   
# pRewardSI <- cowplot::plot_grid(pRewardSItop,pRewardSIbottom, nrow =2 )
# ggsave('plots/rewardSI.pdf', pRewardSI, width = 12, height = 6.3, units = 'in')
# 
# 
# #Foraging SI
# pForagingSI <- cowplot::plot_grid(pDistTime,pCoeff_distance, pForagingRateTime, p_foragingRateCoeff, nrow = 2, labels = 'auto')
# ggsave('plots/foragingSI.pdf', pForagingSI, width = 12, height = 6, units = 'in')




# #pRewardNorm #Alternative reward plot
# pPerformance<- cowplot::plot_grid(pReward+ggtitle(''), pRewardRounds+ggtitle('') + theme(legend.position = 'none'), pExpectedRewardrate+ggtitle('') + theme(legend.position = 'none'), pRewardRateNormalized+ggtitle(''),  rel_widths =  c(1,1.2, 1, 1.2), nrow = 2, labels = 'auto')
# pPerformance
# 
# ggsave('plots/performancePlots.pdf', pPerformance, width = 9, height = 8, units = 'in')
# 
# pBehavSI <- cowplot::plot_grid(pReward +ggtitle('') , 
#                              pRewardRounds+ggtitle('')+theme(legend.position = c(0.05,.95), legend.justification = c(0,1), legend.direction = "horizontal"),
#                              pExpectedRewardrate+ggtitle('') + theme(legend.position = c(0.05,.7), legend.justification=c(0,1)), 
#                              pRewardRateNormalized+ggtitle('')+ theme(legend.position = c(0.05,1), legend.justification=c(0,1), legend.direction = "horizontal"),
#                              pForagingRate, pBlockDistance, 
#                              ncol=3, labels='auto' )
# pBehavSI
# 
# pRewardRateNormalized
# 
# ggsave('plots/behaviorPlot.pdf', pBehav, width = 8, height = 10, units = 'in')

# #Create inset plots
# 
# insettedReward<-ggdraw(pRewardRate + theme(legend.position=c(0,1)))+
#   draw_plot(pRewardInset+
#               theme(text = element_text(size = 10),
#                     plot.background = element_rect(fill = "transparent",colour = NA)), .25, .16, .55, .35)
# insettedReward
# 
# insettedRewardNorm<-ggdraw(pRewardRateNormalized + theme(legend.position='none') )+
#   draw_plot(pRewardNormInset+
#               theme(text = element_text(size = 10),
#                     plot.background = element_rect(fill = "transparent",colour = NA)), .2, .05, .55, .35)
# insettedRewardNorm
# pDistTime

####################################################################################################
#GRAVEYARD
####################################################################################################


####################################################################################################
# Overview of data
####################################################################################################
# # get grasp of player DF
# head(playerDF)
# summary(playerDF)
# 
# playerDF <- playerDF %>% mutate(session=factor(session),
#                                 type=factor(type),
#                                 env=factor(env),
#                                 name=factor(name),
#                                 round=round+1)
# 
# # get a better grasp of the data; how many datapoints per person per round?
# # time: 20Hz, 20 samples per second (120 * 20)
# oneround <- playerDF %>% dplyr::filter(round==5 & type=="solo" & name=="MPIB1" & env=="random")
# View(oneround)
# 
# # get grasp of block DF
# head(blockDF)
# summary(blockDF)
# 
# blockDF <- blockDF %>% mutate(name=factor(name),
#                               session=factor(session),
#                               type=factor(type),
#                               env=factor(env),
#                               round=round+1) %>% select(!id)
# 
# oneblockDF <- blockDF %>% dplyr::filter(round==5 & type=="solo" & name=="MPIB1" & env=="random")
# View(oneblockDF)




####################################################################################################
#Covariance of trajectories
####################################################################################################
# #Compute unique pairs of participans
# dat <- expand.grid(V1 = unique(playerDF$name), V2 = unique(playerDF$name))
# colnames(dat) <- c('V1','V2')
# pairs <- unique(as.data.frame(t(apply(dat, 1, sort ))))
# pairs <- pairs[pairs$V1 != pairs$V2,]

# 
# trajCorDF <- data.frame()
# for (s in unique(playerDF$session)){
#   for (r in unique(playerDF$round)){
#     subd <- subset(playerDF, session == s & round == r)
#     for (pair in 1:nrow(pairs)){
#       p1 <- subset(subd, name == pairs[pair,'V1'])
#       p2 <- subset(subd, name == pairs[pair,'V2'])
#       correlation <- cor(c(p1$x,p1$z),  c(p2$x,p2$z))
#       MI <- mutinformation(round(c(p1$x,p1$z)),round(c(p2$x,p2$z)))
#       dtw <- DTW(cbind(p1$x, p1$z), cbind(p2$x, p2$z), pointSpacing = 20) #https://rdrr.io/cran/SimilarityMeasures/man/DTW.html; point spacing of 20 allows for a 1 second allowance
#       trajCorDF <- rbind(trajCorDF, data.frame(session = s, round = r, cor = correlation, MI = MI, dtw = dtw, pair = paste0(pairs[pair,'V1'], '-', pairs[pair,'V2']),type = subd$type[1], env = subd$env[1] ))
#     }
#   }
# }
# saveRDS(trajCorDF,'simData/trajCor.RDS')
# trajCorDF <- readRDS('simData/trajCor.RDS')
# 
# 
# pTrajCor <- ggplot(trajCorDF, aes(x = type, y = cor, color = env))+
#   #geom_histogram(aes(y = stat(density) * .5), binwidth = .5, color = 'black', alpha = 0.7, size = 0.4) + 
#   stat_summary(fun.y = mean, geom= 'point')+
#   stat_summary(fun.y = mean, aes(group=env), geom= 'line')+
#   stat_summary(fun.data = "mean_cl_boot", geom= 'errorbar', width = .1)+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('Condition')+ ylab('Correlation' )+
#   theme(legend.position=c(0.05,1),legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pTrajCor
# 
# #Dynamic time warping; https://link.springer.com/article/10.1007/s00265-019-2761-1 Equation 3
# pDTW <- ggplot(trajCorDF, aes(x = type, y = dtw, color = env))+
#   #geom_histogram(aes(y = stat(density) * .5), binwidth = .5, color = 'black', alpha = 0.7, size = 0.4) + 
#   stat_summary(fun.y = mean, geom= 'point')+
#   stat_summary(fun.y = mean, aes(group=env), geom= 'line')+
#   stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('Condition')+ ylab('Path Distance (DTW)' )+
#   theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pDTW
# 
# #Mutual information
# pTrajMI <- ggplot(trajCorDF, aes(x = env, y = MI, color = env))+
#   #geom_histogram(aes(y = stat(density) * .5), binwidth = .5, color = 'black', alpha = 0.7, size = 0.4) +
#   stat_summary(fun.y = mean, geom= 'point')+
#   stat_summary(fun.y = mean, geom= 'line')+
#   stat_summary(fun.data = mean_cl_boot, geom= 'errorbar', width = .1)+
#   scale_color_manual(values = c("#999999", "#E69F00"), name = '')+
#   facet_grid(~type)+
#   xlab('')+ ylab('Mutual Information' )+
#   theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pTrajMI


#cowplot::plot_grid(pTrajCor, pDTW, nrow = 2, labels = 'auto')
####################################################################################################
#Regressions
####################################################################################################
# TIME
# # frequentist
# wslsTime1f = lmer(log(time_between_hits) ~  was_reward_previous*type + (1|id), data = foragingRateDF) #interaction with type
# wslsTime2f = lmer(log(time_between_hits) ~ was_reward_previous*env + (1|id), data = foragingRateDF) #adds interaction with env 
# wslsTime3f = lmer(log(time_between_hits) ~ was_reward_previous*env*type + (1|id), data = foragingRateDF) #adds interaction with env and type 
# tab_model(wslsTime1f, wslsTime2f, wslsTime3f)
# multiplot(wslsTime1f, wslsTime2f, wslsTime3f)
# 
# # bayesian
# wslsTime1b = run_model(brm(log(time_between_hits) ~  was_reward_previous*type + (1|id), data = foragingRateDF, chains = 4, cores = 4, control=list(adapt_delta=0.99,max_treedepth=30)), modelName = "wslsTime1")
# wslsTime2b = run_model(brm(log(time_between_hits) ~ was_reward_previous*env + (1|id), data = foragingRateDF, chains = 4, cores = 4, control=list(adapt_delta=0.99,max_treedepth=30)), modelName = "wslsTime2")
# wslsTime3b = run_model(brm(log(time_between_hits) ~ was_reward_previous*env*type + (1|id), data = foragingRateDF, chains = 4, cores = 4, control=list(adapt_delta=0.99,max_treedepth=30)), modelName = "wslsTime3")
# #tab_model(wslsTime1b, wslsTime2b, wslsTime3b)
# plot_model(wslsTime3b, type = 'std', show.values = TRUE, value.offset = .3, sort.est = TRUE)
# 
# # plot marginal effects
# me <- marginal_effects(wslsTime3b)
# me
# 
# # DISTANCE
# # frequentist
# wslsDist1f = lmer(euclidian ~  was_reward_previous*type + (1|id), data = blockDistanceDF) #interaction with type
# wslsDist2f = lmer(euclidian ~ was_reward_previous*env + (1|id), data = blockDistanceDF) #adds interaction with env 
# wslsDist3f = lmer(euclidian ~ was_reward_previous*env*type + (1|id), data = blockDistanceDF) #adds interaction with env and type 
# tab_model(wslsDist1f, wslsDist2f, wslsDist3f)
# multiplot(wslsDist1f, wslsDist2f, wslsDist3f)
# 
# # bayesian
# wslsDist1b = run_model(brm(euclidian ~  was_reward_previous*type + (1|id), data = blockDistanceDF, chains = 4, cores = 4, control=list(adapt_delta=0.99,max_treedepth=30)), modelName = "wslsDist1")
# wslsDist2b = run_model(brm(euclidian ~ was_reward_previous*env + (1|id), data = blockDistanceDF, chains = 4, cores = 4, control=list(adapt_delta=0.99,max_treedepth=30)), modelName = "wslsDist2")
# wslsDist3b = run_model(brm(euclidian ~ was_reward_previous*env*type + (1|id), data = blockDistanceDF, chains = 4, cores = 4, control=list(adapt_delta=0.99,max_treedepth=30)), modelName = "wslsDist3")
# #tab_model(wslsDist1b, wslsDist2b, wslsDist3b)
# plot_model(wslsDist3b, type = 'std', show.values = TRUE, value.offset = .3, sort.est = TRUE)





####################################################################################################
# Aggregate regression plot
####################################################################################################

# 
# allRegDF <- data.frame()         
# modelList <- list(mRewardNorm, mDist, mBlocksDestroyed)
# modelNames <- c('Norm. Reward', 'Avg. Distance', 'Blocks Destroyed')
# coeffs <- c('b_envsmooth', 'b_typegroup', 'b_envsmooth:typegroup')
# for (m in 1:length(modelList)){
#   regM <- modelList[[m]]
#   post <-posterior_samples(regM) #posterior samples
#   ests <- data.frame((sapply(coeffs, FUN=function(coef){unlist(post[coef])})))
#   mDF<- data.frame(model = modelNames[[m]], median = apply(ests,2,median), lower = apply(ests,2,hdi)[,1],upper = apply(ests,2,hdi)[2,], coef  =  c('envSmooth', 'condGroup', 'smooth:group'))
#   allRegDF <- rbind(allRegDF, mDF)
# }
# 
# 
# 
# ggplot(allRegDF, aes(x = coef, y = median, color = model))+
#   geom_point(position=position_dodge(width=0.2))+
#   geom_errorbar(aes(ymin= lower, ymax = upper), width = 0.2, position=position_dodge(width=0.2))+
#   coord_flip(expand = TRUE)


