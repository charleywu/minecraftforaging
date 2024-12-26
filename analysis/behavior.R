#Behavioral patterns
#Charley Wu, 2024
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
  scale_fill_manual(values= c("#E69F00","#009E73"), name = 'Env')+
  scale_color_manual(values= c("#E69F00","#009E73"), name = 'Env')+
  scale_linetype(name = 'Cond')+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  xlab('Time (s)')+ ylab('Normalized Reward Rate')+
  guides(linetype = guide_legend(override.aes= list(color = "black", fill = NA)))+
  #ggtitle('Normalized Reward Rate')+
  theme(legend.position=c(0.2,0.5),legend.justification = c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal")
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
#Reward dependent distance/turning angle
####################################################################################################
#Time since last reward analysis
rewardTimeDF <- readRDS('../cogmodeling/data/rewardTimes.RDS') #computed in rewardTimes.R


#Compute prev reward success
adaptDF <- data.frame()

for (i in unique(blockDF$id)){
  for (r in unique(blockDF$round)){
    #TESTING
    #i <- sample(unique(blockDF$id), 1)
    #r <- sample(unique(blockDF$round), 1)
    subdf <- subset(rewardTimeDF, id == i & round == r) #subset data
    subdf$prevReward <- lag(subdf$reward) #Compute prev reward
    subdf$distance <- c(NA, unlist(sapply(1:nrow(subdf), function(d)sqrt((subdf$x[d] - subdf$x[d-1])^2 + (subdf$z[d] - subdf$z[d-1])^2 )))) #distance between blocks
    #compute turning rate from spatial coordinates using method from Pacheco-Cobos et al., (PNAS 2019) 
    theta <- atan(abs(diff(subdf$z)/diff(subdf$x)))
    theta[is.nan(theta)] <- 0 #replace any NAs
    delta_turning <- abs(diff(theta))/pi #use absolute turning angle as a measure of turning rate
    subdf$delta_turning <- c(NA, NA, delta_turning) #append two NAs, since this is a difference of differences
    #compute time diff
    timediff <- subdf$time - lag(subdf$time, 1) 
    subdf$turningRate <- subdf$delta_turning  / timediff
    
    #put it together
    adaptDF <- rbind(adaptDF, subdf)
  }
}


#Ind means
adaptDist <- adaptDF %>% group_by(id, env, type, prevReward) %>% summarize(distance = mean(distance, na.rm=TRUE)) #ind means
adaptAngle <- adaptDF %>% group_by(id, env, type, prevReward) %>% summarize(delta_turning = mean(delta_turning, na.rm=TRUE)) #ind means

#Binary analysis based on prev reward

pAdaptDist <- ggplot(adaptDist[!is.na(adaptDist$prevReward),], aes(x=prevReward, y = distance, color = env, fill = env, linetype = type, shape = type))+
  stat_summary(fun.y=mean, geom='point', position = position_dodge(.5))+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar', width = 0.1, position = position_dodge(.5))+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_shape_manual(values = c(1,15), name = 'Condition')+
  scale_linetype(name='Condition')+
  xlab('Previous Block')+
  scale_x_discrete(labels = c("No Success", "Success"))+
  theme_classic()+
  guides(fill="none")+
  ylab('Distance Between Blocks')+
  scale_shape_manual(values = c(1,15), name = 'Condition')+
  theme(legend.position="none")
#coord_cartesian(ylim=c(0,32))+
#geom_signif(comparisons = list(c('group', 'solo')), color = 'black', annotation = c("NS"))+ 
#ggtitle('Rewards')+
#theme(legend.position=c(0,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pAdaptDist


#Regression
adaptRegDist <- adaptDF %>% dplyr::filter(!is.na(prevReward)) %>% group_by(id, env, type, prevReward) %>% summarize(distance = mean(distance, na.rm=TRUE))
adaptRegDist$type <- factor(adaptRegDist$type, levels = c('solo', 'group'))
adaptRegDist$env <- factor(adaptRegDist$env, levels = c('random', 'smooth'))
mAdaptDist <- run_model(brm(distance~env*type*prevReward + (1+env+type|id), data =adaptRegDist,
                            cores=4,  iter = 4000, warmup = 1000), modelName = 'adaptDistance')

summary(mAdaptDist)

#coefficient plot
pCoeff_adaptDist <- plot_model(mAdaptDist, 
                               axis.labels =c('smooth:group:prevReward','group:prevReward', 'smooth:prevReward', 'smooth:group', 'prevReward', 'group', 'smooth'), 
                               bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  #ylab('Est ± 95% CI')+
  ylim(c(-2, 1.5))+ 
  ggtitle('Foraging Distance')

pCoeff_adaptDist


fixef(mAdaptDist)

post <- mAdaptDist %>% posterior_samples() #posterior samples
formatHDI(post$`b_envsmooth:prevRewardTRUE`, signDig=1) 
formatHDI(post$b_prevRewardTRUE, signDig=1) #group effect in random



formatHDI(post$b_typegroup + post$`b_envsmooth:prevRewardTRUE`, signDig=2) 

#Turning Angle
adaptMeans <- adaptDF %>% group_by(id, env, type, prevReward) %>% summarize(delta_turning = mean(delta_turning, na.rm=TRUE)) #ind means

pAdaptAngle <- ggplot(adaptMeans[!is.na(adaptMeans$prevReward),], aes(x=prevReward, y = delta_turning, color = env, fill = env, linetype = type, shape=type))+
  stat_summary(fun.y=mean, geom='point', position = position_dodge(.5))+
  #stat_summary(fun.y=mean, aes(group=env), geom='line')+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar', width = 0.1, position = position_dodge(.5))+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  #scale_shape(name='Environment')+
  scale_linetype(name='Condition')+
  xlab('Previous Block')+
  scale_x_discrete(labels = c("No Success", "Success"))+
  theme_classic()+
  guides(fill="none")+
  ylab('Turning Angle')+
  scale_shape_manual(values = c(1,15), name = 'Condition')+
  #coord_cartesian(ylim=c(0,32))+
  #geom_signif(comparisons = list(c('group', 'solo')), color = 'black', annotation = c("NS"))+ 
  #ggtitle('Rewards')+
  theme(legend.position='right', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pAdaptAngle

#Regression
adaptReg <- adaptDF %>% dplyr::filter(!is.na(prevReward)) %>% group_by(id, env, type, prevReward) %>% summarize(delta_turning = mean(delta_turning, na.rm=TRUE))
adaptReg$type <- factor(adaptReg$type, levels = c('solo', 'group'))
adaptReg$env <- factor(adaptReg$env, levels = c('random', 'smooth'))
mAdapt <- run_model(brm(delta_turning~env*type*prevReward + (1+env+type|id), data =adaptReg,
                        cores=4,  iter = 4000, warmup = 1000), modelName = 'adaptTurning')

summary(mAdapt)

#coefficient plot
pCoeff_adapt <- plot_model(mAdapt, 
                           axis.labels =c('smooth:group:prevReward','group:prevReward', 'smooth:prevReward', 'smooth:group', 'prevReward', 'group', 'smooth'), 
                           bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  #ylab('Est ± 95% CI')+
  ylim(c(-.04, .06))+ 
  ggtitle('Turning Angle')

pCoeff_adapt


fixef(mAdapt)

post <- mAdapt %>% posterior_samples() #posterior samples
formatHDI(post$b_envsmooth, signDig=2) 
formatHDI(post$`b_envsmooth:prevRewardTRUE`, signDig=2) 
formatHDI(post$b_prevRewardTRUE, signDig=3) #group effect in random






#Does adaptivity predict performance?

ids <-  blockDF %>% group_by(id) %>% dplyr::summarize(points = sum(reward)) %>% pull(id)
adaptPerfDF <- data.frame()
for (envType in c('random', 'smooth')){
  for (condType in c('solo', 'group')){
    #difference between distance after failure vs. success
    distDiff <- adaptDist %>% dplyr::filter(env==envType & type==condType & !is.na(prevReward)) %>% mutate(distdiff = distance[prevReward==FALSE] - distance[prevReward==TRUE]) %>%  group_by(id) %>% summarize(distdiff = mean(distdiff)) %>% pull(distdiff)
    angleDiff <- adaptAngle %>% dplyr::filter(env==envType & type==condType & !is.na(prevReward)) %>% mutate(angleDiff = delta_turning[prevReward==FALSE] - delta_turning[prevReward==TRUE]) %>%  group_by(id) %>% summarize(angleDiff = mean(angleDiff)) %>% pull(angleDiff)
    rewards <-  blockDF %>% dplyr::filter(env==envType & type==condType) %>% group_by(id) %>% dplyr::summarize(points = sum(reward)) %>% pull(points)
    subdf <- data.frame(id = ids, reward = rewards, distanceDiff = distDiff, angleDiff =angleDiff, env = envType, cond = condType)
    cat(envType)
    cat(condType)
    print(corTestPretty(rewards,distDiff, method='kendall'))
    print(corTestPretty(rewards,angleDiff, method='kendall'))
    adaptPerfDF <- rbind(adaptPerfDF, subdf)
  }
}

#Adaptive Distance
pAdaptPerf <-  ggplot(adaptPerfDF, aes(x = distanceDiff, y = reward/4, color = env, fill = env, shape = cond))+
  geom_point(alpha = .7)+
  geom_smooth(data = subset(adaptPerfDF, distanceDiff<9), method='lm', fullrange = T)+
  facet_grid(cond~env, scales="free")+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  #scale_shape(name='Environment')+
  xlab('Adaptive Foraging distance\n Dist(Failure) - Dist(Success)')+
  theme_classic()+
  guides(fill="none")+
  ylab('Avg. Rewards per Round')+
  scale_shape_manual(values = c(1,15), name = 'Condition')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pAdaptPerf

#ggsave('plots/adaptPerf.pdf', pAdaptPerf, width = 6, height = 4, units = 'in')

#Adaptive Turning Angle
pAnglePerf <-  ggplot(adaptPerfDF, aes(x = angleDiff, y = reward/4, color = env, fill = env, shape = cond))+
  geom_point(alpha = .7)+
  geom_smooth( method='lm', fullrange = T)+
  facet_grid(cond~env, scales="free")+
  scale_fill_manual(values =c("#E69F00","#009E73"), name="Environment")+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  #scale_shape(name='Environment')+
  xlab('Adaptive Turning Angle\n Angle(Failure) - Angle(Success)')+
  theme_classic()+
  guides(fill="none")+
  ylab('Avg. Rewards per Round')+
  scale_shape_manual(values = c(1,15), name = 'Condition')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pAnglePerf

#ggsave('plots/adaptTurning.pdf', pAnglePerf, width = 6, height = 4, units = 'in')


# 
# #Continuous time analysis
# 
# pAdaptTime <- ggplot(adaptDF, aes(x=lastIndReward, y = delta_turning, color = env, fill =env, linetype = type))+
#   geom_smooth()+
#   scale_fill_manual(values =c("#E69F00","#009E73"), name="Env")+
#   scale_color_manual(values =c("#E69F00","#009E73"),  name="Env")+
#   scale_linetype(name='Cond')+
#   xlab('Time Since Reward')+
#   theme_classic()+
#   #coord_cartesian(xlim=c(2.25, 50))+
#   #guides(fill="none")+
#   ylab('Turning Rate')+
#   guides(linetype = guide_legend(override.aes= list(color = "black", fill = NA)))+
#   theme(legend.position=c(0.1,0.1), legend.justification = c(0,0),  strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal", legend.margin=margin())
#   
# pAdaptTime
# 
# 
# pAdaptTimeDist <- ggplot(adaptDF, aes(x=lastIndReward, y = distance, color = env, fill =env, linetype = type))+
#   geom_smooth()+
#   scale_fill_manual(values =c("#E69F00","#009E73"), name="Env")+
#   scale_color_manual(values =c("#E69F00","#009E73"),  name="Env")+
#   scale_linetype(name='Cond')+
#   xlab('Time Since Reward')+
#   theme_classic()+
#   #coord_cartesian(xlim=c(2.25, 50))+
#   #guides(fill="none")+
#   ylab('Distance')+
#   guides(linetype = guide_legend(override.aes= list(color = "black", fill = NA)))+
#   theme(legend.position=c(0.1,0.9), legend.justification = c(0,1),  strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal", legend.margin=margin())
# 
# pAdaptTimeDist
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
pMain <-cowplot::plot_grid(pRewardRateNormalized+ theme(legend.position = c(0.5, 0), legend.justification = c(0.5,0), legend.key.height = unit(-2, "cm")), 
                           pAdaptDist,
                           pDistTime+theme(legend.position='none') + ylab('Avg. Distance to Peers'),NULL, nrow = 1, rel_widths = c(1,.7,1,1), labels = 'auto') 
#pMain

ggsave('plots/behaviorTop.pdf', pMain, width = 12, height = 3, units = 'in')

#Reward SI
# pRewardSItop <- cowplot::plot_grid(pRewardRate,  pCoeff_Reward, rel_widths = c(1.3,1.7), labels = 'auto')
# pRewardSIbottom <- cowplot::plot_grid(pExpectedRewardrate + theme(legend.position = 'None'),  pCoeff_Rewardnorm, nrow = 1, labels = c('c', 'd', 'e'))                                   
#pRewardSI <- cowplot::plot_grid(pRewardSItop,pRewardSIbottom, nrow =2 )


pRewardSI <- cowplot::plot_grid(pRewardRate + theme(legend.position = c(0.5, 0.1), legend.justification = c(0.5,0), legend.key.height = unit(-2, "cm")),
                                pExpectedRewardrate+ theme(legend.position = 'None'), pCoeff_Reward,  pCoeff_Rewardnorm, nrow =2 , labels = 'auto')
                                    
ggsave('plots/rewardSI.pdf', pRewardSI, width = 12, height = 6, units = 'in')

# adaptivity SI
pAdaptSI <- cowplot::plot_grid(pCoeff_adaptDist, pAdaptAngle, pCoeff_adapt, labels = 'auto', nrow =1 )
ggsave('plots/adaptSI.pdf', pAdaptSI, width = 12, height = 3, units = 'in')


#Foraging SI

pForagingSI <- cowplot::plot_grid(pCoeff_adapt, pCoeff_distance, null, pForagingRateTime, p_foragingRateCoeff, nrow = 3, labels = 'auto')
ggsave('plots/foragingSI.pdf', pForagingSI, width = 12, height = 6, units = 'in')

