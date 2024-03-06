#Network analyses
rm(list=ls()) #house keeping
#setwd('analysis')

#load packages
packages <- c('ggplot2', 'trajr', 'tidyverse', 'scales',  'cowplot','jsonlite', 'DescTools', 'ggbeeswarm','brms', 'sjPlot','ggthemes','ggsignif', 'RColorBrewer',  'GGally', 'network', 'sna', 'igraph')
invisible(lapply(packages, require, character.only = TRUE))

source('statisticalTests.R')
source('utilities.R')
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
dataFolders <- c('data/2021batch/')   
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
  blockDF <- rbind(blockDF, arrow::read_feather(paste0(dataFolder, 'all_blocks_pd_cache.feather')))
}


#add unique ids
blockDF$id <- paste0(blockDF$name, blockDF$session) 
playerDF$id <- paste0(playerDF$name, playerDF$session) 

#Convert playerDF into long format
visDF <- playerDF  %>% pivot_longer(cols = MPIB4_visible:MPIB1_visible,  names_to = 'target', values_to = 'serverVis')
visDF$target <- factor(visDF$target)
levels(visDF$target) <- c("MPIB1","MPIB2", "MPIB3", "MPIB4")

####################################################################################################
#Load in visibility info from unity simulations; very slow and requires a lot of memory
####################################################################################################
# 
# #Loop and add unity simulations #SLOW
# pvisDF <- data.frame() #player
# #evisDF <- data.frame() #event
# 
# sessionList <- seq(1,32)
# dataFolder <- 'data/2021batch/'
# for (s in sessionList){
#   cat(s)
#   rounds <- seq(0,15)
#   for (r in rounds){ #skip training rounds
#     #Event visibility; Skipped due to memory issues
#     # evis <- read.csv(paste0(dataFolder, 'session', s,'_ge_',r+2, '_evis.log.csv'), header = T, sep=';') #Event DF; rounds there includue tutorial, so plus 2 correct for that
#     # evis$round <- r
#     # evis$session <- paste0('session', s, '.json')
#     # evisDF <- rbind(evisDF, evis)
#     #Player visibility
#     pvis <- read.csv(paste0(dataFolder, 'session', s,'_ge_',r+2, '_pvis.log.csv'), header = T, sep=';') #Replace visibility in playerDF with this visibility value
#     pvis <- pvis %>% pivot_longer(cols = P1:P4, names_to = 'target', values_to = 'unityVis')
#     pvis$round <- r
#     pvis$session <- paste0('session', s, '.json')
#     pvisDF <- rbind(pvisDF, pvis)
#   }
# }
# 
# saveRDS(pvisDF, 'simData/pvisDFprelim.Rds')
# pvisDF <- readRDS('simData/pvisDFprelim.Rds')
# 
# #Add round information to pvisDF
# pvisDF$target <- factor(pvisDF$target) #conform to match visDF
# levels(pvisDF$target) <-levels(visDF$target)
# visDF$name <- factor(visDF$name) #Factor
# #subset(pvisDF, session == 'session4.json' & round == 2)
# pvisDF$env <- NA #placeholder
# pvisDF$type <- NA
# for (s in unique(pvisDF$session)){
#   subdf <- subset(pvisDF, session == s)
#   roundVec <- unique(subdf$round)
#   for (r in roundVec){
#     roundDF <- subset(blockDF, session==s & round ==r)
#     pvisDF[pvisDF$session == s & pvisDF$round == r, 'env'] <- unique(roundDF$env)[1]
#     pvisDF[pvisDF$session == s & pvisDF$round == r, 'type'] <- unique(roundDF$type)[1]
#     }
# }
# 
# pvisDF$unityVis <- pvisDF$occupancy>0
# saveRDS(pvisDF, 'simData/pvisDF.Rds')
# #saveRDS(evisDF, 'simData/evisDF.Rds')
# arrow::write_feather(pvisDF, 'simData/pvisDF.feather') #save as feather to read into python
# #arrow::write_feather(evisDF, 'simData/evisDF.feather')
# 


pvisDF <- readRDS('simData/pvisDF.Rds') #computed above
#evisDF <- readRDS('simData/evisDF.Rds')

###################################################################################################
#Simple visiblity plots
####################################################################################################
#visibility over time
playerVisDF <- pvisDF %>% group_by(session, name, type, env, time)%>% dplyr::summarize(visible = mean(unityVis, na.rm=T) )
playerVisDF <- playerVisDF %>% group_by(type, env, time)%>% dplyr::summarize(avgVisible = mean(visible, na.rm=T), ssd = sd(visible, na.rm=TRUE), count = n() ) %>% 
  mutate(se = ssd / sqrt(count),
         lower_ci = avgVisible - (qnorm(0.975)*se),
         upper_ci = avgVisible + (qnorm(0.975)*se))

playerVisDF$type <- factor(playerVisDF$type, levels=c('group', 'solo'))

pVisTime <- ggplot(playerVisDF, aes(x = time, y = avgVisible, color = env, fill = env, linetype = type))+
  geom_ribbon(aes(ymin=lower_ci, ymax = upper_ci), alpha = 0.2, color = NA)+
  geom_line()+
  theme_classic()+
  #stat_summary(fun=mean, geom='line')+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_fill_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_linetype(name = 'Condition')+
  guides(linetype = guide_legend(override.aes= list( fill = NA)))+
  ylab('Avg. Visible Peers/Observers')+
  xlab('Time (s)')+
  scale_x_continuous(breaks=seq(0,120,length.out=5))+
  #coord_cartesian(ylim=c(22,34))+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal", legend.margin=margin())
pVisTime


pVisInset <- ggplot(playerVisDF, aes(x = type, y = avgVisible, color = env))+
  stat_summary(fun=mean, geom='point', position=position_dodge(.7))+
  stat_summary(fun.data=mean_cl_boot, geom = 'errorbar', width = 0.4, position=position_dodge(.7))+
  theme_classic()+
  #stat_summary(fun=mean, geom='line')+
  scale_color_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_fill_manual(values =c("#E69F00","#009E73"),  name="Environment")+
  scale_linetype(name = 'Condition')+
  ylab('Avg. Vis.')+
  xlab('')+
  #coord_cartesian(ylim=c(22,34))+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal", legend.margin=margin())
pVisInset

insettedVisTime<-ggdraw(pVisTime)+
    draw_plot(pVisInset+
                theme(text = element_text(size = 10),
                      plot.background = element_rect(fill = "transparent",colour = NA)), .4, .62, .50, .38)
insettedVisTime 

ggsave('plots/pVisTime.pdf', insettedVisTime, width = 4, height = 3, units = 'in' )

#Bayesian regression

visRegDF <- pvisDF %>% group_by(session, name, type, env)%>% dplyr::summarize(visible = mean(unityVis, na.rm=T) )
visRegDF$type <- factor(visRegDF$type, levels = c('solo', 'group'))
visRegDF$id <- paste0(visRegDF$session, visRegDF$name)
mVisReg <- run_model(brm(visible~env*type + (1+env+type|id/session), data = visRegDF,
                                  cores=4,  iter = 4000, warmup = 1000), modelName = 'visReg')
#summary(mVisReg)

#Coefficients
post <- mVisReg %>% posterior_samples() #posterior samples
formatHDI(post$b_envsmooth, signDig=2) 
formatHDI(post$b_typegroup, signDig=2) 
formatHDI(post$`b_envsmooth:typegroup` + post$b_envsmooth + post$b_typegroup, signDig=4)

formatHDI(post$`b_envsmooth:typegroup` + post$b_envsmooth + post$b_typegroup -  post$b_envsmooth, signDig=4) #smooth: group-solo
formatHDI(post$b_typegroup, signDig=5) #smooth: group-solo

p_VisCoeff <- plot_model(mVisReg,  axis.labels =c('smooth:group', 'group', 'smooth'), transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ylim(c(-.03, .03))+
  ggtitle('Visible Peers/Observers')
 
p_VisCoeff
ggsave('plots/pVisCoeff.pdf', p_VisCoeff, width = 6, height = 3, units = 'in' )
###################################################################################################
#Social visibility as a function of environment
#TODO: replace with more holistic network analysis
####################################################################################################
#Join pvis with visDF
#Compute number of visible players
socialD <- pvisDF %>% group_by(session, name, type, env) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
socialD$id <- paste0(socialD$session,socialD$name)
#Now add reward and num blocks destroyed from blockDF
blockInfo<- blockDF %>%  group_by(session, name, type, env) %>% dplyr:: summarize(reward = sum(reward)/4, blocksDestroyed = n()/4) 
socialD$reward <- blockInfo$reward
socialD$blocksDestroyed <- blockInfo$blocksDestroyed

#Simple t-test: Correlated rewards should produce higher rates of social info use
ttestPretty(subset(socialD, type == 'group' & env=='smooth')$avgVisible, subset(socialD, type == 'group' & env=='random')$avgVisible, paired=T)
ttestPretty(subset(socialD, type == 'solo' & env=='smooth')$avgVisible, subset(socialD, type == 'solo' & env=='random')$avgVisible, paired=T)

#Bayesian regression
SocialDReg <- pvisDF %>% group_by(session, name, type, env, round) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
SocialDReg$type <- factor(SocialDReg$type, levels = c('solo', 'group'))
SocialDReg$id <- paste0(SocialDReg$name,SocialDReg$session)
SocialDReg$avgVisible <- scale(SocialDReg$avgVisible)
mVisibility<- run_model(brm(avgVisible~env*type + (1+env+type|id/session), data =SocialDReg,
                            cores=4,  iter = 4000, warmup = 1000), modelName = 'visibilityEnv')

summary(mVisibility)


pVisEnvCoeff <-  plot_model(mVisibility, axis.labels =c('smooth:group', 'group', 'smooth'), bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ggtitle('Visibility')+
  ylim(c(-.3, .6))
pVisEnvCoeff

post <- as_draws(mVisibility, 'b_envsmooth:typegroup')
visEffect <- formatHDI(unlist(post), 2) #effect size

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('solo', 'group'))
preds <- fitted(mVisibility, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$avgVisible <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

pVis <- ggplot(subset(socialD, type == 'group'), aes(x = env, y = avgVisible,  color = env))+
  geom_line(aes(group=interaction(session,name)), color = 'grey', alpha = 0.2)+
  geom_quasirandom(alpha = 0.7)+
  geom_boxplot(width =.2, color = 'black', outlier.shape = NA, fill = NA)+
  stat_summary(fun = mean, geom='point', color = 'black', shape = 23, size = 2)+
  #stat_summary(fun.y = mean, geom='bar', color = 'black')+
  #stat_summary(fun.data = mean_se, geom='errorbar', color = 'black', width = .2)+
  labs(x = 'Environment', y = 'Out-degree\n (Visibility Network)')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position='none')+
  geom_signif(comparison=list(c('random', 'smooth')), test = 't.test', y_position = .47, color = 'black', tip_length = 0, annotations = visEffect)+
  coord_cartesian(ylim=c(0.05,0.5))
pVis


ggplot(socialD, aes(x = type, y = avgVisible, color = env))+
  geom_line(aes(group=interaction(session,name)), color = 'grey', alpha = 0.2)+
  geom_boxplot(width =.2, color = 'black', outlier.shape = NA, fill = NA)+
  geom_quasirandom(alpha = 0.7)+
  stat_summary(fun = mean, geom='point', color = 'black', shape = 23, size = 2)+
  labs(x = 'Environment', y = 'Out-degree\n (Visibility Network)')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position='none')+
  geom_signif(comparison=list(c('random', 'smooth')), test = 't.test', y_position = .47, color = 'black', tip_length = 0, annotations = visEffect)+
  coord_cartesian(ylim=c(0.05,0.5))+
  facet_wrap(~env)


#Does social attention use change over rounds? Not really
# roundSocialD <- pvisDF %>% group_by(session, name, type, env, round) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
# roundSocialD$round <- rep(seq(1,4), nrow(roundSocialD)/4) #convert all to seq(1,4)
# 
# #Run Bayesian mixed model
# roundSocialD$id <- paste0(roundSocialD$name,roundSocialD$session)
# roundSocialDReg <- subset(roundSocialD, type == 'group')
# roundSocialDReg$env <- factor(roundSocialDReg$env, levels = c('smooth', 'random'))
# mVisibilityRound <- run_model(brm(avgVisible~env*round + (1+env+round|id/session), data =roundSocialDReg,
#                                    cores=4,  iter = 4000, warmup = 1000), modelName = 'VisibilityRound')
# summary(mVisibilityRound)
# #plot_model(mVisibilityRound) + ylim(c(-0.07, .05))
# post <- posterior_samples(mVisibilityRound)
# sum(post['b_envrandom:round']>0)/length(unlist(post['b_envrandom:round'])) #Probability of coefficient > 0
# 
# pVisRound <- ggplot(subset(roundSocialD, type == 'group'), aes(x = round, y = avgVisible, color = env, fill = env))+
#   stat_summary(fun.y=mean, geom='point')+
#   stat_summary(fun.data=mean_se, geom='errorbar', width = 0.1)+
#   geom_smooth(method='lm', alpha = 0.2)+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   labs(y = 'Avg. Visible Peers', x= 'Round')+
#   theme(legend.position=c(0.05,0.05), legend.justification = c(0,0), strip.background=element_blank(),legend.background=element_blank(),legend.key=element_blank())
# pVisRound



####################################################################################################
#Build visibility network
####################################################################################################
# # # #Visibility
# visNetworkDF <- data.frame()
# for (s in unique(playerDF$session)) {
#   sesh <-  subset(blockDF, session == s)
#   rounds <- unique(sesh$round)
#   for (r in rounds){
#     #Extract score data
#     scoreDF <-  subset(blockDF, session == s & round == r )%>% group_by(name) %>% dplyr::summarize(score = sum(reward))
#     #extract visibility data
#     subDF <- subset(pvisDF, session ==s & round == r )
#     #subDF %>% group_by(name) %>% dplyr::summarize(visiblePeers =  mean(unityVis))
#     visibilityConnections <- data.frame()
#     for (p1 in unique(playerDF$name)){
#       for (p2 in unique(playerDF$name)){
#         if (p2!=p1){
#           vis <- data.frame(p1 = p1, p2 = p2, vis = mean(subset(subDF, name == p1 & target ==p2)$unityVis))
#           visibilityConnections <- rbind(visibilityConnections, vis)
#         }
#       }
#     }
#     g <-  graph.data.frame(visibilityConnections)
#     E(g)$weight <- visibilityConnections$vis
#     #Centrality
#     EC <- eigen_centrality(g) #compute eigen centrality
#     #Degree
#     inWeight <- strength(g, mode = 'in')
#     outWeight <- strength(g, mode = 'out')
#     #Plot
#     # # Set node size based on vis
#     # V(g)$name <- c('P1', 'P2', 'P3', 'P4')
#     # V(g)$EC <- EC$vector
#     # V(g)$size <- V(g)$EC * 20
#     # # Set edge width based on weight:
#     # E(g)$width <- E(g)$weight  * 10
#     # #change arrow size and edge color:
#     # E(g)$arrow.size <- 0.2
#     # E(g)$edge.color <- "gray80"
#     # plot(g, edge.arrow.size=.2, vertex.color="gold", vertex.size=15,
#     #     vertex.frame.color="gray", vertex.label.color="black",
#     #      vertex.label.cex=0.8, edge.curved=0.2)
# 
#     dummy <- data.frame(player = as_ids(V(g)), EC = EC$vector, score = scoreDF[match(as_ids(V(g)), scoreDF$name),'score'], session = s, round = r, env = unique(subDF$env), type = unique(subDF$type), inWeight = inWeight, outWeight = outWeight)
#     visNetworkDF <- rbind(visNetworkDF, dummy)
#   }
# }
# saveRDS(visNetworkDF, 'networks/visNet.Rds')
visNetworkDF <- readRDS('networks/visNet.Rds')
visNetworkDF$env <- factor(visNetworkDF$env, levels = c('random', 'smooth'))
visNetworkDF$type <- factor(visNetworkDF$type, levels = c('solo', 'group'))

# # #Plot example graph
# s <- 'session4.json' #example
# r <- 7
# scoreDF <-  subset(blockDF, session == s & round == r )%>% group_by(name) %>% dplyr::summarize(score = sum(reward))
# #extract visibility data
# subDF <- subset(pvisDF, session ==s & round == r )
# #subDF %>% group_by(name) %>% dplyr::summarize(visiblePeers =  mean(unityVis))
# visibilityConnections <- data.frame()
# for (p1 in unique(playerDF$name)){
#   for (p2 in unique(playerDF$name)){
#     if (p2!=p1){
#       vis <- data.frame(p1 = p1, p2 = p2, vis = mean(subset(subDF, name == p1 & target ==p2)$unityVis))
#       visibilityConnections <- rbind(visibilityConnections, vis)
#     }
#   }
# }
# g <-  graph.data.frame(visibilityConnections)
# E(g)$weight <- visibilityConnections$vis
# #Centrality
# EC <- eigen_centrality(g) #compute eigen centrality
# #Degree
# inWeight <- strength(g, mode = 'in')
# outWeight <- strength(g, mode = 'out')
# #Plot
# # Set node size based on vis
# V(g)$name <- c('1', '2', '3', '4')
# V(g)$EC <- EC$vector
# V(g)$size <- V(g)$EC * 20
# # Set edge width based on weight:
# E(g)$width <- E(g)$weight  * 10
# #change arrow size and edge color:
# E(g)$arrow.size <-  E(g)$weight
# E(g)$edge.color <- "gray80"
# plot(g,  vertex.color="gold",edge.arrow.mode = ">", vertex.size=20,
#                                           vertex.frame.color="black", vertex.label.color="black",
#                                           vertex.label.cex=0.8, edge.curved=0.2, edge.arrow.size=0.5)



visNetworkDF$id <- paste0(visNetworkDF$player, visNetworkDF$session)

#Calculate average per participant 
visPlayerDF <- visNetworkDF %>% group_by(id, session, env, type) %>% dplyr::summarize(inWeight = mean(inWeight), outWeight = mean(outWeight), EC = mean(EC), score = mean(score)) 


#Correlation between in and out degree
corTestPretty(visPlayerDF$inWeight, visPlayerDF$outWeight)
#random only
corTestPretty(subset(visPlayerDF, env == 'random' & type == 'group')$inWeight, subset(visPlayerDF, env == 'random'& type == 'group')$outWeight) 
corTestPretty(subset(visPlayerDF, env == 'random' & type == 'solo')$inWeight, subset(visPlayerDF, env == 'random'& type == 'solo')$outWeight) 

#smooth only
corTestPretty(subset(visPlayerDF, env == 'smooth' & type == 'group')$inWeight, subset(visPlayerDF, env == 'smooth'& type == 'group')$outWeight) 
corTestPretty(subset(visPlayerDF, env == 'smooth' & type == 'solo')$inWeight, subset(visPlayerDF, env == 'smooth'& type == 'solo')$outWeight) 


#Out degree from in degree
mOutIn <- run_model(brm(outWeight~env*type*inWeight + (1+env+type+inWeight|id/session), data =visPlayerDF ,
                        cores=4,  iter = 4000, warmup = 1000), modelName = 'outIn')

summary(mOutIn)
#
pCoeff_outIn <- plot_model(mOutIn,  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  ggtitle('Out-degree (Visibility Network)')
  ylim(c(-.7, .8))

pCoeff_outIn


#format plottable effect sizes
effects <- c(formatHDI(unlist(as_draws(mOutIn, 'b_inWeight')), signDig=1), formatHDI(unlist(as_draws(mOutIn, 'b_inWeight')) + unlist(as_draws(mOutIn, 'b_envsmooth:inWeight')), signDig=1))
formatHDI( unlist(as_draws(mOutIn, 'b_envsmooth:typegroup:inWeight')), signDig=2)
annotateDF <- data.frame(text = effects, env = c('random', 'smooth'), inWeight = c(0.8,1))

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('solo', 'group'), inWeight = seq(from=min(visPlayerDF$inWeight),to=max(visPlayerDF$inWeight), length.out = 100))
preds <- fitted(mOutIn, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$outWeight <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

pInVOut <- ggplot(visPlayerDF, aes(x = inWeight, y = outWeight, color = env, fill = type))+
  geom_point(aes(shape = type), alpha = 0.8) +
  #geom_line(aes(group =id), alpha = 0.2) +
  geom_ribbon(data = fixedDF, aes(group=type, ymin = lower, ymax = upper), color = NA, alpha = 0.4)+
  geom_line(aes(linetype = type), color = 'black', data = fixedDF)+
  #geom_smooth(aes(linetype=type), method = 'lm', alpha = 0.2, color = 'black')+
  facet_grid(~env, scales = 'free')+
  #coord_cartesian(ylim=c(0.2,1.5))+
  #geom_text(data = annotateDF,aes(label = text), y = 1.52, x=.8, color = 'black')+
  xlab('In-degree (Visibility Network)')+
  ylab('Out-degree\n (Visibility Network)')+
  scale_fill_manual(values =  c("light grey","black"), name = '')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_shape_manual(values = c(1,15), name = '')+
  scale_linetype_manual(values = c('dashed','solid'), name = '')+
  guides(color = FALSE, fill = FALSE)+
  theme(legend.position=c(1,1.1), legend.justification=c(1,1),strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pInVOut

#Effect sizes
formatHDI( unlist(as_draws(mOutIn, 'b_inWeight')), signDig=1)
formatHDI( unlist(as_draws(mOutIn, 'b_inWeight'))+unlist(as_draws(mOutIn, 'b_typegroup:inWeight')), signDig=1)

formatHDI( unlist(as_draws(mOutIn, 'b_inWeight')) + unlist(as_draws(mOutIn, 'b_envsmooth:inWeight')), signDig=1)
formatHDI( unlist(as_draws(mOutIn, 'b_inWeight'))+  +unlist(as_draws(mOutIn, 'b_envsmooth:inWeight')) + unlist(as_draws(mOutIn, 'b_typegroup:inWeight'))+ unlist(as_draws(mOutIn, 'b_envsmooth:typegroup:inWeight')), signDig=1)


#Difference in in/out degree between group and solo
# visDiff <- visPlayerDF %>% 
#   group_by(id, env) %>% 
#   summarize(inDegreeDiff = inWeight[type == "group"] - inWeight[type == "solo"], outDegreeDiff = outWeight[type == "group"] - outWeight[type == "solo"], inDegreeDiff = inWeight[type == "group"] - inWeight[type == "solo"], scoreDiff = score[type == "group"] - score[type == "solo"])
# 
# ggplot(visDiff, aes(x = inDegreeDiff, y = outDegreeDiff, color = env, fill = env))+
#   geom_point()+
#   geom_smooth(method = 'lm')

#ggsave('plots/inVsOut.pdf', pInVOut, width = 5, height = 4, unit = 'in')

#Change in degree between environments
changeDF <- visPlayerDF %>% pivot_longer(c(inWeight, outWeight), names_to = 'direction', values_to = 'weight') %>% group_by(id, type, direction) %>% summarize(randomWeight = weight[env=='random'], smoothWeight = weight[env=='smooth']) 
changeDF$direction <- factor(changeDF$direction, labels = c('In Degree', 'Out Degree'))

pconsistencyEnv <- ggplot(changeDF, aes(x = smoothWeight, y = randomWeight, color = interaction(type,direction)))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = 'lm', alpha = .2, color = 'black')+
  theme_classic()+
  scale_color_viridis_d()+
  facet_grid(type~direction, scales = 'free')+
  xlab('Smooth')+
  ylab('Random')+
  ggtitle('Consistency across environments')+
  theme(legend.position = 'none', strip.background=element_blank())
pconsistencyEnv

#chan ge in degree between types
changeDF <- visPlayerDF %>% pivot_longer(c(inWeight, outWeight), names_to = 'direction', values_to = 'weight') %>% group_by(id, env, direction) %>% summarize(soloWeight = weight[type=='solo'], groupWeight = weight[type=='group']) 
changeDF$direction <- factor(changeDF$direction, labels = c('In Degree', 'Out Degree'))

pconsistencyType<- ggplot(changeDF, aes(x = soloWeight, y = groupWeight, color = interaction(env,direction)))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = 'lm', alpha = .2, color = 'black')+
  theme_classic()+
  scale_color_viridis_d()+
  facet_grid(env~direction, scales = 'free')+
  xlab('Solo')+
  ylab('Group')+
  ggtitle('Consistency across game type')+
  theme(legend.position = 'none', strip.background=element_blank())
pconsistencyType

pConsistency <- cowplot::plot_grid(pconsistencyEnv, pconsistencyType)
# ggsave('plots/consistencyVis.pdf', pConsistency, width = 10, height = 3)


#in degree and score
corTestPretty(subset(visPlayerDF, env == 'smooth')$inWeight, subset(visPlayerDF, env == 'smooth')$score) #smooth only
pIn <- ggplot(visPlayerDF, aes(x = inWeight, y = score, color = env, fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  xlab('In-degree')+
  facet_grid(type~env, scales = 'free')+
  ylab('Score')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pIn

#out degree and score
corTestPretty(subset(visPlayerDF, env == 'smooth')$outWeight, subset(visPlayerDF, env == 'smooth')$score) #smooth only
pOut <- ggplot(visPlayerDF, aes(x = outWeight, y = score, color = env,  fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  facet_grid(type~env, scales = 'free')+
  xlab('Out-degree')+
  ylab('Score')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pOut

#Specialization
pSpec <- ggplot(visPlayerDF, aes(x = abs(inWeight-outWeight), y = score, color = env,  fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  facet_grid(type~env, scales = 'free')+
  xlab('In-degree - Out-degree')+
  ylab('Score')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position='none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pSpec


###################################################################################################
#Which group statistics predict group-level performance
####################################################################################################
visPlayerDF <- visPlayerDF %>% group_by(session, env, type) %>% mutate(groupScore = mean(score))
visPlayerDF$relScore <- visPlayerDF$score - visPlayerDF$groupScore
visPlayerDF$degDiff <- visPlayerDF$outWeight - visPlayerDF$inWeight

ggplot(visPlayerDF, aes(x = degDiff, y = relScore, color = env, fill = env))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = 'lm')+
  theme_classic()+
  labs(x='Out Degree - In Degree', y = 'Group Score')+
  facet_grid(type~env)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

groupDF <- visPlayerDF %>% group_by(session, env, type) %>% summarize(groupScore = mean(score), meanInDeg = mean(inWeight), meanOutDeg = mean(outWeight), meanDiff = mean(degDiff))

corTestPretty(subset(groupDF, env=='smooth')$meanInDeg, subset(groupDF, env=='smooth')$groupScore)
corTestPretty(subset(groupDF, env=='random')$meanInDeg, subset(groupDF, env=='random')$groupScore)
ggplot(groupDF, aes(x = meanOutDeg, y = groupScore, color = env, fill = env))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_classic()+
  #labs(x='Mean Imitation Weight', y = 'Group Score')+
  facet_grid(type~env)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())


changeDF <- groupDF %>% group_by(session, env) %>% summarize(scoreDiff = groupScore[type=='group']-groupScore[type=='solo'], outDegDiff = meanOutDeg[type=='group']-meanOutDeg[type=='solo'], inDegDiff = meanInDeg[type=='group']-meanInDeg[type=='solo'])
ggplot(changeDF, aes(x = outDegDiff, y = scoreDiff))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_classic()+
  facet_wrap(~env)+
  labs(x='Mean Out-degree (group - solo)', y = 'Mean score (group - solo)')+
  theme(legend.position = 'none', strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

###################################################################################################
#Does social attention comes at the cost of fewer blocks
####################################################################################################

#Sanity check: does social attention reduce the number of blocks that were destroyed? Yes it does
corTestPretty(subset(socialD, type == 'group' & env == 'smooth')$blocksDestroyed, subset(socialD, type == 'group' & env == 'smooth')$avgVisible, method = 'kendall')
corTestPretty(subset(socialD, type == 'group' & env == 'random')$blocksDestroyed, subset(socialD, type == 'group' & env == 'random')$avgVisible,  method = 'kendall')


socialReg <- pvisDF %>% group_by(session, name, type, env, round) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T))%>% arrange(session, name, type, env, round) #prepare more robust regression
blockInfo<- blockDF  %>% group_by(session, name, type, env, round) %>% dplyr:: summarize(reward = sum(reward), blocksDestroyed = n()) %>% arrange(session, name, type, env, round)
socialReg$reward <- blockInfo$reward
socialReg$type <- factor(socialReg$type, levels = c('solo', 'group'))
socialReg$blocksDestroyed <- blockInfo$blocksDestroyed
socialReg$id <- paste0(socialReg$name, socialReg$session)

mVisBlockDestruction <- run_model(brm(blocksDestroyed~avgVisible*env*type + (1+avgVisible+env|id/session),data = socialReg,
                                      family = poisson, cores=4,  iter = 4000, warmup = 1000), modelName = 'VisBlocksDestroyed')

formatHDI(unlist(as_draws(mVisBlockDestruction, 'b_avgVisible:envsmooth')), 4)

PVisBlocksDestroyedCoeff <- plot_model(mVisBlockDestruction, transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  #axis.labels =c('outDegree:smooth', 'smooth', 'outDegree'), 
  theme_classic()+
  xlab('')+
  #ylim(c(-.6,.15))+
  ggtitle('Blocks Destroyed')
  
PVisBlocksDestroyedCoeff

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'),type = c('solo', 'group'), avgVisible = seq(from=min(socialD$avgVisible),to=max(socialD$avgVisible), length.out = 100))
preds <- fitted(mVisBlockDestruction, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$blocksDestroyed <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  


pVisBlocksDestroyed <- ggplot(socialD, aes(x = avgVisible, y = blocksDestroyed, color = env, fill = env))+
  geom_point()+
  geom_ribbon(data = fixedDF, aes(ymin = lower, ymax = upper), color = NA, alpha = 0.4)+
  geom_line(data = fixedDF)+
  theme_classic()+
  xlab('Out-degree')+
  ylab('Blocks Destroyed per Round')+
  facet_wrap(~type)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position = c(.02, .02), legend.justification = c(0,0))

pVisBlocksDestroyed


###################################################################################################
#Does social attention come at the cost of fewer rewards
####################################################################################################

#Sanity check: does social attention reduce the number of blocks that were destroyed? Yes it does
corTestPretty(subset(socialD, type == 'group' & env == 'smooth')$reward, subset(socialD, type == 'group' & env == 'smooth')$avgVisible, method = 'kendall')
corTestPretty(subset(socialD, type == 'group' & env == 'random')$reward, subset(socialD, type == 'group' & env == 'random')$avgVisible,  method = 'kendall')


mVisReward <- run_model(brm(reward~avgVisible*env*type + (1+avgVisible+env+type|id/session),data = socialReg,
                            family = poisson, cores=4,  iter = 4000, warmup = 1000), modelName = 'VisRewards')
summary(mVisReward)

formatHDI(unlist(as_draws(mVisReward, 'b_avgVisible:envsmooth')), 1)

PVisRewardCoeff <- plot_model(mVisReward,  transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  #axis.labels =c('outDegree:smooth', 'smooth', 'outDegree'), 
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  #ylim(c(-.8,.7))+
  ggtitle('Rewards')
  
PVisRewardCoeff

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('solo', 'group'), avgVisible = seq(from=min(socialD$avgVisible),to=max(socialD$avgVisible), length.out = 100))
preds <- fitted(mVisReward, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$reward <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  


pVisReward <- ggplot(socialD, aes(x = avgVisible, y = reward, color = env, fill = env))+
  geom_point()+
  geom_ribbon(data = fixedDF, aes(ymin = lower, ymax = upper), color = NA, alpha = 0.4)+
  geom_line(data = fixedDF)+
  theme_classic()+
  xlab('Out-degree')+
  ylab('Rewards per Round')+
  facet_wrap(~type)+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position = c(.02, .02), legend.justification = c(0,0))

pVisReward

#Compute in degree, out degree, and score rank for each player
VisPlayerRoundDF <- visNetworkDF %>% group_by(player, session,  env, type)   %>% dplyr::summarize(inWeight = mean(inWeight), outWeight = mean(outWeight), EC = mean(EC), score = mean(score)) 
VisPlayerRoundDF$type <- factor(VisPlayerRoundDF$type, levels = c('solo', 'group'))
VisPlayerRoundDF$id <- paste0(VisPlayerRoundDF$name, VisPlayerRoundDF$session)

#Predict Score from visibility
mVisibilityScore <- run_model(brm(scale(score)~env*type*inWeight*outWeight + (1+env+type+inWeight+outWeight|id/session), data =VisPlayerRoundDF ,
                                  cores=4,  iter = 4000, warmup = 1000), modelName = 'VisibilityScore')

summary(mVisibilityScore)
plot_model(mVisibilityScore,  sort.est=TRUE)





####################################################################################################
#Build distance network
####################################################################################################
# # #Loop through data and compute EC for all players on all rounds; then correlate it with performance
# distanceDF <- readRDS('trajectories/pairwiseDistances.Rds') #Computed in pullAnalysis.R
# distanceNetworkDF <- data.frame()
# 
# for (s in unique(playerDF$session)) {
#   sesh <-  subset(blockDF, session ==s)
#   socialRounds <- unique(sesh$round)
#   for (r in socialRounds){
#     subDF <- subset(blockDF, session ==s & round == r )
#     subDistanceDF <- subset(distanceDF, session ==s & round == r )
#     #compute player scores
#     scoreDF <- subDF%>% group_by(name) %>% dplyr::summarize(score = sum(reward))
#     #Calculate average distance between participants
#     pairwiseDistances <- subDistanceDF %>% group_by(p1, p2) %>% dplyr::summarize(avgDistance = mean(distance))
#     edgeList <- data.frame(unique(t(apply(pairwiseDistances, 1, sort)))) #remove duplicates
#     names(edgeList) <- c('distance', 'p1', 'p2') #column names
#     edgeList <- edgeList[,order(ncol(edgeList):1)] #flip column order
#     #define weight as the inverse of distance; while also converting factor back to numeric
#     edgeList$weight <- as.numeric(edgeList$distance)^-1
#     #Create graph
#     g <- graph.data.frame(edgeList, directed = F)
#     #Compute centrality
#     EC <- eigen_centrality(g) #compute eigen centrality
#     degree <- strength(g)
#     #Save EC
#     dummy <- data.frame(player = as_ids(V(g)), EC = EC$vector, score = scoreDF[match(as_ids(V(g)), scoreDF$name),'score'], session =s, round = r, env = unique(subDF$env), type = unique(subDF$type), degree = degree)
#     distanceNetworkDF <- rbind(distanceNetworkDF, dummy)
#     #Plot graph (rather boring actually)
#     #V(g)$EC <- EC$vector
#     #V(g)$name <- c('P1', 'P2', 'P3', 'P4')
#     # Set node size based on EC
#     #V(g)$size <- V(g)$EC * 20
#     # Set edge width based on weight:
#     #E(g)$width <- E(g)$weight * 100
#     #change arrow size and edge color:
#     #E(g)$arrow.size <- 0.2
#     #E(g)$edge.color <- "gray80"
#     #plot(g, edge.arrow.size=.2, vertex.color="gold", vertex.size=15,vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=0.8, edge.curved=0.2)
# 
#   }
# }
# saveRDS(distanceNetworkDF, 'networks/distNet.Rds')

distanceNetworkDF <- readRDS('networks/distNet.Rds')

#Degree
#Calculate average per participant, relative to solo round as a baseline
distPlayerDF <- distanceNetworkDF %>% group_by(player, session, env, type) %>% dplyr::summarize(degree = mean(degree), score = mean(score)) 
distPlayerDF$env <- factor(distPlayerDF$env, levels=c('random', 'smooth'))
distPlayerDF$type <- factor(distPlayerDF$type, levels = c('solo', 'group'))
distPlayerDF$id <- paste0(distPlayerDF$player, distPlayerDF$session)

#is the a relationship between being centrality (being at the center of the action) and score?
mDistDegree <- run_model(brm(score~env*type*degree+ (1+env+type+degree|id/session), data =distPlayerDF ,
                         cores=4,  iter = 4000, warmup = 1000), modelName = 'distDeg')

summary(mDistDegree)
PDistDegCoeff <- plot_model(mDistDegree,  transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  #axis.labels =c('outDegree:smooth', 'smooth', 'outDegree'), 
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  #ylim(c(-.8,.7))+
  ggtitle('Avg. Score')

PDistDegCoeff

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('solo', 'group'), degree = seq(from=min(distPlayerDF$degree),to=max(distPlayerDF$degree), length.out = 100))
preds <- fitted(mDistDegree, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$score <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

#fixedDF <- fixedDF[!(fixedDF$env == 'random' &fixedDF$EC>.17),] #trim predictions that are outside the raw data

pDistDeg <- ggplot(distPlayerDF, aes(x = degree, y = score, color = env, fill = type))+
  geom_point(aes(shape = type), alpha = 0.8) +
  geom_ribbon(data = fixedDF, aes(group=type, ymin = lower, ymax = upper), color = NA, alpha = 0.4)+
  geom_line(aes(linetype = type), color = 'black', data = fixedDF)+
  theme_classic()+
  xlab('Degree (Proximity Network)')+
  ylab('Avg. Reward')+
  facet_grid(~env, scales = 'free')+
  scale_fill_manual(values =  c("light grey","black"), name = '')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_shape_manual(values = c(1,15), name = '')+
  scale_linetype_manual(values = c('dashed','solid'), name = '')+
  guides(color = FALSE, fill = FALSE)+
  theme(legend.position=c(0,1.1), legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pDistDeg


#Eigenvector centrality
#Calculate average per participant, relative to solo round as a baseline
distPlayerDF <- distanceNetworkDF %>% group_by(player, session, env, type) %>% dplyr::summarize(EC = mean(EC), score = mean(score)) 
distPlayerDF$env <- factor(distPlayerDF$env, levels=c('random', 'smooth'))
distPlayerDF$type <- factor(distPlayerDF$type, levels = c('solo', 'group'))
distPlayerDF$id <- paste0(distPlayerDF$name, distPlayerDF$session)



#is the a relationship between being centrality (being at the center of the action) and score?
mDistEC <- run_model(brm(score~env*type*EC+ (1+env+type+EC|id/session), data =distPlayerDF ,
                                          cores=4,  iter = 4000, warmup = 1000), modelName = 'distEC')

summary(mDistEC)
PDistECCoeff <- plot_model(mDistEC,  transform = NULL, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  #axis.labels =c('outDegree:smooth', 'smooth', 'outDegree'), 
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  #ylim(c(-.8,.7))+
  ggtitle('Avg. Score')

PDistECCoeff

#Now generate posterior predictions from model
newdat <-expand.grid(env = c('random', 'smooth'), type = c('solo', 'group'), EC = seq(from=min(distPlayerDF$EC),to=max(distPlayerDF$EC), length.out = 100))
preds <- fitted(mDistEC, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))

#create new fixed effects dataframe
fixedDF <- newdat
fixedDF$score <- preds[,1]                   
fixedDF$lower <- preds[,3]  
fixedDF$upper <- preds[,4]  

fixedDF <- fixedDF[!(fixedDF$env == 'random' &fixedDF$EC<.7),] #trim predictions that are outside the raw data

pDistEig <- ggplot(distPlayerDF, aes(x = EC, y = score, color = env, fill = type))+
  geom_point(aes(shape = type), alpha = 0.8) +
  geom_ribbon(data = fixedDF, aes(group=type, ymin = lower, ymax = upper), color = NA, alpha = 0.4)+
  geom_line(aes(linetype = type), color = 'black', data = fixedDF)+
  theme_classic()+
  xlab('Centrality (Proximity Network)')+
  ylab('Avg. Reward')+
  facet_grid(~env, scales = 'free')+
  scale_fill_manual(values =  c("light grey","black"), name = '')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_shape_manual(values = c(1,15), name = '')+
  scale_linetype_manual(values = c('dashed','solid'), name = '')+
  guides(color = FALSE, fill = FALSE)+
  theme(legend.position=c(0,1.1), legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pDistEig



formatHDI(unlist(as_draws(mDistEC, 'b_EC')) + unlist(as_draws(mDistEC, 'b_envsmooth:EC'))+ unlist(as_draws(mDistEC, 'b_envsmooth:typegroup:EC'))+ unlist(as_draws(mDistEC, 'b_typegroup:EC')), 1)
formatHDI(unlist(as_draws(mDistEC, 'b_EC')) + unlist(as_draws(mDistEC, 'b_envsmooth:EC')), 1)


formatHDI(unlist(as_draws(mDistEC, 'b_EC')) + unlist(as_draws(mDistEC, 'b_typegroup:EC')), 1)
formatHDI(unlist(as_draws(mDistEC, 'b_EC')), 1)

####################################################################################################
#Merge networks
####################################################################################################
#Merge vis and dist nets
mergedNetworkDF <- merge(visNetworkDF ,distanceNetworkDF, by = c('player', 'session', 'env', 'type', 'round', 'score'), suffixes = c('vis','dist')) 
#Compute mean statistics per player
mergedPlayerNetworkDF <- mergedNetworkDF %>% group_by(session, player, env, type) %>% dplyr::summarize(score = mean(score), ECvis = mean(ECvis), inWeight = mean(inWeight), outWeight = mean(outWeight), ECdist = mean(ECdist), distWeight = mean(degree))

pECbothNets <- ggplot(mergedPlayerNetworkDF, aes(x = ECdist, y = ECvis, color = env, fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  xlab('Centrality\n (Proximity Network)')+
  ylab('Centrality\n (Visibility Network)')+
  facet_wrap(~type)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position=c(0.05,0.05), legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pECbothNets

pInDist <- ggplot(mergedPlayerNetworkDF, aes(x = distWeight^-1, y = inWeight, color = env, fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  xlab('Avg. Distance')+
  ylab('In-degree\n (VisNet)')+
  facet_wrap(~type)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position=c(0.05,0.95), legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pInDist

pOutDist <- ggplot(mergedPlayerNetworkDF, aes(x = distWeight^-1, y = outWeight, color = env, fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  xlab('Avg. Distance')+
  ylab('Out-degree\n (VisNet)')+
  facet_wrap(~type)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position=c(0.05,0.05), legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pOutDist

pOutEC <- ggplot(mergedPlayerNetworkDF, aes(x = outWeight, y = ECdist, color = env, fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  xlab('Out-degree\n (VisNet)')+
  ylab('Centralitity\n (ProxNet)')+
  facet_wrap(~type)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position=c(0.05,0.05), legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pOutEC

pInEC <- ggplot(mergedPlayerNetworkDF, aes(x = inWeight, y = ECdist, color = env, fill = env))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm', alpha = 0.2)+
  xlab('In-degree\n (VisNet)')+
  ylab('Centralitity\n (ProxNet)')+
  facet_wrap(~type)+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position=c(0.05,0.05), legend.justification=c(0,0), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
pInEC

####################################################################################################
#What predicts more successful groups?
####################################################################################################
# groupDF <- mergedNetworkDF %>% group_by(session, env, type) %>% dplyr::summarize(score = mean(score), 
#                                                                                  ECvis = mean(ECvis), inDeg = mean(inWeight), outDeg = mean(outWeight),
#                                                                                  specialization = mean(abs(inWeight - outWeight)),
#                                                                                  ECdist = mean(ECdist), distWeight = mean(degree))
# 
# groupDF <- groupDF %>% group_by(session, env) %>% mutate(relScore = score[type=='group']- score[type=='solo']) 
# 
# 
# mFreq <- lm(relScore ~ECvis  + inDeg + specialization+ ECdist + distWeight , subset(groupDF, type == 'group' & env == 'smooth'))
# plot_model(mFreq)
####################################################################################################
#Put together a composite plot
####################################################################################################
#Visibility network as bottom row in maintext fiture
p <- cowplot::plot_grid(NULL, pDistEig,pInVOut+theme(legend.position='none'),  ncol = 3, rel_widths = c(.45, 1,1), labels = c('d', 'e','f'))
p

ggsave('plots/visPlots.pdf', p, width = 12, height = 3, units = 'in')



pCoeff_outIn #REgression coefficients of out-degree

#SI Plot focusing on visibility
SItop <- cowplot::plot_grid( pVisBlocksDestroyed+ theme(legend.position =c(1,0), legend.justification = c(1,0), legend.background=element_blank(),legend.key=element_blank()), 
                             pVisReward+theme(legend.position ='none'), 
                             prankInOut + theme(legend.position ='none', strip.background=element_blank(),legend.background=element_blank(),legend.key=element_blank()), 
                             nrow = 1,  labels = c('a','c','e'))
SIbottom <- cowplot::plot_grid(PVisBlocksDestroyedCoeff+ylab('Estimates') , PVisRewardCoeff, pCoeff_outIn, nrow = 1,labels = c('b','d','f'))

SIcombined <- cowplot::plot_grid(SItop,SIbottom, nrow = 2)

ggsave('plots/visSI.pdf', SIcombined, width = 12, height = 6, units = 'in')


#SI Plot focusing on distance
pProxSI <- cowplot::plot_grid(pSpatialCoeff, pspatialDistanceRank, labels = 'auto')
pProxSI
ggsave('plots/vproxSI.pdf', pProxSI, width = 10, height = 3, units = 'in')

#SI plot combining both networks
pBothNets <- cowplot::plot_grid(pECbothNets, pInDist + theme(legend.position='none'), pOutDist + theme(legend.position='none'), 
                                pOutEC + theme(legend.position='none'), pInEC + theme(legend.position='none'), NULL, labels = 'auto', nrow = 2)
pBothNets
ggsave('plots/combinedNetworks.pdf', pBothNets, width = 12, height = 6, units = 'in')


####################################################################################################
#GRAVEYARD
####################################################################################################


# 
# 
# #Test for relationship
# distReg <- distPlayerDF #clone
# distReg$id <- paste0(distReg$session, distReg$player)
# distReg$env <- factor(distReg$env, levels = c('random', 'smooth'))
# distReg$score <- scale(distReg$score)
# #distReg$ECsq <- scale(distReg$EC^2)
# distReg$EC <- scale(distReg$EC)
# mEC <- run_model(brm(score~env*EC+ (1+env+EC|session), data =distReg ,
#                      cores=4,  iter = 4000, warmup = 1000), modelName = 'EC')
# 
# 
# summary(mEC)
# 
# pSpatialCoeff <- plot_model(mEC, axis.labels =c('smooth:centrality', 'centrality',  'smooth'),  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
#   theme_classic()+
#   xlab('')+
#   ylab('Estimates')+
#   ggtitle('Avg. Rewards')+
#   ylim(c(-.4, 1.5))
# pSpatialCoeff
# 
# 
# #marginal_effects(mEC)
# #tab_model(mEC)
# post <- posterior_samples(mEC)
# formatHDI(unlist(post$b_EC),1)
# formatHDI(unlist(post$b_EC) +unlist(post$`b_envsmooth:EC`) ,2)
# 
# xseq <- seq(min(distReg$EC), max(distReg$EC), length.out = 100)
# newdat <- expand.grid(env = c("random","smooth"), EC = xseq)
# preds <- fitted(mEC, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
# 
# #create new fixed effects dataframe
# fixedDF <-  cbind(newdat, preds)
# names(fixedDF) <- c('env', 'EC',  'score', 'est.error', 'lower', 'upper')
# #unnormalizE variables
# fixedDF$score <- (fixedDF$score * sd(distPlayerDF$score)) + mean(distPlayerDF$score)
# fixedDF$lower <- (fixedDF$lower * sd(distPlayerDF$score)) + mean(distPlayerDF$score)
# fixedDF$upper <- (fixedDF$upper * sd(distPlayerDF$score)) + mean(distPlayerDF$score)
# fixedDF$EC <- (fixedDF$EC * sd(distPlayerDF$EC)) + mean(distPlayerDF$EC)
# 
# 
# #fixedDF <- fixedDF %>% dplyr::filter( !(env=='smooth' & (meanVisibility > max(subset(groupD, env == 'smooth')$meanVisibility) | meanVisibility < min(subset(groupD, env == 'smooth')$meanVisibility))))
# 
# pCentralityScore <- ggplot(distPlayerDF, aes(x = EC, y = score, color = env, fill = env))+
#   geom_point(alpha = 0.7) +
#   geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper),  color = 'NA', alpha = 0.4)+
#   geom_line(data=fixedDF)+
#   #geom_smooth(alpha = 0.2)+
#   theme_classic()+
#   xlab('Centrality (Proximity Network)')+
#   ylab('Avg. Reward')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position=c(0,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal")
# pCentralityScore
# 
# #Gamesmoothing
# # ggplot(distPlayerDF, aes(x = EC, y = score, color = env, fill = env))+
# #   geom_point(alpha = 0.7) +
# #   geom_smooth(alpha = 0.2)+
# #   theme_classic()+
# #   xlab('Eigen Centrality')+
# #   ylab('Avg. Reward')+
# #   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
# #   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
# #   theme(legend.position=c(1,1), legend.justification = c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal")
# 
# 
# #Compute rank centrality for each player for each player
# distancerankDF <- distanceNetworkDF %>% group_by(player, session, env)  %>% dplyr::summarize(EC = mean(EC), score = mean(score))
# for (s in unique(distanceNetworkDF$session)){
#   for (en in c('random', 'smooth')){
#     subd <- subset(distanceNetworkDF, session == s & env == en )  %>% group_by(player, session, env)  %>% dplyr::summarize(EC = mean(EC), score = mean(score))
#     distancerankDF[(distancerankDF$session == s & distancerankDF$env == en), 'ECrank'] <- rank(subd$EC, ties.method = 'min')
#     distancerankDF[(distancerankDF$session == s & distancerankDF$env == en), 'scoreRank'] <- rank(subd$score, ties.method = 'min')
#   }
# }
# distancerankDF$id <-  paste0(distancerankDF$player, distancerankDF$session) 
# distancerankDF$env <- factor(distancerankDF$env, levels = c('random', 'smooth'))
# 
# corTestPretty(subset(distancerankDF, env=='random')$EC, subset(distancerankDF, env=='random')$score, method='kendall')
# corTestPretty(subset(distancerankDF, env=='smooth')$EC, subset(distancerankDF, env=='smooth')$score, method='kendall')
# ttestPretty(subset(distancerankDF, env=='smooth' & ECrank == 2)$score, subset(distancerankDF, env=='smooth' & ECrank == 1)$score) #comparing less central
# ttestPretty(subset(distancerankDF, env=='smooth' & ECrank == 2)$score, subset(distancerankDF, env=='smooth' & ECrank >2)$score) #comparing more central
# pspatialDistanceRankScore <- ggplot(distancerankDF, aes(x = ECrank, y = score, color = env))+
#   stat_summary(fun=mean, geom='point')+
#   stat_summary(fun=mean, geom='line')+
#   stat_summary(fun.data = mean_se, geom='errorbar', width = 0.2)+
#   theme_classic()+
#   coord_cartesian(ylim=c(7.5, 16))+
#   xlab('Eigen Centrality (Rank)')+
#   ylab('Avg. Reward')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position=c(1,1.05), legend.justification = c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pspatialDistanceRankScore
# 
# pspatialDistanceRank <- ggplot(distancerankDF, aes(x = ECrank, y = scoreRank, color = env))+
#   stat_summary(fun=mean, geom='point')+
#   stat_summary(fun=mean, geom='line')+
#   stat_summary(fun.data = mean_se, geom='errorbar', width = 0.2)+
#   theme_classic()+
#   xlab('Centrality (Rank)')+
#   ylab('Score (Rank)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position=c(1,1), legend.justification = c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), legend.direction = "horizontal")
# pspatialDistanceRank
###################################################################################################
#Do groups with the right balance of social learning performed better? 
####################################################################################################
# 
# groupReg <- visNetworkDF  %>% group_by(session, env, type) %>% 
#   dplyr::summarize(meanReward = mean(score), giniReward = Gini(score), varReward = var(score),
#                    meanIn = mean(inWeight), giniIn = Gini(inWeight), varIn = var(inWeight),
#                    meanOut = mean(outWeight), giniOut= Gini(outWeight), varOut = var(outWeight), EC = mean(EC))
# 
# groupReg %>% ungroup() %>% select(-session) %>% ggpairs() #loook at all pairs
# 
# 
# #Prep data for regression
# groupReg$env <- factor(groupReg$env, levels = c('random', 'smooth'))
# # groupReg$meanReward <- scale(groupReg$meanReward)
# groupReg$giniIn <- scale(groupReg$giniIn)
# groupReg$giniOut <- scale(groupReg$giniOut)
# 
# 
# #Does inequality of social attention influence rewards?
# mRewardGini <- run_model(brm(meanReward~env*type*giniIn + (1+env+type+giniIn), data =groupReg,
#                              cores=4,  iter = 4000, warmup = 1000), modelName = 'rewardGini')
# 
# summary(mRewardGini)
# plot_model(mRewardGini)
# 
# PRewardGini <- plot_model(mRewardGini,  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
#   theme_classic()+
#   xlab('')+
#   ylab('Estimates')+
#   ggtitle('Rewards')
# ylim(c(-.8,.7))
# PRewardGini
# 
# 
# #Posterior predictions
# newdat <- expand.grid(env = c('random', 'smooth'), giniIn = seq(min(groupReg$giniIn),max(groupReg$giniIn), length.out = 100))
# preds <- fitted(mRewardGini, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
# #create new fixed effects dataframe
# fixedDF <- newdat
# fixedDF$meanReward <- preds[,1]                   
# fixedDF$lower <- preds[,3]  
# fixedDF$upper <- preds[,4]  
# 
# 
# pInequality <- ggplot(groupReg, aes(x = giniIn, y = meanReward, color = env, fill = env))+
#   geom_point()+
#   geom_ribbon(data = fixedDF, aes(ymin = lower, ymax = upper), color = NA, alpha = 0.4)+
#   geom_line(data = fixedDF)+
#   facet_grid(~env, scales = 'free')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('Gini Inequality of Social Attention')+
#   ylab('Mean Reward')+
#   theme(legend.position='none',  strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pInequality
# 

# 
# ###################################################################################################
# #Rank-based analyses
# ####################################################################################################
# 
# #Alternative Rank-based analyses
# 
# #add in/out degre rank order for each player
# for (s in unique(visNetworkDF$session)){
#   for (en in c('random', 'smooth')){
#     subd <- subset(VisPlayerRoundDF, session == s & env == en)
#     VisPlayerRoundDF[(VisPlayerRoundDF$session == s & VisPlayerRoundDF$env == en ), 'inRank'] <-  rank(subd$inWeight, ties.method = 'min')
#     VisPlayerRoundDF[(VisPlayerRoundDF$session == s & VisPlayerRoundDF$env == en ), 'outRank'] <- rank(subd$outWeight, ties.method = 'min')
#     VisPlayerRoundDF[(VisPlayerRoundDF$session == s & VisPlayerRoundDF$env == en ), 'scoreRank'] <- rank(subd$score, ties.method = 'min')  
#   }
# }
# VisPlayerRoundDF$id <-  paste0(VisPlayerRoundDF$player, VisPlayerRoundDF$session) 
# 
# #SCatter plots
# prankInOut<- ggplot(VisPlayerRoundDF, aes(x = inRank, y = outRank, color = env))+
#   stat_summary(fun.y = mean, geom = 'line')+
#   stat_summary(fun.y = mean, geom = 'point')+
#   stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.2)+
#   #facet_grid(~round_base)+ #not enough data yet
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('In Rank')+
#   ylab('Out Rank')+
#   theme(legend.position =c(0.1,0.1), legend.justification =c(0,0))
# prankInOut
# 
# prankInScore <- ggplot(VisPlayerRoundDF, aes(x = inRank, y = score, color = env))+
#   stat_summary(fun.y = mean, geom = 'line')+
#   stat_summary(fun.y = mean, geom = 'point')+
#   stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.2)+
#   #facet_grid(~round_base)+ #not enough data yet
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('In Rank')+
#   ylab('Avg. Reward')+
#   theme(legend.position ='none')
# prankInScore
# 
# prankOutScore <- ggplot(VisPlayerRoundDF, aes(x = outRank, y = score, color = env))+
#   stat_summary(fun.y = mean, geom = 'line')+
#   stat_summary(fun.y = mean, geom = 'point')+
#   stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.2)+
#   #facet_grid(~round_base)+ #not enough data yet
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   xlab('Out Rank')+
#   ylab('Avg. Reward')+
#   theme(legend.position ='none')
# prankOutScore
# 
# #is there a relationship between being central (being at the center of the action) and score?
# # corTestPretty(visPlayerDF$EC, visPlayerDF$score, method='kendall')
# # corTestPretty(subset(visPlayerDF, env == 'random')$EC, subset(visPlayerDF, env == 'random')$score, method='kendall') #random only
# # corTestPretty(subset(visPlayerDF, env == 'smooth')$EC, subset(visPlayerDF, env == 'smooth')$score, method='kendall') #smooth only
# # ranktestPretty(subset(visPlayerDF, env == 'random')$EC,subset(visPlayerDF, env == 'smooth')$EC)
# 
# #Eigen centrality
# pEigVis <- ggplot(visPlayerDF, aes(x = EC, y = score, color = env, fill = env))+
#   geom_point(alpha = 0.7) +
#   geom_smooth(method = 'lm', alpha = 0.2)+
#   xlab('Eigen Centrality\n (Average Visibility Network)')+
#   ylab('Score')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position=c(0.1,1), legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pEigVis
# 
# 

####################################################################################################
#Old plots
####################################################################################################

#Is visibility influenced by distnace?
#Compute the average visibility of a specific player
# directedVis <- pvisDF %>% group_by(session, name, type, env, target, round) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
# #Load Pairwise distances
# distanceDF <- readRDS('trajectories/pairwiseDistances.Rds') #Computed in pullAnalysis.R
# avgDistanceDF <- distanceDF %>% group_by(session, round, p1, p2) %>% dplyr::summarize(avgDistance = mean(distance))
# names(avgDistanceDF) <- c('session', 'round', 'name', 'target', 'avgDistance')
# distvisibility <- left_join(directedVis, avgDistanceDF, by = c("name", "target", "round","session" )) #Join
# 
# pDistVisibility <- ggplot(distvisibility, aes(x = avgDistance, y = avgVisible, color = env, fill = env))+
#   geom_point(alpha = 0.2)+
#   geom_smooth(method='lm', alpha = 0.2)+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   labs(y = 'Avg. Visibility', x= 'Avg. Distance')+
#   theme_classic()+
#   theme(legend.position=c(1,1), legend.justification = c(1,1), strip.background=element_blank(),legend.background=element_blank(),legend.key=element_blank())
# pDistVisibility
# 
# #Hypothesis: Individual search ability predicts social information use
# groupMeans <- blockDF %>% filter(type=='group') %>% group_by(session,type,env) %>% dplyr::summarize(reward = sum(reward)/(4*4)) #reward divded 4 rounds of each type, divded by 4 players in each round
# socialD$groupMean <-  rep(groupMeans$reward, 4) #TODO: Find more robust way to do this
# socialD$overUnder <- socialD$reward - socialD$groupMean #TODO: standarize by dividing by stdev of socialD$rewardfor each env type
# 
# 
# p4<- ggplot(subset(socialD, type == 'group'), aes(x = overUnder, y = avgVisible, color = env, fill = env))+
#   geom_point()+
#   geom_smooth(alpha = 0.2)+
#   scale_fill_manual(values =cbPalette, name="")+
#   scale_color_manual(values =cbPalette,  name="")+
#   labs(x = 'Relative Performance', y = 'Social Info Use')+
#   theme_classic()+ 
#   theme(legend.position=c(1,1), legend.justification = c(1,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# p4
# 
# 


# #Hypothesis: People who are attend more to others are more productive
# corTestPretty(subset(socialD, type == 'group' & env == 'smooth')$reward, subset(socialD, type == 'group' & env == 'smooth')$avgVisible, method = 'kendall')
# corTestPretty(subset(socialD, type == 'group' & env == 'random')$reward, subset(socialD, type == 'group' & env == 'random')$avgVisible,  method = 'kendall')
# 
# 
# pVisReward <- ggplot(subset(socialD, type == 'group'), aes(x = avgVisible, y = reward, color = env, fill = env))+
#   #geom_line(aes(group=id), alpha = 0.2, color = 'grey')+
#   geom_jitter(alpha = 0.8)+
#   geom_smooth( alpha = 0.2, method = 'lm', fullrange=TRUE)+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   labs(y = 'Avg Reward', x = 'Avg. Visible Peers')+
#   theme(legend.position=c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pVisReward #This is answered better below using the network analysis including both in and out degree
# 
# 
# 
# #Hypothesis: People who are watched the most are also the most productive
# watchedDF <-pvisDF %>% group_by(session, target, type, env) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
# watchedDF$reward <- rewards
# watchedDF$id <- paste0(watchedDF$session,watchedDF$target)
# 
# pWatchedReward <- ggplot(subset(watchedDF, type == 'group'), aes(x = avgVisible, y = reward, color = env, fill = env))+
#   geom_line(aes(group=id), alpha = 0.2, color = 'grey')+
#   geom_jitter(alpha = 0.8)+
#   geom_smooth(alpha = 0.2 )+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   labs(y = 'Avg Reward', x = 'Avg. Watchers')+
#   theme(legend.position=c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pWatchedReward



###################################################################################################
#How do individuals adapt their social attention over rounds?
####################################################################################################
# #Within group variability in social attention over rounds. How much do individuals differ in social attention, and how does that change over rounds?
# pvisDF$id <- paste0(pvisDF$name, pvisDF$session)
# groupVisDF <- pvisDF %>% group_by(session, id, type, env, round) %>% dplyr::summarize(meanVis = mean(unityVis, na.rm=T)) %>% group_by(session, type, env, round) %>% dplyr::summarize(varVisGroup = var(meanVis))
# groupVisDF$round_Base  <- rep(1:4,nrow(groupVisDF)/4 ) #base1-4 for rounds
# 
# pQithinGroupVisVar <- ggplot(groupVisDF, aes(x = round_Base, y = varVisGroup, color = env, fill = env))+
#   stat_summary(fun.y=mean, geom='point')+
#   stat_summary(fun.data=mean_se, geom='errorbar', width = 0.1)+
#   geom_smooth(method='lm', alpha = 0.2)+
#   xlab('Round')+
#   ylab('Var(Social Attention)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position = c(0.1,1), legend.justification= c(0,1))
# pQithinGroupVisVar
# 
# #within individual variability in social attention over rounds. How much does an individual change in socia attention over rounds? And how is that related to individual performance?
# indVisDF <- pvisDF %>% group_by(session, id, type, env, round) %>% dplyr::summarize(meanVis = mean(unityVis, na.rm=T)) %>% group_by(session, id, type, env) %>% dplyr::summarize(varVisibleInd = var(meanVis))
# indVisDF$reward <-  blockDF  %>% dplyr::filter(type == 'group') %>% group_by(session, id, type, env) %>% dplyr:: summarize(reward = sum(reward)) %>% pull(reward)
# 
# 
# #How much round to round variability is there in social attention (individual level)
# pSocialAttentionInd <- ggplot(indVisDF, aes(x = varVisibleInd, y = reward/4, color = env, fill = env))+
#   geom_point()+
#   geom_smooth(method = 'lm', alpha = 0.2 )+
#   ylab('Avg Rewards')+
#   xlab('Var(Social Attention)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position = c(1,1), legend.justification= c(1,1))
# pSocialAttentionInd
# 
# 
# #Change in rewards over rounds (for comparison)
# groupVisDF$rewards <-  blockDF %>% dplyr::filter(type == 'group') %>% group_by(session, type, env, round) %>% dplyr:: summarize(reward = sum(reward)) %>% pull(reward)
# fullRewardsDF <- blockDF  %>% group_by(session, type, env, round) %>% dplyr:: summarize(reward = sum(reward)) 
# fullRewardsDF$round_Base <-rep(1:4,nrow(fullRewardsDF)/4 ) #base1-4 for rounds
# 
# pRewardOVerRounds <- ggplot(fullRewardsDF, aes(x = round_Base, y = reward/4, color = env, fill = env))+
#   stat_summary(fun.y=mean, geom='point')+
#   stat_summary(fun.data=mean_se, geom='errorbar', width = 0.1)+
#   geom_smooth(method='lm', alpha = 0.2)+
#   facet_grid(~type)+
#   xlab('Round')+
#   ylab('Avg. Rewards')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position=c(0.05,1), legend.justification = c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())
# pRewardOVerRounds
# 
# #mean of visibility (aggregated across groups)
# groupVisDF <- pvisDF %>% group_by(session, type, env, round) %>% dplyr::summarize(meanVis = mean(unityVis, na.rm=T)) 
# groupVisDF$round_Base  <- rep(1:4,nrow(groupVisDF)/4 ) #base1-4 for rounds
# groupVisDF$rewards <-  blockDF %>% dplyr::filter(type == 'group') %>% group_by(session, type, env, round) %>% dplyr:: summarize(reward = sum(reward)) %>% pull(reward)
# 
# #meanVis is linear with varVis, since it's a binary variable
# ggplot(groupVisDF, aes(x = meanVis, y = rewards, color = env, fill = env))+
#   geom_point()+
#   geom_smooth(alpha = 0.2 )+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')
# 
# 
# #Groups variability (across rounds) in social attention (aggregated over individuals)
# groupVisDF <- pvisDF %>% group_by(session, type, env, round) %>% dplyr::summarize(meanVis = mean(unityVis, na.rm=T)) %>% #Mean visibility across group members for each round
#   group_by(session, env) %>% dplyr::summarize(groupVisVar = var(meanVis))
# groupVisDF$reward <-  blockDF %>% dplyr::filter(type == 'group') %>% group_by(session, env) %>% dplyr:: summarize(reward = sum(reward)) %>% pull(reward)
# 
# #Not enough data to really see a picture yet
# ggplot(groupVisDF, aes(x = groupVisVar, y = reward/4, color = env, fill = env))+
#   geom_point()+
#   #geom_smooth( alpha = 0.2 )+
#   ylab('Avg Rewards')+
#   xlab('Var(Social Attention)')+
#   scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
#   theme(legend.position = c(1,1), legend.justification= c(1,1))
# 
# 
# cowplot::plot_grid( pQithinGroupVisVar, pSocialAttentionInd, labels = 'auto', nrow = 1)
# 
# #TODO: Compute duration of visibility events. Look for patterns in longer avg. social attention duration