#Adaptivity and performance
#Charley Wu 2024
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
s_ars_conditional <- loadRData("Stanfits/s_conditional_ARS_weights") #group rounds
#names(s_ars_conditional)
s_soloARS <-  loadRData("Stanfits/s_solo_ARS_weights") #solo rounds

#Compute posterior mean for group rounds
W <- data.frame(apply(s_ars_conditional$v_ID, c(2,3), mean)) #group rounds first
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
W$type <- 'group'

#Compute posterior mean for group rounds
Wsolo <- data.frame(apply(s_soloARS$v_ID, c(2,3), mean)) 
names(Wsolo) <- c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality')
Wsolo$Locality <- W$Locality * -1 #Convert distance weights to proximity, for interpretability
Wsolo$`Adapt. Locality` <- Wsolo$`Adapt. Locality` * -1

Wsolo$id <- c('MPIB2session1.json', 'MPIB3session1.json', 'MPIB4session1.json', 'MPIB1session1.json', 'MPIB3session10.json', 'MPIB2session10.json', 'MPIB4session10.json', 'MPIB1session10.json', 'MPIB4session11.json', 'MPIB3session11.json', 'MPIB2session11.json', 'MPIB1session11.json', 'MPIB4session12.json', 'MPIB3session12.json', 'MPIB2session12.json', 'MPIB1session12.json', 'MPIB4session13.json', 'MPIB2session13.json', 'MPIB1session13.json', 'MPIB3session13.json', 'MPIB4session14.json', 'MPIB1session14.json', 'MPIB3session14.json', 'MPIB2session14.json', 'MPIB2session15.json', 'MPIB1session15.json', 'MPIB4session15.json', 'MPIB3session15.json', 'MPIB3session16.json', 'MPIB1session16.json', 'MPIB4session16.json', 'MPIB2session16.json', 'MPIB2session17.json', 'MPIB4session17.json', 'MPIB3session17.json', 'MPIB1session17.json', 'MPIB4session18.json', 'MPIB1session18.json', 'MPIB2session18.json', 'MPIB3session18.json', 'MPIB4session19.json', 'MPIB2session19.json', 'MPIB1session19.json', 'MPIB3session19.json', 'MPIB3session2.json', 'MPIB2session2.json', 'MPIB4session2.json', 'MPIB1session2.json', 'MPIB3session20.json', 'MPIB4session20.json', 'MPIB1session20.json', 'MPIB2session20.json', 'MPIB3session21.json', 'MPIB1session21.json', 'MPIB4session21.json', 'MPIB2session21.json', 'MPIB2session22.json', 'MPIB3session22.json', 'MPIB1session22.json', 'MPIB4session22.json', 'MPIB1session23.json', 'MPIB4session23.json', 'MPIB2session23.json', 'MPIB3session23.json', 'MPIB2session24.json', 'MPIB1session24.json', 'MPIB4session24.json', 'MPIB3session24.json', 'MPIB2session25.json', 'MPIB1session25.json', 'MPIB3session25.json', 'MPIB4session25.json', 'MPIB1session26.json', 'MPIB3session26.json', 'MPIB2session26.json', 'MPIB4session26.json', 'MPIB1session27.json', 'MPIB2session27.json', 'MPIB3session27.json', 'MPIB4session27.json', 'MPIB3session28.json', 'MPIB1session28.json', 'MPIB2session28.json', 'MPIB4session28.json', 'MPIB3session29.json', 'MPIB1session29.json', 'MPIB4session29.json', 'MPIB2session29.json', 'MPIB4session3.json', 'MPIB2session3.json', 'MPIB3session3.json', 'MPIB1session3.json', 'MPIB4session30.json', 'MPIB2session30.json', 'MPIB1session30.json', 'MPIB3session30.json', 'MPIB1session31.json', 'MPIB3session31.json', 'MPIB4session31.json', 'MPIB2session31.json', 'MPIB3session32.json', 'MPIB2session32.json', 'MPIB1session32.json', 'MPIB4session32.json', 'MPIB2session4.json', 'MPIB3session4.json', 'MPIB1session4.json', 'MPIB4session4.json', 'MPIB4session5.json', 'MPIB2session5.json', 'MPIB1session5.json', 'MPIB3session5.json', 'MPIB2session6.json', 'MPIB4session6.json', 'MPIB1session6.json', 'MPIB3session6.json', 'MPIB3session7.json', 'MPIB2session7.json', 'MPIB1session7.json', 'MPIB4session7.json', 'MPIB3session8.json', 'MPIB4session8.json', 'MPIB2session8.json', 'MPIB1session8.json', 'MPIB4session9.json', 'MPIB1session9.json', 'MPIB3session9.json', 'MPIB2session9.json')
Wsolo$type <- 'solo'

#combine together


postMeansDF <- read.csv('data/modelWeights/allPostMeans.csv')
#Duplicate for env; individual offsets are computed across envs, so they are the same for rough vs. smooth; here we also add the group mean as well for each env
#Group rounds first
Wrand <- W
Wsmooth<- W
Wrand$env <- 'random'
Wsmooth$env <- 'smooth'
#Posterior means for each env
randMeans <- filter(postMeansDF, model == 'ARS+Cond' & env=="random")  %>% 
  arrange(match(weights, c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful'))) %>%
  pull(est)
smoothMeans <- filter(postMeansDF, model == 'ARS+Cond' & env=="smooth")  %>% 
  arrange(match(weights, c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful'))) %>%
  pull(est)
Wrand[,1:8] <- Wrand[,1:8] + randMeans[1:8]
Wsmooth[,1:8] <- Wsmooth[,1:8] + smoothMeans[1:8]
W <-rbind(Wrand,Wsmooth)

Wrand <- Wsolo
Wsmooth<- Wsolo
Wrand$env <- 'random'
Wsmooth$env <- 'smooth'
#Posterior means for each env
randMeans <- filter(postMeansDF, model == 'SoloARS' & env=="random")  %>% 
  arrange(match(weights, c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality'))) %>%
  pull(est)
smoothMeans <- filter(postMeansDF, model == 'SoloARS' & env=="smooth")  %>% 
  arrange(match(weights, c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality'))) %>%
  pull(est)
Wrand[,1:4] <- Wrand[,1:4] + randMeans[1:4]
Wsmooth[,1:4] <- Wsmooth[,1:4] + smoothMeans[1:4]
Wsolo <- rbind(Wrand,Wsmooth)

W <- rbind.fill(W, Wsolo )




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
visDF <- playerDF  %>% pivot_longer(cols = MPIB4_visible:MPIB1_visible,  names_to = 'target', values_to = 'serverVis')
visDF$target <- factor(visDF$target)
levels(visDF$target) <- c("MPIB1","MPIB2", "MPIB3", "MPIB4")

#Visibility info
pvisDF <- readRDS('simData/pvisDF.Rds') 
evisDF <- readRDS('simData/evisDF.Rds')

#Join pvis with visDF
#Compute number of visible players
socialD <- pvisDF %>% group_by(session, name, type, env) %>% dplyr::summarize(avgVisible = mean(unityVis, na.rm=T) )
socialD$id <- paste0(socialD$name, socialD$session)
#Now add reward and num blocks destroyed from blockDF
blockInfo<- blockDF %>% group_by(session, name, type, env) %>% dplyr:: summarize(reward = sum(reward)/4, blocksDestroyed = n()/4) 
socialD$reward <- blockInfo$reward
socialD$blocksDestroyed <- blockInfo$blocksDestroyed

#Vis network
visNetworkDF <- readRDS('networks/visNet.Rds')
visNetworkDF$env <- factor(visNetworkDF$env, levels = c('random', 'smooth'))

#Distance network
distanceNetworkDF <- readRDS('networks/distNet.Rds')
#Calculate average per participant
distPlayerDF <- distanceNetworkDF %>% group_by(player, session, env, type) %>% dplyr::summarize(EC = mean(EC), score = mean(score)) 
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

for (envType in c('smooth', 'random') ){ #loop through env type
  for (gameType in c('group', 'solo')){
    leaderCounts <- eventDF %>% dplyr::filter(env == envType & type == 'pull' & roundtype == gameType) %>% select(session, leader) %>% table
    followerCounts <- eventDF %>% dplyr::filter(env == envType & type == 'pull' & roundtype == gameType) %>% select(session, follower) %>% table
    leaderShip <- leaderCounts - followerCounts #positive when leader more than follower
    leaderProb <- leaderCounts / rowSums(leaderCounts)
    followerProb <- followerCounts / rowSums(followerCounts)
    leaderIndex = leaderProb - followerProb
    leaderDF <- as.data.frame(leaderIndex) #convert to dataframe
    names(leaderDF) <- c('session', 'name', 'leadership')
    leaderDF$name <- factor(paste0('MPIB', leaderDF$name))
    #compute average score for each group
    groupScoreDF <- blockDF %>% dplyr::filter(env == envType & type  == gameType) %>% group_by(session) %>% dplyr::summarize(avgScore = sum(reward)/16)
    #Compute score for each player
    playerScoreDF <- blockDF %>% dplyr::filter(env == envType & type  == gameType) %>% group_by(session, name) %>% dplyr::summarize(avgScore = sum(reward)/4)
    playerScoreDF$groupScore <- rep(groupScoreDF$avgScore, each = 4)
    playerScoreDF$relScore <- playerScoreDF$avgScore - playerScoreDF$groupScore
    #Put together
    leaderDF <- merge(leaderDF, playerScoreDF, by = c('session', 'name'))
    leaderDF$env <- envType
    leaderDF$type <- gameType
    leaderShipDF <- rbind(leaderShipDF, leaderDF)
}}
leaderShipDF$id <- paste0(leaderShipDF$name, leaderShipDF$session)


setwd('../modeling')

####################################################################################################
# Combine data together
####################################################################################################

df <- merge(W, leaderShipDF, by = c('id','env', 'type')) #weights and leadership
df <- merge(df, socialD, by = c('id','env', 'type')) #weights and basic social/behavioral data


#Visibility network
visNetworkDF$id <- paste0(visNetworkDF$player,visNetworkDF$session)
visNetworkDF <- visNetworkDF %>% group_by(id, env, type) %>% dplyr::summarize(inWeight = mean(inWeight), outWeight = mean(outWeight))
df <- merge(df, visNetworkDF, by=c('id', 'env', 'type'))

#Proximity network
distanceNetworkDF$id <- paste0(distanceNetworkDF$player,distanceNetworkDF$session)
distanceNetworkDF <- distanceNetworkDF %>% group_by(id, env, type) %>% dplyr::summarize(Centrality = mean(EC))
df <- merge(df, distanceNetworkDF, by=c('id', 'env', 'type'))


df$env <- factor(df$env, levels = c('smooth', 'random'))
df$type<- factor(df$type, levels = c('solo', 'group'))

####################################################################################################
# Regression
####################################################################################################
#compile data for regressions

#Group rounds 
mDF <- df %>% dplyr::filter(type == 'group')  #copy over
names(mDF)[4:11] <- c('locality', 'blockVis', 'rewardPred', 'succProx', 'unsuccProx', 'adaptLoc', 'adaptSucc', 'adaptUnsucc' ) #rename to avoid errors with brms
mDF$env <- fct_rev(mDF$env) #make sure random is the baseline
#solo rounds
soloDF <- df %>% dplyr::filter(type == 'solo')  #copy over
names(soloDF)[c(4,5,6,9)] <- c('locality', 'blockVis', 'rewardPred',  'adaptLoc') #rename to avoid errors with brms
soloDF$env <- fct_rev(soloDF$env) #make sure random is the baseline


#names for coeff plots
weightNames <- c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful')
coefLabels <- rev(c(weightNames,'[Smooth]', paste0(weightNames, "\n[Smooth]"))) #reversed order
soloNames <- c('Locality', 'Block Visibility', 'Reward Prediction',  'Adapt. Locality')
soloCoefs <- rev(c(soloNames,'[Smooth]', paste0(soloNames, "\n[Smooth]"))) #reversed order


#Avg. Score
#test <- lmer(reward~ (locality + blockVis+ rewardPred+ succProx+ unsuccProx+ adaptLoc+ adaptSucc +  adaptUnsucc)*env + (1|id), data = mDF) #frequentist regression for sanity check
mPerf <- run_model(brm(reward~ (locality + blockVis+ rewardPred+ succProx+ unsuccProx+ adaptLoc+ adaptSucc +  adaptUnsucc)*env + (1|id), data = mDF,
                              cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'perfWeights')
#summary(mPerf)
soloPerf <- run_model(brm(reward~ (locality + blockVis+ rewardPred+  adaptLoc)*env + (1|id), data = soloDF,
                          cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'perfWeightsSolo')
#summary(soloPerf)

#Plot group results
ci <- posterior_interval(mPerf, prob = 0.95) # Extract posterior intervals (95% credible intervals)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
signifLabels <- coefLabels # Create a theme variable has an asterix or coefficients that do not overlap with 0
signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))] <- paste0('* ', signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))]) #skip intercept and non fixef variables; rev order 

pCoeff_Perf <- plot_model(mPerf,axis.labels = signifLabels,  bpe = "mean", bpe.style = "dot", 
                        bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-14,10))+
  ggtitle('Avg. Reward (Group)')
pCoeff_Perf

#Solo rounds
ci <- posterior_interval(soloPerf, prob = 0.95)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
solosignifLabels <- soloCoefs # Create a theme variable has an asterix or coefficients that do not overlap with 0
solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))] <- paste0('* ', solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))]) #skip intercept and non fixef variables; rev order 


pCoeff_Perf_solo <- plot_model(soloPerf,axis.labels = solosignifLabels,  bpe = "mean", bpe.style = "dot", 
                          bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  #ylim(c(-15,10))+
  ggtitle('Avg. Reward (Solo)')
pCoeff_Perf_solo



#Centrality
mCent<- run_model(brm(Centrality~ (locality + blockVis+ rewardPred+ succProx+ unsuccProx+ adaptLoc+ adaptSucc +  adaptUnsucc)*env + (1|id), data = mDF,
                       cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'centWeights')
#summary(mCent)
soloCent <- run_model(brm(Centrality~ (locality + blockVis+ rewardPred+  adaptLoc)*env + (1|id), data = soloDF,
                          cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'centWeightsSolo')


#Group rounds
ci <- posterior_interval(mCent, prob = 0.95) # Extract posterior intervals (95% credible intervals)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
signifLabels <- coefLabels # Create a theme variable has an asterix or coefficients that do not overlap with 0
signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))] <- paste0('* ', signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))]) #skip intercept and non fixef variables; rev order 

#Solo rounds
ci <- posterior_interval(soloCent, prob = 0.95)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
solosignifLabels <- soloCoefs # Create a theme variable has an asterix or coefficients that do not overlap with 0
solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))] <- paste0('* ', solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))]) #skip intercept and non fixef variables; rev order 


pCoeff_Cent <-plot_model(mCent,axis.labels = signifLabels,  bpe = "mean", bpe.style = "dot", 
                         bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-.37,.2))+
  ggtitle('Centrality (Group)')
pCoeff_Cent
#tab_model(mCent)

pCoeff_Cent_solo <-plot_model(soloCent,axis.labels = solosignifLabels,  bpe = "mean", bpe.style = "dot", 
                         bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-.07,.07))+
  ggtitle('Centrality (Solo)')
pCoeff_Cent_solo


#out-degree
mVis<- run_model(brm(outWeight~ (locality + blockVis+ rewardPred+ succProx+ unsuccProx+ adaptLoc+ adaptSucc +  adaptUnsucc)*env + (1|id), data = mDF,
                      cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'visWeights')
#summary(mVis)
soloVis<- run_model(brm(outWeight~ (locality + blockVis+ rewardPred + adaptLoc)*env + (1|id), data = soloDF,
                     cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'visWeightsSolo')
#summary(soloVis)

#group rounds
ci <- posterior_interval(mVis, prob = 0.95)# Extract posterior intervals (95% credible intervals)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
signifLabels <- coefLabels# Create a theme variable has an asterix or coefficients that do not overlap with 0
signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))] <- paste0('* ', signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))]) #skip intercept and non fixef variables; rev order 

#Solo rounds
ci <- posterior_interval(soloVis, prob = 0.95)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
solosignifLabels <- soloCoefs # Create a theme variable has an asterix or coefficients that do not overlap with 0
solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))] <- paste0('* ', solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))]) #skip intercept and non fixef variables; rev order 


pCoeff_Vis <- plot_model(mVis,axis.labels = signifLabels,  bpe = "mean", bpe.style = "dot", 
                         bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-.8,.9))+
  ggtitle('Out-degree (Group)')
pCoeff_Vis
#tab_model(mVis)

pCoeff_Vis_solo <- plot_model(soloVis,axis.labels = solosignifLabels,  bpe = "mean", bpe.style = "dot", 
                         bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-.2,.23))+
  ggtitle('Out-degree (Solo)')
pCoeff_Vis_solo


#in degree 
mVisIn<- run_model(brm(inWeight~ (locality + blockVis+ rewardPred+ succProx+ unsuccProx+ adaptLoc+ adaptSucc +  adaptUnsucc)*env + (1|id), data = mDF,
                     cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'visInWeights')
#summary(mVisIn)
soloVisIn<- run_model(brm(inWeight~ (locality + blockVis+ rewardPred+  adaptLoc)*env + (1|id), data = soloDF,
                       cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'visInWeightssolo')
#summary(soloVisIn)

#group rounds
ci <- posterior_interval(mVisIn, prob = 0.95) # Extract posterior intervals (95% credible intervals)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
signifLabels <- coefLabels # Create a theme variable has an asterix or coefficients that do not overlap with 0
signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))] <- paste0('* ', signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))]) #skip intercept and non fixef variables; rev order 
#Solo rounds
ci <- posterior_interval(soloVisIn, prob = 0.95)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
solosignifLabels <- soloCoefs # Create a theme variable has an asterix or coefficients that do not overlap with 0
solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))] <- paste0('* ', solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))]) #skip intercept and non fixef variables; rev order 


pCoeff_VisIn <- plot_model(mVisIn,axis.labels = signifLabels,  bpe = "mean", bpe.style = "dot", 
                         bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  scale_y_continuous(n.breaks = 2, limits = c(-.5,.7))+
  ggtitle('In-degree (Group)')
pCoeff_VisIn
#tab_model(mVis)


pCoeff_VisIn_solo <- plot_model(soloVisIn,axis.labels = solosignifLabels,  bpe = "mean", bpe.style = "dot", 
                              bpe.color='black', show.values = TRUE,value.offset = .4, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-.11,.11))+
  ggtitle('In-degree (Solo)')
pCoeff_VisIn_solo

#Leadership
mLeader<- run_model(brm(leadership~ (locality + blockVis+ rewardPred+ succProx+ unsuccProx+ adaptLoc+ adaptSucc +  adaptUnsucc)*env + (1|id), data = mDF,
                     cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'leadershipWeights')
#summary(mLeader)
soloLeader<- run_model(brm(leadership~ (locality + blockVis+ rewardPred+  adaptLoc)*env + (1|id), data = soloDF,
                        cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'leadershipWeightssolo')
#summary(soloLeader)

#group rounds
ci <- posterior_interval(mLeader, prob = 0.95) # Extract posterior intervals (95% credible intervals)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
signifLabels <- coefLabels # Create a theme variable has an asterix or coefficients that do not overlap with 0
signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))] <- paste0('* ', signifLabels[which(rev(ci_overlap_0[2:18]==TRUE))]) #skip intercept and non fixef variables; rev order 
#Solo rounds
ci <- posterior_interval(soloLeader, prob = 0.95)
ci_overlap_0 <- ci[, 1] * ci[, 2] > 0  # Check if credible intervals overlap with 0
solosignifLabels <- soloCoefs # Create a theme variable has an asterix or coefficients that do not overlap with 0
solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))] <- paste0('* ', solosignifLabels[which(rev(ci_overlap_0[2:10]==TRUE))]) #skip intercept and non fixef variables; rev order 


pCoeff_Leadership <- plot_model(mLeader,axis.labels = signifLabels,  bpe = "mean", bpe.style = "dot", 
                         bpe.color='black', show.values = TRUE, value.offset = .4,vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-3.8,3.8))+
  ggtitle('Leadership')

pCoeff_Leadership


pCoeff_Leadership_solo <- plot_model(soloLeader,axis.labels = solosignifLabels,  bpe = "mean", bpe.style = "dot", 
                                bpe.color='black', show.values = TRUE, value.offset = .4,vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=TRUE) +
  theme_classic()+
  xlab('')+
  ylim(c(-.6,.6))+
  ggtitle('Leadership')

pCoeff_Leadership_solo



pCoeff <- cowplot::plot_grid(pCoeff_Perf, pCoeff_Cent, pCoeff_Vis, pCoeff_VisIn,pCoeff_Perf_solo, pCoeff_Cent_solo, pCoeff_Vis_solo, pCoeff_VisIn_solo, labels = 'auto', 
                             nrow = 2, rel_heights = c(1,.6))
pCoeff

ggsave('plots/WeightsBehav.pdf', pCoeff, width = 14, height = 8, units = 'in')
