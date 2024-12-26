
#Script to process data and run computational models using stan
#Written by Dominik Deffner


#For each of N block choices, we construct different feature matrices and save them in a format required by Stan

#Load relevant packages and convenience functions for extracting individual id and group id from strings
library(arrow)
library(stringr)
library(abind)
library(rethinking)

get_id    <- function(x) as.numeric(str_match(x, "MPIB\\s*(.*?)\\s*session")[2])
get_group <- function(x) as.numeric(str_match(x, "session\\s*(.*?)\\s*.json")[2])

standardize <- function(x) {
  x <- scale(x)
  z <- as.numeric(x)
  attr(z,"scaled:center") <- attr(x,"scaled:center")
  attr(z,"scaled:scale") <- attr(x,"scaled:scale")
  return(z)
}

#Set working directory
setwd("~/modeling/data/modelFeatures")

#Get all file names
files <- list.files(pattern = "")

#Choose "solo" or "group
Condition <- "group"

#Empty vectors to store player id, group id, round, decision number and environment (smooth vs. random)
id_in_group <- c()
id_original <- c()
group <- c()
decision_number <- c()
environment <- c()
round <- c()

#Number of available blocks for each choice
B <- c()

#Which block was chosen (as index of all available blocks)?
choices <- c()


# Overall Feature matrices
fruit_distance <- matrix(NA, nrow = 0,ncol = 400)
fruit_pov <- matrix(NA, nrow = 0,ncol = 400)

asocial_GP_pred <- matrix(NA, nrow = 0,ncol = 400)

if (Condition == "group"){
  successful_player_distance <- matrix(NA, nrow = 0,ncol = 400)
  visible_player_distance <- matrix(NA, nrow = 0,ncol = 400)
  all_visible_player_distance <- matrix(NA, nrow = 0,ncol = 400)
  
  player1_distance <- matrix(NA, nrow = 0,ncol = 400)
  player2_distance <- matrix(NA, nrow = 0,ncol = 400)
  player3_distance <- matrix(NA, nrow = 0,ncol = 400)
  player4_distance <- matrix(NA, nrow = 0,ncol = 400)
}

reward_times <- readRDS("~/GitLab/producerScrounger/cogmodeling/data/rewardTimes.RDS")

#Loop over all files and compute relevant statistics before adding to output objects
for (file in files) {
  
  #Load feather file  
  d <- arrow::read_feather(file)
  
  #Subset based on condition
  d <- d[d$type == Condition, ]
  
  #Index starting from 1
  d$decision_number <- d$decision_number+1
  d$index <- d$index + 1
  
  #Get number of choices
  N <- length(which(d$chosen == 1)) 
  
  #Get IDs for each choice
  d$choice_id <- cumsum(!duplicated(d[c("round","decision_number")]))
  
  #Create temporary objects to add to output later
  
  #Block choices
  choices_temp <- c()
  
  #Number of available blocks
  B_temp <- c()
  
  #Environment
  environment_temp <- c()
  
  #Number of decision in round
  decision_number_temp <- c()
  
  #Round
  round_temp <- c()
  
  # Feature matrices
  fruit_distance_temp <- matrix(NA, nrow = N,ncol = 400)
  fruit_pov_temp <- matrix(NA, nrow = N,ncol = 400)
  
  
  asocial_GP_pred_temp <- matrix(NA, nrow = N,ncol = 400)
  
  if (Condition == "group"){
    successful_player_distance_temp <- matrix(NA, nrow = N,ncol = 400)
    visible_player_distance_temp <- matrix(NA, nrow = N,ncol = 400)
    
    all_visible_player_distance_temp <- matrix(NA, nrow = N,ncol = 400)
    
    player1_distance_temp <- matrix(NA, nrow = N,ncol = 400)
    player2_distance_temp <- matrix(NA, nrow = N,ncol = 400)
    player3_distance_temp <- matrix(NA, nrow = N,ncol = 400)
    player4_distance_temp <- matrix(NA, nrow = N,ncol = 400)
  }
  
  #Loop over all choices to construct necessary information
  for (i in 1:N) {
    
    #Indices of available blocks for choice i
    available <- which(is.na(d$fruit_distance[d$choice_id == i])==FALSE)
    
    #Number of available blocks for choice i
    B_temp[i] <- length(available)
    
    #Construct feature matrices and add Inf in the end, so each has same length  
    fruit_distance_temp[i,]             <- c(d$fruit_distance[d$choice_id == i][available],              rep(Inf, 400 - B_temp[i] ))
    fruit_pov_temp[i,]                  <- c(d$fruit_pov[d$choice_id == i][available],                   rep(Inf, 400 - B_temp[i] ))
    
    asocial_GP_pred_temp[i,]            <- c(d$asocial_GP_pred[d$choice_id == i][available],             rep(Inf, 400 - B_temp[i] ))
    
    if (Condition == "group"){
      successful_player_distance_temp[i,] <- c(d$successful_player_distance[d$choice_id == i][available],  rep(Inf, 400 - B_temp[i] ))
      visible_player_distance_temp[i,]    <- c(d$visible_player_distance[d$choice_id == i][available],     rep(Inf, 400 - B_temp[i] ))
      all_visible_player_distance_temp[i,]<- c(d$all_visible_player_distance[d$choice_id == i][available], rep(Inf, 400 - B_temp[i] ))
      
      player1_distance_temp[i,]<- c(d$player1_dist[d$choice_id == i][available],       rep(Inf, 400 - B_temp[i] ))
      player2_distance_temp[i,]<- c(d$player2_dist[d$choice_id == i][available],       rep(Inf, 400 - B_temp[i] ))
      player3_distance_temp[i,]<- c(d$player3_dist[d$choice_id == i][available],       rep(Inf, 400 - B_temp[i] ))
      player4_distance_temp[i,]<- c(d$player4_dist[d$choice_id == i][available],       rep(Inf, 400 - B_temp[i] ))
    }
    
    #Which of the available blocks was chosen for decision i?
    choices_temp[i] <- which(available == which(d$chosen[d$choice_id == i]==1))
    
    environment_temp[i] <- unique(d$env[d$choice_id == i])
    round_temp[i] <- unique(d$round[d$choice_id == i])
    decision_number_temp[i] <- unique(d$decision_number[d$choice_id == i])
    
  }#i
  
  
  #Add to output objects
  B <- c(B, B_temp)
  choices <- c(choices, choices_temp)
  environment <- c(environment, environment_temp)
  round <- c(round, round_temp)
  decision_number <- c(decision_number, decision_number_temp)
  
  
  id_in_group <- c(id_in_group,  rep(  get_id(unique(d$id)), N)  )      
  group <- c(group,rep(get_group(unique(d$id)), N)) 
  
  fruit_distance  <- rbind(fruit_distance, fruit_distance_temp)
  fruit_pov       <- rbind(fruit_pov, fruit_pov_temp)
  
  asocial_GP_pred <- rbind(asocial_GP_pred, asocial_GP_pred_temp)
  
  if (Condition == "group"){
    successful_player_distance <- rbind(successful_player_distance, successful_player_distance_temp)
    visible_player_distance    <- rbind(visible_player_distance, visible_player_distance_temp)
    
    all_visible_player_distance <- rbind(all_visible_player_distance, all_visible_player_distance_temp)
    
    player1_distance <- rbind(player1_distance, player1_distance_temp)
    player2_distance <- rbind(player2_distance, player2_distance_temp)
    player3_distance <- rbind(player3_distance, player3_distance_temp)
    player4_distance <- rbind(player4_distance, player4_distance_temp)
    
    id_original <- c(id_original, unique(d$id))
    
  }
  
  print(which(files==file))
}



#Construct unique numeric ids
N <- length(id_in_group)
id <- c()
id[1] <- 1

counter <- 1
for (i in 2:N) {
  if ((id_in_group[i] != id_in_group[i-1]) | (group[i] != group[i-1])) counter <- counter + 1
  id[i] <- counter
}




#Create list for stan
#Below, we add feature arrays, depending on which model we want to run

dat <- list(N = N, 
            N_block = 400,
            B = B,
            N_id = length(unique(id)), 
            N_groups = length(unique(group)),
            id = id,
            id_in_group = id_in_group,
            group = group,
            round = round,
            env = ifelse(environment == "random", 1, 2), 
            decision_number = decision_number,
            choice = choices
)

#Include time since last (individual or observed) reward
#Select only solo or group rounds
reward_times <- reward_times[reward_times$type == Condition, ]

reward_times$id_in_group <- c()

for (i in 1:nrow(reward_times)) {
  reward_times$id_in_group[i] <- get_id(reward_times$id[i])
  reward_times$group[i] <- get_group(reward_times$id[i])
}


dat$last_ind_reward <- c()
dat$last_soc_reward <- c()
for (group in unique(dat$group)) {
  print(group)
  for (id in unique(dat$id_in_group)) {
    rounds <- unique(dat$round[dat$group == group & dat$id_in_group == id])
    for (r in rounds) {
      dat$last_ind_reward[which(dat$group == group & dat$id_in_group == id & dat$round == r)] <- reward_times$lastIndReward[which(reward_times$group == group & reward_times$id_in_group == id & reward_times$round == r)]
      dat$last_soc_reward[which(dat$group == group & dat$id_in_group == id & dat$round == r)] <- reward_times$lastSocReward[which(reward_times$group == group & reward_times$id_in_group == id & reward_times$round == r)]
    }
  }
}  

dat$last_ind_reward <- standardize(dat$last_ind_reward)
dat$last_soc_reward <- standardize(dat$last_soc_reward)

###
##
# Asocial model
##
### 

dat$N_feat = 3
dat$Feature_matrix = array(NA, dim=c(dat$N, dat$N_block, dat$N_feat))

dat$Feature_matrix[,,1] = fruit_distance
dat$Feature_matrix[,,2] = fruit_pov
dat$Feature_matrix[,,3] = asocial_GP_pred

#standardize predictors within rounds and set NA to -Inf
for (i in c(1,3)) {
  for (j in 1:dat$N) {
    if(dat$decision_number[j]>1){
      dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
    } else{
      dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
    }
  }
}

m_parallel <- cmdstan_model("Minecraft_parallel_solo_asocial.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_asocial <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_asocial$output_files())
s_group_asocial <- extract.samples(stanfit)

##
# Adaptive area-restricted search model
##
m_parallel <- cmdstan_model("Minecraft_parallel_asocial_ARS.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_asocial_ARS <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfitasocial_ARS <- rstan::read_stan_csv(fit_parallel_group_asocial_ARS$output_files())
s_asocial_ARS <- extract.samples(stanfitasocial_ARS)
save(s_asocial_ARS, file = "s_asocial_ARS_weights")


###
##
# Unbiased model
##
### 

dat$N_feat = 4
dat$Feature_matrix = array(NA, dim=c(dat$N, dat$N_block, dat$N_feat))

dat$Feature_matrix[,,1] = fruit_distance
dat$Feature_matrix[,,2] = fruit_pov
dat$Feature_matrix[,,3] = asocial_GP_pred
dat$Feature_matrix[,,4] = all_visible_player_distance

dat$all_visible_player_distance_NA <- sapply(1:nrow(dat$Feature_matrix[,,4]), function(i) ifelse(any(is.na(dat$Feature_matrix[i,,4])),1,0))

#standardize predictors within rounds and set NA to -Inf
for (i in 1:4) {
  for (j in 1:dat$N) {
    if(dat$decision_number[j]>1){
      
      #Asocial features
      if (i %in% c(1,3)){
        dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
      }
      
      #Successful player distance
      if (i == 4){
        if (dat$all_visible_player_distance_NA[j]==0){
          dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
        } else{
          dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
        }
      }
      
    } else{
      dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
    }
  }
}

m_parallel <- cmdstan_model("Minecraft_parallel_unbiased.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_unbiased <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_unbiased$output_files())
s_group_unbiased <- extract.samples(stanfit)


###
##
# Success-biased model
##
### 

dat$N_feat = 5
dat$Feature_matrix = array(NA, dim=c(dat$N, dat$N_block, dat$N_feat))

dat$Feature_matrix[,,1] = fruit_distance
dat$Feature_matrix[,,2] = fruit_pov
dat$Feature_matrix[,,3] = asocial_GP_pred
dat$Feature_matrix[,,4] = successful_player_distance
dat$Feature_matrix[,,5] = visible_player_distance


#Create variable for which features contain NAs
dat$successful_NA    <- sapply(1:nrow(dat$Feature_matrix[,,4]), function(i) ifelse(any(is.na(dat$Feature_matrix[i,,4])),1,0))
dat$unsuccessful_NA  <- sapply(1:nrow(dat$Feature_matrix[,,5]), function(i) ifelse(any(is.na(dat$Feature_matrix[i,,5])),1,0))

#standardize predictors within rounds and set NA to -Inf
for (i in 1:5) {
  for (j in 1:dat$N) {
    if(dat$decision_number[j]>1){
      
      #Asocial features
      if (i %in% c(1,3)){
        dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
      }
      
      #Successful player distance
      if (i == 4){
        if (dat$successful_NA[j]==0){
          dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
        } else{
          dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
        }
      }
      
      #Unuccessful player distance
      if (i == 5){
        if (dat$unsuccessful_NA[j]==0){
          dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
        } else{
          dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
        }
      }
    } else{
      dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
    }
  }
}

m_parallel <- cmdstan_model("Minecraft_parallel_success.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_success <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_success$output_files())
s_group_success <- extract.samples(stanfit)


##
# Adaptive extensions of success-biased model
##

#Conditional social learning model
m_parallel <- cmdstan_model("Minecraft_parallel_conditionalSL.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_conditional <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_conditional$output_files())
s_conditional <- extract.samples(stanfit)
save(s_conditional, file = "s_conditional_weights")

#Critical social learning model
m_parallel <- cmdstan_model("Minecraft_parallel_criticalSL.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_critical <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_critical$output_files())
s_critical <- extract.samples(stanfit)
save(s_critical, file = "s_critical_weights")

#Area restricted search model
m_parallel <- cmdstan_model("Minecraft_parallel_ARS.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_ARS <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfitARS <- rstan::read_stan_csv(fit_parallel_group_ARS$output_files())
s_ARS <- extract.samples(stanfitARS)
save(s_ARS, file = "s_ARS_weights")

#Conditional SL + Area restricted search model
m_parallel <- cmdstan_model("Minecraft_parallel_conditionalSL_ARS.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_ARS <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 60, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfitconditional_ARS <- rstan::read_stan_csv(fit_parallel_group_ARS$output_files())
s_conditional_ARS <- extract.samples(stanfitconditional_ARS)
save(s_conditional_ARS, file = "s_conditional_ARS")



###
##
# Player-specific model
##
### 

dat$N_feat = 7
dat$Feature_matrix = array(NA, dim=c(dat$N, dat$N_block, dat$N_feat))

dat$Feature_matrix[,,1] = fruit_distance
dat$Feature_matrix[,,2] = fruit_pov
dat$Feature_matrix[,,3] = asocial_GP_pred
dat$Feature_matrix[,,4] = player1_distance
dat$Feature_matrix[,,5] = player2_distance
dat$Feature_matrix[,,6] = player3_distance
dat$Feature_matrix[,,7] = player4_distance


dat$Possibilities <- cbind(rep(1,16),rep(1,16),rep(1,16),unname(as.matrix(expand.grid(Player1 = 0:1, Player2 = 0:1, Player3 = 0:1, Player4 = 0:1))) )

Visible <- matrix(NA, nrow = dat$N, ncol = 4)

Visible[,1] <- sapply(1:nrow(player1_distance), function(i) ifelse(any(is.na(player1_distance[i,])),0,1))
Visible[,2] <- sapply(1:nrow(player2_distance), function(i) ifelse(any(is.na(player2_distance[i,])),0,1))
Visible[,3] <- sapply(1:nrow(player3_distance), function(i) ifelse(any(is.na(player3_distance[i,])),0,1))
Visible[,4] <- sapply(1:nrow(player4_distance), function(i) ifelse(any(is.na(player4_distance[i,])),0,1))

dat$available <- c()
for (i in 1:dat$N) {
  dat$available <- c(dat$available , unname(which(colSums(t(dat$Possibilities) == c(1,1,1,Visible[i,])) == ncol(dat$Possibilities))) )
}

#standardize predictors within rounds and set NA to -Inf

for (i in 1:7) {
  for (j in 1:dat$N) {
    if(dat$decision_number[j]>1){
      
      #Asocial features
      if (i %in% c(1,3)){
        dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
      }
      
      #player distance
      if (i > 3){
        if (dat$Possibilities[dat$available[j], i] == 1){
          dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
        } else{
          dat$Feature_matrix[j,1:dat$B[j],i] <- 0
        }
      }
      
    } else{
      dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
    }
  }
}

m_parallel <- cmdstan_model("Minecraft_parallel_unique.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_unique <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_unique$output_files())
s_group_unique <- extract.samples(stanfit)


###
##
# Solo rounds (set "Condition" variable above to "solo")
##
### 

dat$N_feat = 3
dat$Feature_matrix = array(NA, dim=c(dat$N, dat$N_block, dat$N_feat))

dat$Feature_matrix[,,1] = fruit_distance
dat$Feature_matrix[,,2] = fruit_pov
dat$Feature_matrix[,,3] = asocial_GP_pred


#standardize predictors within rounds and set NA to -Inf
for (i in c(1,3)) {
  for (j in 1:dat$N) {
    if(dat$decision_number[j]>1){
      dat$Feature_matrix[j,1:dat$B[j],i] <- standardize(dat$Feature_matrix[j,1:dat$B[j],i])
    } else{
      dat$Feature_matrix[j,1:dat$B[j],i] <- -Inf
    }
  }
}


m_parallel <- cmdstan_model("Minecraft_parallel_solo_asocial.stan", cpp_options = list(stan_threads = TRUE))
fit_parallel_group_asocial <- m_parallel$sample(dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, refresh = 1, iter_warmup = 1500,adapt_delta = 0.99, iter_sampling = 2500)
stanfit <- rstan::read_stan_csv(fit_parallel_group_asocial$output_files())
s_solo <- extract.samples(stanfit)

