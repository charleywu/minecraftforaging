#Creates the trial structure of the task in a environments/trialstructure.json
#Charley Wu, Ralf Kurvers 2019
library(dplyr)
library(schoolmath)
library(rapportools)
library(tidyverse)
library(jsonlite)

################################################################################
# Experiment variables
################################################################################
envTypes <- c('random','smooth') #environment types
soloRound <- c(TRUE, FALSE )
blockTypes <- c('pumpkin', 'melon')
swaps <- c("environment", "soloRound")
rounds <- seq(1,16) #+ 2 training rounds
games <- seq(1,16)
envNumbers <- seq(1,20) #env 21 and 22 are for demo purposes in the tutorial
trainingRoundLength <- 120
fullRoundLength <- 120

#conditions defines the starting conditions (e.g., for the first round in a game) along with which of the variable (either environment or roundtype) changes ("swaps") first
conditions <- expand.grid(environment = envTypes, soloRound = soloRound, block = blockTypes, swap = swaps)

#swap function. Value is any of soloRound, envTypes, or blockTypes. The function will return the opposite value
swapFun <- function(value){
  #Round types
  if (value==soloRound[1]){
    return (soloRound[2])
  }else if(value==soloRound[2]){
    return (soloRound[1])
  #Env types
  }else if (value==envTypes[1]){
    return (envTypes[2])
  }else if (value==envTypes[2]){
    return (envTypes[1])
  #Block types
  }else if (value==blockTypes[1]){
    return (blockTypes[2])
  }else if (value==blockTypes[2]){
    return (blockTypes[1])
  }
}

################################################################################
# Start building each game array
################################################################################

for (g in games){#loop through all games
  starting <- conditions[g,] #this will define the first 4 rounds
  if (starting$swap == 'environment'){ #if environment swaps first
    gameArray <- expand.grid(rep = seq(1,4), environment = c(as.character(starting$environment), swapFun(starting$environment)), soloRound = c(as.character(starting$soloRound), swapFun(starting$soloRound)), trainingRound="FALSE")
  }else if(starting$swap == 'soloRound'){#If roundtype swaps first
    gameArray <- expand.grid(rep = seq(1,4),   soloRound = c(as.character(starting$soloRound), swapFun(starting$soloRound)), environment = c(as.character(starting$environment), swapFun(starting$environment)), trainingRound="FALSE")
    gameArray <- gameArray[,c(1,3,2,4)] #reorder columns back
  }
  #Append two training rounds at the very top
  gameArray <- rbind(data.frame(rep=NA, environment = c(as.character(starting$environment), swapFun(starting$environment)), soloRound = TRUE, trainingRound="TRUE"), gameArray) #training rounds are always solo
  #Now add the correct blocktype
  gameArray$blockType <- ifelse(gameArray$environment == starting$environment, as.character(starting$block), swapFun(starting$block))
  gameArray$round <- seq(1,18) #rounds; first 2 are training rounds
  #Now add timelimits: 
  gameArray$time_limit <- c(trainingRoundLength,trainingRoundLength, rep(fullRoundLength,16))
  #now add the correct csv file to load the environment from, sampling WITHOUT replacement for each category of environment
  gameArray$envNum <- NA #placeholder
  gameArray[gameArray$environment=='random','envNum'] <- sample(seq(1,20), 9, replace = F)
  gameArray[gameArray$environment=='smooth','envNum'] <- sample(seq(1,20), 9, replace = F)
  #Now build the environment file name
  gameArray$envFile <- paste0(gameArray$environment, '.', gameArray$envNum, '.csv')
  gameArray$game <- g
  #Save output: [game meta data, round data]
  output <- toJSON(list(metaData = data.frame(smoothBlockType =unique(gameArray[gameArray$environment=='smooth','blockType']), language = 'En' ), roundData = gameArray[,c('game', 'round', 'environment', 'blockType', 'soloRound', 'envFile', 'time_limit', 'trainingRound')]))
  write(output, paste0('sessions/session',g,'.json'))
}









