# trajectory analyses
#Charley Wu
rm(list=ls()) #house keeping


#load packages
packages <- c('tidyverse','zoo',  'DescTools','brms','arrow', 'trajr')
invisible(lapply(packages, require, character.only = TRUE))

#read in data

playerDF <- data.frame()


#2021 data
dataFolders <- c('data/2021batch/')
for (dataFolder in dataFolders){
  playerDF <- rbind(playerDF, arrow::read_feather(paste0(dataFolder, 'all_players_pd_cache.feather')))
}

#add unique ids
playerDF$id <- paste0(playerDF$name, playerDF$session)

#Create new dataframe
turningDF <- data.frame()

#Define window for time-lagged mean
window = 45 #equivalent to the 2.25 seconds it takes to destroy a block, based on the 20hz sampling rate

#loop through data
for (i in unique(playerDF$id)){
  playersub <- subset(playerDF, id == i)
  for (r in unique(playerDF$round)){
    subDF <- subset(playerDF, id == i & round == r)
    #subDF <- subset(playerDF, id == 'MPIB1session24.json' & round == 2) #test
    #1. Directly measure chances in gaze angle from time to time
    delta_gaze <- atan(diff(subDF$zlook)/diff(subDF$xlook)) #arctan of ratio between changes in looking angle
    delta_gaze[is.nan(delta_gaze)] <- 0 #replace NaN with zero (these are due to division by 0)
    delta_gaze <- c(NA, delta_gaze) #append a NA for the first datapoint
    delta_gaze <- rollmean(abs(delta_gaze),window,fill=NA, align='right') #rolling mean of absolute delta-gaze 
    #plot(delta_gaze)
    
    #2. Use method from Pacheco-Cobos et al., (PNAS 2019) to compute turning rate from spatial coordinates alone
    theta <- atan(diff(subDF$z)/diff(subDF$x))
    theta[is.nan(theta)] <- 0
    #plot(theta)
    delta_turning <- diff(theta)/pi
    delta_turning <- c(NA, NA, delta_turning) #append two NAs, since this is a difference of differences
    delta_turning <- rollmean(abs(delta_turning),window,fill=NA, align='right') #rolling mean of absolute delta-gaze 
    #plot(delta_turning)
    
    #3. Straightness index based on displacement/time with some moving window
    
    displacement <- c(rep(NA, window), unlist(sapply(window:nrow(subDF), function(d)sqrt((subDF$x[d] - subDF$x[d-window])^2 + (subDF$z[d] - subDF$z[d-window])^2 ))))
    straightness = displacement/(window*.05)
    #plot(straightness)
    
    #Compile measures
    subDF$delta_gaze <- delta_gaze
    subDF$delta_turning <- delta_turning
    subDF$straightness <- straightness
    
    #plotDF<- select(subDF, 'time', 'delta_gaze', 'delta_turning', 'straightness') %>% pivot_longer(cols = c('delta_gaze', 'delta_turning', 'straightness'))
    # ggplot(plotDF, aes(x = time, y = abs(value), color = name))  +
    #   geom_line()+
    #   theme_classic()+
    #   facet_wrap(~name, scales = 'free')
    
    #Aggregate together
    turningDF<- rbind(turningDF, select(subDF,'id', 'round', 'time', 'delta_gaze', 'delta_turning', 'straightness'))
                      
  }
}

# #save to disk
write_feather(turningDF, 'dynamicData/trajectories.feather')
# # saveRDS(turningDF, 'dynamicData/trajectories.RDS') #alternative file format