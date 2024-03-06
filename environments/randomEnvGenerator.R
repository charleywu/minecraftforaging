#Random environment generator
#Binary resources (0=no reward; 1 = reward)
#Charley Wu, July 2019

rm(list=ls()) #house keeping
library('lattice')
library('jsonlite')
library('extraDistr')


#Aesthetics
palf <- colorRampPalette(c("white", "black")) 

#Environment variables
replications = 22
gridSize = 20 #square environment with this many blocks along each edge
density = .25
envList <- list()
cond <- 'random'


envNum <- 0 #counter for each environment
for (r in 1:replications){
  #Generate zero matrix
  env <- rep(0, gridSize^2) #generate vector (one value per block) defaulted to 0
  env[sample(x = 1:gridSize^2, size = (gridSize^2 * density), replace = F)] <- 1 #sample blocks with p(density) to be flipped to 1
  environment = matrix(env, nrow = gridSize, ncol = gridSize)  

  
  #assign row and column labels50*50
  rownames(environment) <- sprintf("x%03d", seq(1,gridSize))
  colnames(environment) <-  sprintf("y%03d", seq(1,gridSize))
  
  envNum <- envNum + 1
  envList[[envNum]] <- environment
  
  #save plot
  png(filename = paste0('environments/plots/', cond,'.',envNum, '.png'), width = 64, height = 64, units = "px")
  op <- par(mar = rep(0, 4)) #set option without margins
  image(environment,xaxt='n', yaxt='n', col = palf(100)) #plot data
  par(op)
  dev.off()
    

  #Save matrix
  write.table(environment, paste0('environments/', cond,'.',envNum, '.csv'), row.names = TRUE, col.names = TRUE, sep=';')
  
}




