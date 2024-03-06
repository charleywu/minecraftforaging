#Smooth environment generator using a GP-RBF prior
#Charley Wu July 2019

library("ggplot2")
library("plgp")
library('cowplot')

# kernel function
rbf_D <- function(X,l=1, eps = sqrt(.Machine$double.eps) ){
  D <- plgp::distance(X)
  Sigma <- exp(-D/(2*l))^2 + diag(eps, nrow(X))
}

#Simulation parameters
conditions <- c('smooth') #environment manipulation
lambdas <- c(4) #corresponding lambda values
noise <- 0.001 
resourceRate <- 0.25
nx <- 20 #size of environment
x <- seq(1,nx)
X <- expand.grid(x, x) #grid of pairwise values
replications <- 22
envList = list()

for (cond in 1:length(conditions)){
  lambda <- lambdas[cond]
  conditionName <- conditions[cond]
  envList[[cond]] <- list()
  # Compute kernel based on lambda
  Sigma <- rbf_D(X,l=lambda)
  for (rep in 1:replications){
    # sample from multivariate normal with mean zero, sigma = sigma
    Y <- MASS::mvrnorm(1,rep(0,dim(Sigma)[1]), Sigma)
    #put it into a dataframe
    pp <- data.frame(y=Y,x1=X[,1],x2=X[,2])
    #plot continuous payoffs
    p1<- ggplot(pp,aes(x=x1,y=x2)) +
      geom_raster(aes(fill=y), interpolate = T) +
      geom_contour(aes(z=y), bins = 12, color = "white", size = 0.2, alpha = 0.5) +
      coord_equal() +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_viridis_c(option = "viridis", name = 'Fitness')+
      ggtitle('Continuous Payoff')
    
    #Quartile split
    split <- quantile(Y, probs=1 - resourceRate)
    Ybin <- ifelse(Y>split, 1, 0) 
    print(sum(Ybin))
    # Binarize payoffs above 75th quartile
    pbin <- data.frame(y=Ybin,x1=X[,1],x2=X[,2])
    pbin$y <- factor(pbin$y)
    #Plot binarized rewards
    p2<- ggplot(pbin,aes(x=x1,y=x2)) +
      geom_tile(aes(fill=y)) +
      #geom_contour(aes(z=y), bins = 12, color = "gray30", size = 0.5, alpha = 0.5) +
      coord_equal() +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(values=c('white', 'black'))+
      theme(legend.position = 'none')+
      ggtitle('Binarized payoff')
    p <- plot_grid(p1,p2, align = 'h', scale = c(1,0.75))
    filename <-paste0('environments/plots/', conditionName,'unbinarized.',rep,'.pdf') #save full plot
    ggsave(filename, p)
    #Now save just the binarized reward distributions
    png(filename = paste0('environments/plots/', conditionName,'.',rep, '.png'), width = 64, height = 64, units = "px")
    op <- par(mar = rep(0, 4)) #set option without margins
    image(matrix(Ybin, nrow = gridSize, ncol = gridSize)  ,xaxt='n', yaxt='n', col = palf(100)) #plot data
    par(op)
    dev.off()
    
    #Now save the environment
    environment <- matrix(Ybin, nrow = nx)
    rownames(environment) <- sprintf("x%02d", seq(1,nx))
    colnames(environment) <-  sprintf("y%02d", seq(1,nx))
    write.table(environment, paste0('environments/', conditionName,'.', rep, '.csv'), row.names = TRUE, col.names = TRUE, sep=';')  
    envList[[cond]][[rep]] <- environment #add to masterlist
    
  }
  
}
# 
# loss <- function(lambda){
#   #original
#   nx <- 20 #size of environment
#   x <- seq(1,nx)
#   X <- expand.grid(x, x) #grid of pairwise values
#   Sigma <- rbf_D(X,l=4)
#   #Expanded grid used in the game
#   X.alt = expand.grid(seq(2,59, length.out=20), seq(2,59, length.out=20))
#   Sigma.alt <- rbf_D(X.alt,l=lambda)
#   return(sum(abs(Sigma-Sigma.alt)))
# }
# 
# 
# optimize(loss, interval = c(1,24))