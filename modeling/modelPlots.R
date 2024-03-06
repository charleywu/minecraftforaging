# Plotting Script for reinforcement learning model results
#Charley Wu and Dominik Deffner 2023

rm(list=ls()) #house keeping
#setwd('modeling')


#load packages
packages <- c('tidyr','tidybayes', 'ggplot2', 'ggh4x', 'scales','RColorBrewer','shape', 'abind', 'plyr', 'cowplot')
invisible(lapply(packages, require, character.only = TRUE))




############################################
#Model comparison
############################################
modelPal <- c("#000000","#44AA99","#004488","#88CCee", "#999933",  "#CC6677", "#882255",  "#AA4499" )


#PXP computed in PXP.ipynb
#Group rounds
pxpDFgroup <- data.frame(model =  c("Asocial", "Unbiased","Success-biased","Player-specific", "ARS", "Critical", "Conditional", "ARS+Cond"), pxp = t(read.csv('data/modelComparison/pxp.csv', header=F))[,1])
pxpDFgroup$model <- factor(pxpDFgroup$model, levels = c("Asocial", "Unbiased","Success-biased","Player-specific", "ARS","Critical", "Conditional", "ARS+Cond"))
#Solo
pxpDFSolo <- data.frame(model =  c("Asocial", "ARS"), pxp = t(read.csv('data/modelComparison/PXPsolo.csv', header=F))[,1])
pxpDFSolo$model <- factor(pxpDFSolo$model, levels = c("Asocial", "ARS"))

pxpDFgroup$condition <- 'group'
pxpDFSolo$condition <- 'solo'

pxpDF <- rbind(pxpDFgroup, pxpDFSolo)

pPXP <-ggplot(pxpDF, aes(x = model, y = pxp, fill = model))+
  geom_bar(stat='identity', color = 'black')+
  theme_classic()+
  facet_grid(~condition, scales = 'free', space='free')+
  xlab('')+
  ylab('p(bestModel)')+
  #scale_y_continuous(labels = scales::percent)+
  #scale_fill_manual(values = c('black', 'grey', '#0096FF', 'grey'))+
  scale_fill_manual(values = modelPal)+
  ggtitle('Bayesian model selection')+
  theme(legend.position='none', axis.text.x = element_text(angle=90, vjust=1, hjust=1),axis.title.x=element_blank(), strip.background=element_blank(), panel.spacing = unit(1, "lines"))
pPXP

#WAIC

#Group rounds
WAIC <- data.frame(read.csv('Stanfits/IndividualWAICPlus.csv'))
names(WAIC)[1] <- "id"
#Compute relative WAICS
WAIC$Asocial <- WAIC$Asocial/WAIC$ConditionalARS
WAIC$Unbiased <- WAIC$Unbiased/WAIC$ConditionalARS
WAIC$Success.based <- WAIC$Success.based/WAIC$ConditionalARS
WAIC$Unique <- WAIC$Unique/WAIC$ConditionalARS
WAIC$ARS <- WAIC$ARS/WAIC$ConditionalARS
WAIC$Critical <- WAIC$Critical/WAIC$ConditionalARS
WAIC$Conditional <- WAIC$Conditional/WAIC$ConditionalARS
WAIC$ConditionalARS <- 1

WAICdf <- WAIC %>% pivot_longer(cols = -id, values_to = 'WAIC', names_to = 'model')
WAICdf$model <- factor(WAICdf$model, levels = c("Asocial", "Unbiased","Success.based","Unique",  "ARS", "Critical", "Conditional", "ConditionalARS"))
levels(WAICdf$model) <-  c("Asocial", "Unbiased","Success-biased","Player-specific", "ARS", "Critical", "Conditional", "ARS+Cond")
WAICdf$condition <-'group'

#solo
WAICsolo <- data.frame(read.csv('Stanfits/SoloWAIC.csv'))
names(WAICsolo)[1] <- "id"
#Compute relative WAICS
WAICsolo$Solo <- WAICsolo$Solo/WAICsolo$Solo_ARS
WAICsolo$Solo_ARS <- 1

WAICdfsolo <- WAICsolo %>% pivot_longer(cols = -id, values_to = 'WAIC', names_to = 'model')
WAICdfsolo$model <- factor(WAICdfsolo$model, levels = c("Solo", "Solo_ARS"))
levels(WAICdfsolo$model) <-  c("Asocial", "ARS")
WAICdfsolo$condition <-'solo'

#Combine

WAICdf <- rbind(WAICdf,WAICdfsolo )


pRelWAIC <- ggplot(WAICdf, aes(x=model, y = WAIC, color = model))+
  theme_classic()+
  facet_grid(~condition, scales = 'free', space='free')+
  geom_hline(yintercept= 1, linetype = 'dashed')+
  stat_summary(fun = mean, geom='point', size = 0.6)+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width = 0.6)+
  scale_color_manual(values = modelPal)+
  #coord_cartesian(ylim = c(1019, 1050))+
  #stat_summary(fun.data = mean_se, geom='errorbar', width = 0.2)+
  ylab('Rel. WAIC (>1 is worse)')+
  xlab('')+
  theme(legend.position='none', axis.text.x = element_text(angle=90, vjust=1, hjust=1), axis.title.x=element_blank(), strip.background=element_blank(), panel.spacing = unit(1, "lines"))
pRelWAIC


############################################
#Model weights
############################################

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
#Load Stan data and assign variable names
s_success <- loadRData("Stanfits/fit_success_050123")
s_asocial <- loadRData("Stanfits/fit_asocial_050123")
s_same <- loadRData("Stanfits/fit_same_050123")
s_unique <- loadRData("Stanfits/fit_unique_050123")

#Adaptive models
s_ars <- loadRData("Stanfits/s_ARS_weights")
s_critical <-  loadRData("Stanfits/s_critical_weights")
s_conditional <- loadRData("Stanfits/s_conditional_weights")
s_ars_conditional <- loadRData("Stanfits/s_conditional_ARS_weights")

#Solo rounds computed separately
s_solo <- loadRData("Stanfits/fit_solo_050123")
s_soloARS <-  loadRData("Stanfits/s_solo_ARS_weights")


############################################
#Create a plottable dataframe for weights
############################################
n <- 5000 #number of posterior samples

modelList <- c("Solo", "SoloARS", "Asocial", "Unbiased","Success-biased","Player-specific", "ARS", "Critical", "Conditional", "ARS+Cond")
weightList <- list(s_solo, 
                   abind(s_soloARS$weights, array(s_soloARS$beta_t_ind, dim = c(5000, 1, 2)), along = 2),
                   s_asocial, s_same, s_success, 
                   abind(s_unique[,1:3,], apply(s_unique[,4:131, ], c(1,3), mean), along=2), #Average over players
                   abind(s_ars$weights, array(s_ars$beta_t_ind, dim = c(5000, 1, 2)), along = 2), #concatenate adaptivity weights
                   abind(s_critical$weights, array(s_critical$beta_t_soc, dim = c(5000, 1, 2)), along = 2), 
                   abind(s_conditional$weights, s_conditional$beta_t_ind, along = 2), 
                   abind(s_ars_conditional$weights, s_ars_conditional$beta_t_ind, along = 2))




weightNames <- list(
  c('Locality', 'Block Visibility', 'Reward Prediction'), #solo
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality'), #soloARS
  c('Locality', 'Block Visibility', 'Reward Prediction'), #Asocial
  c('Locality', 'Block Visibility', 'Reward Prediction', "Social Proximity"), #Unbiased
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity'), #Success-biased
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Player-specific Proximity'), #Player-specific
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality'), #ARS
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Reward'), #Critical
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Successful', 'Adapt. Unsuccessful'), #Conditional
  c('Locality', 'Block Visibility', 'Reward Prediction', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Locality', 'Adapt. Successful', 'Adapt. Unsuccessful') #ARS+Cond
)

#Initialize dataframe
columns <-  c('sample', 'env', 'model', unique(unlist(weightNames)) )
modelDF <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(modelDF) <-columns

#add model weights
for (m in 1:length(modelList)){
  weights <- weightList[[m]] #extract weights
  #do each env separately
  #Rand
  randDF <- data.frame(weights[,,1])
  colnames(randDF) <- weightNames[[m]]
  randDF$env <- 'random'
  #Smooth
  smoothDF <- data.frame(weights[,,2])
  colnames(smoothDF) <- weightNames[[m]]
  smoothDF$env <- 'smooth'
  #combine and add missing column labels
  combDF <- rbind(randDF, smoothDF)
  combDF$model <-  modelList[m]
  combDF$sample <- 1:n
  #add in to main df
  modelDF <- rbind.fill(modelDF, combDF)
}

#Flip signs to make some weights more interpretable (distance--> locality)
modelDF$Locality <- modelDF$Locality*-1
modelDF$`Social Proximity` <- modelDF$`Social Proximity`*-1
modelDF$`Successful Proximity` <- modelDF$`Successful Proximity`*-1
modelDF$`Unsuccessful Proximity` <- modelDF$`Unsuccessful Proximity`*-1
modelDF$`Player-specific Proximity` <- modelDF$`Player-specific Proximity`*-1
modelDF$`Adapt. Locality` <- modelDF$`Adapt. Locality`*-1
modelDF$`Adapt. Successful` <- modelDF$`Adapt. Successful`*-1
modelDF$`Adapt. Unsuccessful` <- modelDF$`Adapt. Unsuccessful`*-1

#Pivot
modelDF <- modelDF %>% dplyr::group_by(sample, env, model) %>% pivot_longer(cols = c(unique(unlist(weightNames))), names_to = 'weights', values_to = 'posterior')

#add smooth-random contrast
contrastDF <- subset(modelDF, env == 'smooth') #copy over smooth weights first
contrastDF$posterior <- contrastDF$posterior - subset(modelDF, env == 'random')$posterior
contrastDF$env <- 'smooth - random'

modelDF <- rbind(modelDF, contrastDF)

#Add condition
modelDF$condition <- 'group'
modelDF[modelDF$model %in% c('Solo', 'SoloARS'), 'condition'] <- 'solo'
modelDF$condition <- factor(modelDF$condition)

#Save weights
#write_feather(modelDF, 'Stanfits/modelposteriors.feather')
postMeansDF <-  modelDF %>% dplyr::group_by(model, env, weights) %>% dplyr::summarize(est = mean(posterior))
write.csv(postMeansDF, 'data/modelWeights/allPostMeans.csv')


############################################
#Illustrative plot of adaptivity weights 
############################################
groupMeans <- subset(postMeansDF, env=='smooth' & model == 'ARS+Cond')

#Initialize DB with time
exampleDF <- expand.grid(time=seq(0,120))
exampleDF$normedTime <- (exampleDF$time - 12.89456)/13.01498 #Mean and SD from empirical data of observed individual success distribution; inverting the normalization in the STAN models
#Compute weights based on population level means
exampleDF$locality <- as.numeric(groupMeans[groupMeans$weights=='Locality', 'est']) + (as.numeric(groupMeans[groupMeans$weights=='Adapt. Locality', 'est']) * exampleDF$normedTime)
exampleDF$success <- as.numeric(groupMeans[groupMeans$weights=='Successful Proximity', 'est']) + (as.numeric(groupMeans[groupMeans$weights=='Adapt. Successful', 'est']) * exampleDF$normedTime)
exampleDF$unsuccess <- as.numeric(groupMeans[groupMeans$weights=='Unsuccessful Proximity', 'est']) + (as.numeric(groupMeans[groupMeans$weights=='Adapt. Unsuccessful', 'est']) * exampleDF$normedTime)

adaptDF <- exampleDF %>% pivot_longer(cols=c('locality', 'success', 'unsuccess'), names_to = 'Feature', values_to = 'Weights' )
adaptDF$Feature <- factor(adaptDF$Feature, levels = c('locality', 'success', 'unsuccess'))
levels(adaptDF$Feature) <- c('Locality', 'Successful', 'Unsuccessful')



pAdapt <- ggplot(adaptDF, aes(x = time, y = Weights, color = Feature, linetype = Feature))+
  geom_line()+
  theme_classic()+
  xlab(bquote(Delta~t[IndRew]~'(s)'))+
  ylab('Weights')+
  scale_color_manual(values = c("#4daf4a", "#377eb8", "#e41a1c"), name='')+
  scale_linetype_discrete(name='')+
  #ggtitle('Adaptivity')+
  scale_x_continuous(breaks = c(0,60,120))+
  scale_y_continuous(limits =c(-2, 14))+
  theme(legend.position = c(1.15,1), legend.justification = c(1,1),  strip.background=element_blank(),legend.background=element_blank(),legend.key.size = unit(0.2, "cm"), legend.spacing.y = unit(-0.7, 'cm'))
pAdapt

ggsave('plots/adaptivityWeights.pdf',pAdapt,  width = 1.6,height = 1.3, units = 'in')
############################################
#Plot model weights
############################################
#Factorize and add levels
modelDF$env <- factor(modelDF$env, levels =  c("smooth - random", 'smooth', 'random')) #reverse order
modelDF$model <- factor(modelDF$model, levels = c("Solo", "SoloARS", "Asocial", "Unbiased","Success-biased","Player-specific", "ARS", "Critical", "Conditional", "ARS+Cond"))
modelDF$weights <- factor(modelDF$weights, levels = unique(unlist(weightNames)))
modelDF$env <- factor(modelDF$env, levels =  c("smooth - random", 'smooth', 'random')) #reverse order


#Maintext figure
maintextDF <- subset(modelDF,model %in% c('SoloARS', 'ARS+Cond') &  env %in% c('smooth', 'random') & weights %in% c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Successful', 'Adapt. Unsuccessful') )
maintextDF$weights <- factor(maintextDF$weights, levels =c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality', 'Successful Proximity', 'Unsuccessful Proximity', 'Adapt. Successful', 'Adapt. Unsuccessful') )
maintextDF$model <- factor(maintextDF$model, levels =c('SoloARS', 'ARS+Cond'))
levels(maintextDF$model) <-c('solo', 'group')

levels(maintextDF$weights) <- c("Locality", "Block Visibility", "Reward Prediction", "Adapt. Locality", "Successful Prox.",  "Unsuccessful Prox.", "Adapt. Success.", "Adapt. Unsuccess.")   

pPosteriorDots <- ggplot(maintextDF, aes( y = posterior, shape = model, fill = env, color =env ))+
  geom_hline(yintercept=0, linetype  = 'dashed')+
  #stat_slab(normalize='panels', alpha = 0.7, color = NA)+
  stat_pointinterval(.width = c(0, 0.95),position = position_dodge(width = .4, preserve = "single"), size=0.8,linewidth =0.4) +
  facet_wrap(weights~., scales = "free",  ncol=4, dir="h")+
  #facet_grid2(feature~.,  scales = "free_x", independent = "x")+
  #facet_grid2(~feature, scales = "free",  axes = "all", independent='all')+
  scale_fill_manual(values = rev(c("#E69F00","#009E73")), name="Environment")+
  scale_color_manual(values =rev(c("#E69F00","#009E73")), name="Environment")+
  scale_shape_manual(values = c(16,15), name="Condition")+
  theme_classic()+
  #guides(shape = guide_legend(override.aes = list(size=2)))+
  coord_flip()+
  scale_y_continuous(n.breaks = 3)+
  guides(color = guide_legend(reverse=TRUE),fill = guide_legend(reverse=TRUE), shape = guide_legend(reverse=TRUE))+
  xlab('')+
  ggtitle('Model weights')+
  ylab('Estimates')+
  theme(legend.position = 'right', strip.background=element_blank(),legend.background=element_blank(), legend.spacing.y = unit(0.01, 'cm'), legend.direction = "vertical",legend.margin=margin(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(), 
        strip.text.x =element_text(angle = 0, hjust = -.1))
pPosteriorDots


#ggsave('plots/modelPosteriorPaper.pdf', pPosteriorDots, width = 10, height = 3.5, units = 'in')


#SI

modelDF$weights <- factor(modelDF$weights, 
                          levels =  c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality', 
                                      'Social Proximity', 'Successful Proximity', 'Unsuccessful Proximity', 'Player-specific Proximity',
                                      'Adapt. Reward', 'Adapt. Successful', 'Adapt. Unsuccessful')) 

#Collapse model names
modelDF[modelDF$condition=='solo' & modelDF$model == 'SoloARS', 'model'] <- 'ARS'
modelDF[modelDF$condition=='solo' & modelDF$model == 'Solo', 'model'] <- 'Asocial'
modelDF$model <- factor(modelDF$model, levels =  c("Asocial", "Unbiased","Success-biased","Player-specific", "ARS", "Critical", "Conditional", "ARS+Cond"))

#Social models
pPosteriorDotsSISocial <- ggplot(subset(modelDF, env %in% c('smooth', 'random') & condition=='group' ), aes( y = posterior, shape = model, fill = env, color =env ))+
  geom_hline(yintercept=0, linetype  = 'dashed')+
  #stat_slab(normalize='panels', alpha = 0.7, color = NA)+
  stat_pointinterval(.width = c(0, 0.95),position = position_dodge(width = .6, preserve = "single"), size=0.8,linewidth =0.4 ) +
  facet_wrap(weights~., scales = "free",  ncol=4, dir="h")+
  #facet_grid2(feature~.,  scales = "free_x", independent = "x")+
  #facet_grid2(~feature, scales = "free",  axes = "all", independent='all')+
  scale_fill_manual(values = rev(c("#E69F00","#009E73")), name="Environment")+
  scale_color_manual(values =rev(c("#E69F00","#009E73")), name="Environment")+
  scale_shape_manual(values = c(1, 15, 17,0, 4,5,2, 11), name="Model")+
  theme_classic()+
  guides(color = guide_legend(reverse=TRUE),fill = guide_legend(reverse=TRUE), shape = guide_legend(reverse=TRUE))+
  #guides(shape = guide_legend(override.aes = list(size=2)))+
  coord_flip()+
  xlab('')+
  ggtitle('Model Weights (Group rounds)')+
  ylab('Estimates')+
  theme(legend.position = 'right', strip.background=element_blank(),
        legend.background=element_blank(), legend.direction = "vertical",legend.margin=margin(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        strip.text = element_text(hjust = 0))
pPosteriorDotsSISocial

ggsave('plots/fullModelWeightsSocial.pdf', pPosteriorDotsSISocial, width = 12, height = 6, units = 'in')


#Solo models #TODO:

#Social models
pPosteriorDotsSISolo <- ggplot(subset(modelDF, env %in% c('smooth', 'random') & condition=='solo' & weights %in% c('Locality', 'Block Visibility', 'Reward Prediction', 'Adapt. Locality') ), aes( y = posterior, shape = model, fill = env, color =env ))+
  geom_hline(yintercept=0, linetype  = 'dashed')+
  #stat_slab(normalize='panels', alpha = 0.7, color = NA)+
  stat_pointinterval(.width = c(0, 0.95),position = position_dodge(width = .6, preserve = "single"), size=0.8,linewidth =0.4 ) +
  facet_wrap(weights~., scales = "free", ncol=4, dir="h")+
  #facet_grid2(feature~.,  scales = "free_x", independent = "x")+
  #facet_grid2(~feature, scales = "free",  axes = "all", independent='all')+
  scale_fill_manual(values = rev(c("#E69F00","#009E73")), name="Environment")+
  scale_color_manual(values =rev(c("#E69F00","#009E73")), name="Environment")+
  scale_shape_manual(values = c(1, 4), name="Model")+
  theme_classic()+
  guides(color = guide_legend(reverse=TRUE),fill = guide_legend(reverse=TRUE), shape = guide_legend(reverse=TRUE))+
  #guides(shape = guide_legend(override.aes = list(size=2)))+
  coord_flip()+
  xlab('')+
  ggtitle('Model Weights (Solo rounds)')+
  ylab('Estimates')+
  theme(legend.position = 'right', strip.background=element_blank(),
        legend.background=element_blank(), legend.direction = "vertical",legend.margin=margin(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        strip.text = element_text(hjust = 0))
pPosteriorDotsSISolo

ggsave('plots/fullModelWeightsSolo.pdf', pPosteriorDotsSISolo, width = 12, height = 3, units = 'in')

############################################
#Combine together
############################################
pComplete <-  cowplot::plot_grid(pPXP+ggtitle('Bayesian\n model selection'),pRelWAIC+ggtitle('Individual fits'), pPosteriorDots + theme(strip.text.x = element_text(hjust = -.1)), rel_widths = c(1,1, 3), nrow = 1, labels = c('b','c', 'd'))
pComplete

ggsave('plots/modelBottom.pdf', pComplete, width = 10.5, height = 3.5, units = 'in')

############################################
#Examine effects/contrasts
############################################


formatHDI <- function(x, signDig=2, est = 'mean'){
  x <- unlist(x)
  if (est == 'mean'){
    x.est <- sprintf("%.*f",signDig, mean(x))
  }else{
    x.est <- sprintf("%.*f",signDig, median(x))
  }
  x.CI <- sprintf("%.*f",signDig, hdi(x))
  return(paste0(x.est, ' [', x.CI[1], ', ', x.CI[2], ']'))
}


#locality
formatHDI(modelDF[modelDF$model=='ARS' & modelDF$env == 'smooth - random' & modelDF$weights=='Locality' & modelDF$condition == 'solo', 'posterior'],1) #TODO: replace with ARS+solo

#GP
formatHDI(c(modelDF[modelDF$condition == 'solo' & modelDF$model=='ARS' & modelDF$env == 'smooth' & modelDF$weights=='Reward Prediction', 'posterior'], modelDF[modelDF$condition == 'group' & modelDF$model=='ARS+Cond' & modelDF$env == 'smooth' & modelDF$weights=='Reward Prediction', 'posterior']),2) 
formatHDI(modelDF[modelDF$condition == 'solo' & modelDF$model=='ARS' & modelDF$env == 'random' & modelDF$weights=='Reward Prediction', 'posterior'],2) 
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'random' & modelDF$weights=='Reward Prediction', 'posterior'],2)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env !='smooth - random' & modelDF$weights=='Reward Prediction', 'posterior'] - modelDF[modelDF$condition == 'solo' & modelDF$model=='ARS'& modelDF$env != 'smooth - random' & modelDF$weights=='Reward Prediction', 'posterior'],2)

#Adapt Local.
formatHDI(modelDF[modelDF$condition == 'solo' & modelDF$model=='ARS'& modelDF$env == 'smooth - random' & modelDF$weights=='Adapt. Locality', 'posterior'], 2)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth - random' & modelDF$weights=='Adapt. Locality', 'posterior'],2)

formatHDI(modelDF[modelDF$condition == 'solo' & modelDF$model=='ARS'& modelDF$env == 'random' & modelDF$weights=='Adapt. Locality', 'posterior'], 2)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env !='smooth - random' & modelDF$weights=='Adapt. Locality', 'posterior'] - modelDF[modelDF$condition == 'solo' & modelDF$model=='ARS'& modelDF$env != 'smooth - random' & modelDF$weights=='Adapt. Locality', 'posterior'],2)

## successful
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth' & modelDF$weights=='Successful Proximity', 'posterior'],1)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'random' & modelDF$weights=='Successful Proximity', 'posterior'],2)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth - random' & modelDF$weights=='Successful Proximity', 'posterior'],1)

formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth' & modelDF$weights=='Unsuccessful Proximity', 'posterior'],3)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'random' & modelDF$weights=='Unsuccessful Proximity', 'posterior'],2)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth - random' & modelDF$weights=='Unsuccessful Proximity', 'posterior'],2)

# Social adaptivity
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth' & modelDF$weights=='Adapt. Successful', 'posterior'],1)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'random' & modelDF$weights=='Adapt. Successful', 'posterior'],2)
formatHDI(modelDF[modelDF$model=='ARS+Cond' & modelDF$env == 'smooth - random' & modelDF$weights=='Adapt. Successful', 'posterior'],1)


# 
# #talk figure
# pPosterior <- ggplot(na.omit(subset(modelDF,  env!='smooth - random')), aes(x = post, fill = env, color =env))+
#   geom_vline(xintercept=0)+
#   stat_slab(normalize='panels', alpha = 0.7, color = NA)+
#   stat_pointinterval(position = position_dodge(width = .2, preserve = "single")) +
#   facet_grid2(feature~type, scales = "free",  axes = "all", independent='all', switch="y")+
#   scale_fill_manual(values = c("#E69F00","#009E73", 'black'), name="")+
#   scale_color_manual(values = c("#E69F00","#009E73", 'black'), name="")+
#   theme_classic()+
#   xlab('Model Weights')+
#   ylab('')+
#   theme(legend.position = c(.9,0.1), legend.justification =c(1,0), strip.background=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y = element_blank(),  panel.border = element_rect(colour = "grey", fill=NA, size=.2))
# pPosterior
# 
# ggsave('plots/modelPosteriorTalk.pdf', pPosterior, width = 6, height = 8, units = 'in')

############################################
#Graveyard
############################################

# 
# ############################################
# #Vanilla plotting function
# ############################################
# #Assign variables and compute contrasts
# #Solo
# locSoloRand   <-  s_solo$weights[, 1, 1]
# locSoloSmooth <-  s_solo$weights[, 1, 2]
# locSoloCont <- locSoloSmooth-locSoloRand
# 
# GPSoloRand   <-  s_solo$weights[, 2, 1]
# GPSoloSmooth <-  s_solo$weights[, 2, 2]
# GPSoloCont <- GPSoloSmooth-GPSoloRand
# 
# 
# #Group
# locGroupRand   <-  s_group$weights[, 1, 1]
# locGroupSmooth <-  s_group$weights[, 1, 2]
# locGroupCont <- locGroupSmooth-locGroupRand
# 
# GPGroupRand   <-  s_group$weights[, 2, 1]
# GPGroupSmooth <-  s_group$weights[, 2, 2]
# GPGroupCont <- GPGroupSmooth-GPGroupRand
# 
# SuccessGroupRand   <-  s_group$weights[, 3, 1]
# SuccessGroupSmooth <-  s_group$weights[, 3, 2]
# SuccessGroupCont <- SuccessGroupSmooth-SuccessGroupRand
# 
# UnsuccessGroupRand   <-  s_group$weights[, 4, 1]
# UnsuccessGroupSmooth <-  s_group$weights[, 4, 2]
# UnsuccessGroupCont <- UnsuccessGroupSmooth-UnsuccessGroupRand
# 
# 
# 
# 
# #Create (colorblind friendly) color palette
# x <- seq(from=0, to=1, by=0.2) # fake data
# col.pal <- brewer.pal(length(x), "Dark2") #create a palette which you loop over for corresponding values
# 
# 
# #graphics.off()
# png("Stanfit_plot.png", res = 400, height = 28, width = 34, units = "cm")
# 
# 
# par(mfrow = c(4,4), 
#     mar= c(3,2.5,0,1.5), 
#     oma =c(0,8,4,0))
# 
# 
# #####
# ###
# ##
# # LOCALITY
# ##
# ###
# ####
# 
# 
# #Group 
# dens <- density(locGroupRand)
# x1 <- min(which(dens$x >= quantile(locGroupRand, 0)))  
# x2 <- max(which(dens$x <  quantile(locGroupRand, 1)))
# plot(dens, xlim = c(-10,10), ylim = c(0,2), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[6],alpha = 0.75), border = NA))
# mtext("Group Rounds",  cex = 2, side = 3, line = 1.5, at=15)
# abline(v=0, lty=2)
# 
# dens <- density(locGroupSmooth)
# x1 <- min(which(dens$x >= quantile(locGroupSmooth, 0)))  
# x2 <- max(which(dens$x <  quantile(locGroupSmooth, 1)))
# par(new = TRUE)
# plot(dens, xlim = c(-10,10), ylim = c(0,2), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[1],alpha = 0.75), border = NA))
# mtext("Locality", side = 2, line = 5, cex = 1.2)
# 
# 
# Arrows(-1 , 1.9, -10 , 1.9, arr.type="triangle", arr.width=0.2)
# Arrows(1,1.9,10,1.9, arr.type="triangle", arr.width=0.2)
# 
# text(-5,1.82, "Closer to self")
# text(5,1.82, "Farther from self")
# 
# par(mar=c(3,1,0,2))
# 
# dens <- density(locGroupCont)
# x1 <- min(which(dens$x >= quantile(locGroupCont, 0.025)))  
# x2 <- max(which(dens$x <  quantile(locGroupCont, 0.975 )))
# plot(dens, xlim = c(-1,1), ylim = c(0,6), type="l", ann = FALSE,col=alpha("black",alpha = 0.75), bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# legend("topright", "Smooth-Random", bty="n", cex=1.1) 
# 
# 
# 
# #Solo 
# dens <- density(locSoloRand)
# x1 <- min(which(dens$x >= quantile(locSoloRand, 0)))  
# x2 <- max(which(dens$x <  quantile(locSoloRand, 1)))
# plot(dens, xlim = c(-2,2), ylim = c(0,9), type="n", ann = FALSE, bty ="n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[6],alpha = 0.75), border = NA))
# mtext("Solo Rounds",  cex = 2, side = 3, line = 1.5, at=3)
# abline(v=0, lty=2)
# 
# 
# 
# Arrows(-0.18, 8.5, -1.8, 8.5, arr.type="triangle", arr.width=0.2)
# Arrows(0.18, 8.5, 1.8, 8.5, arr.type="triangle", arr.width=0.2)
# 
# text(-1,8.2, "Closer to self")
# text(0.9,8.2, "Farther from self")
# 
# dens <- density(locSoloSmooth)
# x1 <- min(which(dens$x >= quantile(locSoloSmooth, 0)))  
# x2 <- max(which(dens$x <  quantile(locSoloSmooth, 1)))
# par(new = TRUE)
# plot(dens, xlim = c(-2,2), ylim = c(0,9), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[1],alpha = 0.75), border = NA))
# 
# 
# 
# 
# par(mar= c(3,1,0,2.5))
# 
# 
# dens <- density(locSoloCont)
# x1 <- min(which(dens$x >= quantile(locSoloCont, 0.025)))  
# x2 <- max(which(dens$x <  quantile(locSoloCont, 0.975 )))
# plot(dens, xlim = c(-1,1), ylim = c(0,35), type="l", ann = FALSE,col=alpha("black",alpha = 0.75), bty = "n", yaxt = "n")
# abline(v=0, lty=2)
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.75), border =NA))
# legend("topright", "Smooth-Random", bty="n", cex=1.1) 
# 
# par(mar=c(3,2.5,0,1.5))
# 
# 
# #####
# ###
# ##
# # GP reward prediction
# ##
# ###
# ####
# 
# 
# #Group
# dens <- density(GPGroupRand)
# x1 <- min(which(dens$x >= quantile(GPGroupRand, 0)))  
# x2 <- max(which(dens$x <  quantile(GPGroupRand, 1)))
# plot(dens, xlim = c(-1,1), ylim = c(0,20), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[6],alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# 
# dens <- density(GPGroupSmooth)
# x1 <- min(which(dens$x >= quantile(GPGroupSmooth, 0)))  
# x2 <- max(which(dens$x <  quantile(GPGroupSmooth, 1)))
# par(new = TRUE)
# plot(dens, xlim = c(-1,1), ylim = c(0,20), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[1],alpha = 0.75), border = NA))
# mtext("GP reward prediction", side = 2, line = 5, cex = 1.2)
# 
# Arrows(-0.1, 19, -1, 19, arr.type="triangle", arr.width=0.2)
# Arrows(0.1, 19, 1, 19, arr.type="triangle", arr.width=0.2)
# 
# text(-0.5,18, "Against prediction")
# text(0.5,18, "Along prediction")
# 
# 
# 
# 
# par(mar=c(3,1,0,2))
# 
# dens <- density(GPGroupCont)
# x1 <- min(which(dens$x >= quantile(GPGroupCont, 0.025)))  
# x2 <- max(which(dens$x <  quantile(GPGroupCont, 0.975 )))
# plot(dens, xlim = c(-1,1), ylim = c(0,25), type="l", ann = FALSE,col=alpha("black",alpha = 0.75), bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# legend("topright", "Smooth-Random", bty="n", cex=1.1) 
# 
# 
# 
# 
# par(mar=c(3,2.5,0,1.5))
# 
# #Solo
# dens <- density(GPSoloRand)
# x1 <- min(which(dens$x >= quantile(GPSoloRand, 0)))  
# x2 <- max(which(dens$x <  quantile(GPSoloRand, 1)))
# plot(dens, xlim = c(-1,1), ylim = c(0,15), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[6],alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# 
# dens <- density(GPSoloSmooth)
# x1 <- min(which(dens$x >= quantile(GPSoloSmooth, 0)))  
# x2 <- max(which(dens$x <  quantile(GPSoloSmooth, 1)))
# par(new = TRUE)
# plot(dens, xlim = c(-1,1), ylim = c(0,15), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[1],alpha = 0.75), border = NA))
# 
# 
# Arrows(-0.1, 14, -1, 14, arr.type="triangle", arr.width=0.2)
# Arrows(0.1, 14, 1, 14, arr.type="triangle", arr.width=0.2)
# 
# text(-0.5,13.25, "Against prediction")
# text(0.5,13.25, "Along prediction")
# 
# 
# 
# par(mar= c(3,1,0,2.5))
# 
# 
# dens <- density(GPSoloCont)
# x1 <- min(which(dens$x >= quantile(GPSoloCont, 0.025)))  
# x2 <- max(which(dens$x <  quantile(GPSoloCont, 0.975 )))
# plot(dens, xlim = c(-1,1), ylim = c(0,40), type="l", ann = FALSE,col=alpha("black",alpha = 0.75), bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0),  col=alpha("black",alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# legend("topright", "Smooth-Random", bty="n", cex=1.1) 
# 
# 
# 
# par(mar=c(3,2.5,0,1.5))
# 
# 
# 
# 
# 
# 
# #####
# ###
# ##
# # Successful others
# ##
# ###
# ####
# 
# 
# #Group
# dens <- density(SuccessGroupRand)
# x1 <- min(which(dens$x >= quantile(SuccessGroupRand, 0)))  
# x2 <- max(which(dens$x <  quantile(SuccessGroupRand, 1)))
# plot(dens, xlim = c(-6,6), ylim = c(0,5), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[6],alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# 
# dens <- density(SuccessGroupSmooth)
# x1 <- min(which(dens$x >= quantile(SuccessGroupSmooth, 0)))  
# x2 <- max(which(dens$x <  quantile(SuccessGroupSmooth, 1)))
# par(new = TRUE)
# plot(dens, xlim = c(-6,6), ylim = c(0,5), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[1],alpha = 0.75), border = NA))
# mtext("Successful player proximity", side = 2, line = 5, cex = 1.1)
# 
# 
# Arrows(-0.6 , 4.5, -6 , 4.5, arr.type="triangle", arr.width=0.2)
# Arrows(0.6 , 4.5, 6 , 4.5, arr.type="triangle", arr.width=0.2)
# 
# text(-3,4.25, "Closer to successful")
# text(3.35 ,4.25, "Farther from successful")
# 
# par(mar=c(3,1,0,2))
# 
# dens <- density(SuccessGroupCont)
# x1 <- min(which(dens$x >= quantile(SuccessGroupCont, 0.025)))  
# x2 <- max(which(dens$x <  quantile(SuccessGroupCont, 0.975 )))
# plot(dens, xlim = c(-1,1), ylim = c(0,5), type="l", ann = FALSE,col=alpha("black",alpha = 0.75), bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# legend("topright", "Smooth-Random", bty="n", cex=1.1) 
# 
# 
# 
# 
# 
# par(mar=c(3,2.5,0,1.5))
# 
# plot.new()
# legend("bottomright", title = "Environment", c("Random", "Smooth"), col = c(alpha(col.pal[6],alpha = 0.75),alpha(col.pal[1],alpha = 0.75)), lwd = 10, bty="n", cex = 1.8)
# 
# 
# par(mar=c(3,2.5,0,1.5))
# 
# plot.new()
# 
# 
# 
# #####
# ###
# ##
# # Unuccessful others
# ##
# ###
# ####
# 
# 
# #Group
# dens <- density(UnsuccessGroupRand)
# x1 <- min(which(dens$x >= quantile(UnsuccessGroupRand, 0)))  
# x2 <- max(which(dens$x <  quantile(UnsuccessGroupRand, 1)))
# plot(dens, xlim = c(-3,3), ylim = c(0,6), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[6],alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# 
# dens <- density(UnsuccessGroupSmooth)
# x1 <- min(which(dens$x >= quantile(UnsuccessGroupSmooth, 0)))  
# x2 <- max(which(dens$x <  quantile(UnsuccessGroupSmooth, 1)))
# par(new = TRUE)
# plot(dens, xlim = c(-3,3), ylim = c(0,6), type="n", ann = FALSE, bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[1],alpha = 0.75), border = NA))
# mtext("Unsuccessful player proximity", side = 2, line = 5, cex = 1.1)
# 
# Arrows(-0.3 , 5.75, -3 , 5.75, arr.type="triangle", arr.width=0.2)
# Arrows(0.3 , 5.75, 3 , 5.75, arr.type="triangle", arr.width=0.2)
# 
# text(-1.75,5.3, "Closer to unsuccessful")
# text(1.65 ,5.3, "Farther from unsuccessful")
# 
# 
# 
# par(mar=c(3,1,0,2))
# 
# dens <- density(UnsuccessGroupCont)
# x1 <- min(which(dens$x >= quantile(UnsuccessGroupCont, 0.025)))  
# x2 <- max(which(dens$x <  quantile(UnsuccessGroupCont, 0.975 )))
# plot(dens, xlim = c(-1,1), ylim = c(0,10), type="l", ann = FALSE,col=alpha("black",alpha = 0.75), bty = "n", yaxt = "n")
# with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.75), border = NA))
# abline(v=0, lty=2)
# legend("topright", "Smooth-Random", bty="n", cex=1.1) 
# 
# 
# 
# 
# 
# 
# par(mar=c(3,2.5,0,1.5))
# 
# plot.new()
# legend("topright", title = "Contrast", c("Smooth-Random"), col = c(alpha("black",alpha = 0.75)), lwd = 10, bty="n", cex = 1.8)
# 
# 
# par(mar=c(3,2.5,0,1.5))
# plot.new()
# 
# 
# mtext("Features", side = 2, line = 6, cex = 1.5, outer = TRUE)
# 
# 
# 
# dev.off()
