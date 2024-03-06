#Plot results from agent based simulations
#Charley Wu, 2023
library('tidyverse')
library('arrow')
library('ggplot2')
library('stringr')
source('../analysis/statisticalTests.R')

#batch = 'batch1' #simple simulations
batch = 'batch2' #all relevant features set to 1

#load simulation data
simresults <- data.frame()
for (i in 1:100){
  simdat <-  read_csv(paste0('results/', batch, '/simulations.',as.character(i),'.csv'))
  simresults <- rbind(simresults, simdat)
}

#Filter to only include the final step and group rounds
fullDF <- simresults
simresults <- fullDF %>% filter(n_agents==4 & step == 400)
  
#note that mean reward and foraged blocks are summed across the number of agents, so we need to adjust
simresults$avg_reward <- simresults$tot_reward/simresults$n_agents
simresults$avg_foraged_blocks <- simresults$n_foraged_blocks/simresults$n_agents

#Refactor and rename variables
simresults$agent_type = factor(simresults$agent_type, labels  = c('Asocial', 'Social:\nUnbiased', 'Social: Success-\nbiased'))
#Group environments
simresults$env <- factor(simresults$env_file)
levels(simresults$env) <- c(rep('random',20), rep('smooth',20))

#Create plotting df

plotdf <- simresults %>% group_by(env, agent_type, step) %>% summarize(avgReward = mean(avg_reward), ssd = sd(avg_reward), count=n()) %>% 
  mutate(se = ssd / sqrt(count),
   lower_ci = avgReward - (qnorm(0.975)*se),
   upper_ci = avgReward + (qnorm(0.975)*se))


pReward <- ggplot(plotdf, aes(x = agent_type, y = avgReward, color = agent_type))+
  #geom_point(position=position_dodge(width=.7)) +
  #geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, position=position_dodge(width=.7)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5) +
  facet_grid(~env)+
  theme_classic()+
  labs(y = 'Simulated Reward', x='')+
  scale_color_manual(values = c('black', '#D81B60', '#0096FF'), name = '')+
  theme(legend.position='none', legend.justification = c(.9,.5), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank(), axis.text.x = element_text(angle=45, vjust=1, hjust=1))

pReward

ggsave('../plots/simEnv.pdf', pReward, width = 3.1, height = 2.7, units = 'in')
