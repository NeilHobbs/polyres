#Data analysis for Set 12::
#In this set of simulations, we expect rotations and sequences to perform identically in terms of simulation duration.
#There may be differences in terms of mean and peak bioassay survival in the simulations that lasted 500 generations. 
library(dplyr)
library(ggplot2)

#read in the required datasets:
sequences.set.12.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set12.csv")
rotations.set.12.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set12.csv")

difference.duration = sequences.set.12.df$simulation.duration - rotations.set.12.df$simulation.duration

percentage.diff = ((sequences.set.12.df$simulation.duration - rotations.set.12.df$simulation.duration)/sequences.set.12.df$simulation.duration)*100


return.threshold = sequences.set.12.df$return.thresh * 100
simulation = seq(1, 30000, 1)

set12.df = data.frame(difference.duration, return.threshold, simulation, percentage.diff)


pal = c("#66c2a5",
        "#fc8d62",
        "#8da0cb",
        "#e78ac3",
        "#a6d854",
        "#ffd92f")
  

ggplot(set12.df, aes(x = difference.duration,
                     fill = as.character(return.threshold)))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~return.threshold)+
  theme_classic()+
  guides(fill=guide_legend(title="Return Threshold (%)"))
  


ggplot(set12.df, aes(x = simulation,
                     y = percentage.diff,
                     colour = as.character(return.threshold)))+
  scale_colour_manual(values = pal)+
  geom_point(alpha = 0.4)+
  geom_hline(yintercept = -10, size = 2,
             colour = "black",
             alpha = 0.3)+
  geom_hline(yintercept = 10, size = 2,
             colour = "black",
             alpha = 0.3)+
  ylim(-80, 20)+
  xlab("simulation")+
  ylab("Difference in Duration (%)")+
  theme_classic()+
  theme(legend.position = "none")

set12.operational.df = set12.df%>%
  dplyr::filter(percentage.diff <= -10)

df = data.frame(table(set12.operational.df$return.threshold))




