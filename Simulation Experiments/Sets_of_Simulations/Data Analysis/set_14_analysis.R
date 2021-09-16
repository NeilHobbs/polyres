#Data analysis for Set 14::
#In this set of simulations, we expect rotations and sequences to perform identically in terms of simulation duration.
#There may be differences in terms of mean and peak bioassay survival in the simulations that lasted 500 generations. 
library(dplyr)
library(ggplot2)

#read in the required datasets:
sequences.set.14.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set14.csv")
rotations.set.14.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set14.csv")

difference.duration = sequences.set.14.df$simulation.duration - rotations.set.14.df$simulation.duration

percentage.diff = ((sequences.set.14.df$simulation.duration - rotations.set.14.df$simulation.duration)/sequences.set.14.df$simulation.duration)*100

start.resistance = sequences.set.14.df$start.resistance

simulation = seq(1, 50000, 1)

set14.df = data.frame(difference.duration, start.resistance, simulation, percentage.diff)

ggplot(set14.df, aes(x = simulation,
                     y = difference.duration,
                     colour = start.resistance))+
  geom_point(alpha = 0.4)+
  xlab("simulation")+
  ylab("Difference in Duration (%)")+
  theme_classic()





