#Data analysis for Set 10::
#In this set of simulations, we expect rotations and sequences to perform identically in terms of simulation duration.
  #There may be differences in terms of mean and peak bioassay survival in the simulations that lasted 500 generations. 
library(dplyr)

#read in the required datasets:
sequences.set.10.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set.10.csv")
rotations.set.10.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set.10.csv")

sequence.duration = sequences.set.10.df$simulation.duration    
rotation.duration = rotations.set.10.df$simulation.duration

difference.duration = sequence.duration - rotation.duration

table(difference.duration)
 #Difference is 0 for all comparisons.
sequence.resistance = sequences.set.10.df$mean.resistance.intensity.
rotation.resistance = rotations.set.10.df$mean.resistance.intensity.

difference.resistance = sequences.set.10.df$mean.resistance.intensity. - rotations.set.10.df$mean.resistance.intensity.
hist(difference.resistance)

df = data.frame(sequence.duration, rotation.duration, sequence.resistance, rotation.resistance)

#When simulations were terminated before reaching the 500 limit.
df.not.500 = df%>%
  dplyr::filter(sequence.duration != 500)

hist(df.not.500$sequence.resistance - df.not.500$rotation.resistance)


#When sequences were terminated at the 500 limit (ran to completion)
df.500 = df%>%
  dplyr::filter(sequence.duration == 500)

hist(df.500$sequence.resistance - df.500$rotation.resistance)




