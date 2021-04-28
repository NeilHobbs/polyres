#Sequences Rotations Set 8
library(devtools)
load_all()
library(epiR)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

#Analysis to compare sequences and rotations in the absence of cross resistance

#Read in datasets from simulations from set 1:
sequences.set.8.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set8.csv")
rotations.set.8.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set8.csv")

sequences.rotations.set.8.df = rbind(sequences.set.8.df, rotations.set.8.df)

#Find difference in simulations durations:
sequences.duration = sequences.set.8.df$simulation.duration
rotations.duration = rotations.set.8.df$simulation.duration

#Calculate difference in duration
duration.difference = sequences.duration-rotations.duration

#Calculate proportion difference 
proportion.difference = 1 - (sequences.duration/rotations.duration)

outcome = ifelse(sequences.duration > rotations.duration,
                             yes = "Sequence.Win",
                             no = ifelse(rotations.duration > sequences.duration,
                                         yes = "Rotation.Win",
                                         no = "Draw"))

sequences.set.8.df$duration.difference = duration.difference
sequences.set.8.df$proportion.difference = proportion.difference

operational.outcome = ifelse(proportion.difference >= 0.1,
                             yes = "Rotation.Operational.Win",
                             no = ifelse(proportion.difference <= -0.1,
                                         yes = "Sequence.Operational.Win",
                                         no = "Operational.Draw"))

sequences.set.8.df$operational.outcome = operational.outcome 
table(outcome)
table(operational.outcome)


9060/45000
1087/45000
34853/45000

692/9060
222/1087



operational.outcome.df = sequences.set.8.df%>%
  dplyr::filter(operational.outcome != "Operational.Draw")

set.8.tree.fit = rpart(operational.outcome ~ 
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage,
                       data = operational.outcome.df,
                       method = "class")

rpart.plot(set.8.tree.fit,
           type = 0,
           tweak = 1.2,
           extra = 100,
           box.palette = list("#984ea3", "#ff7f00"))
