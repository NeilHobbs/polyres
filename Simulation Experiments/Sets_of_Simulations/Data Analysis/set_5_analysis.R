#Set 5 Analysis

#read in datasets from set 5 simulations
set.5.seqeunces = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set.5.csv")
set.5.rotations = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set.5.csv")
set.5.mixtures = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/mixtures.set.5.csv")

#Get the vectors of the simulation durations:
seq.duration = set.5.seqeunces$simulation.duration
rot.duration = set.5.rotations$simulation.duration
mix.duration = set.5.mixtures$simulation.duration
cross.resistance = set.5.mixtures$cross.resistance

outcome = c()
for(i in 1:35000){
  if(seq.duration[i] > rot.duration[i] & mix.duration[i]){outcome[i] = "sequence win"}
  if(rot.duration[i] > seq.duration[i] & mix.duration[i]){outcome[i] = "rotation win"}
  if(mix.duration[i] > seq.duration[i] & rot.duration[i]){outcome[i] = "mixture win"}
  if(seq.duration[i] == rot.duration[i] & mix.duration[i]){outcome[i] = "draw"}
}

table(outcome, cross.resistance)





