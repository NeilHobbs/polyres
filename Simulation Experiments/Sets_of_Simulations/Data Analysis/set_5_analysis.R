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
  if(rot.duration[i] > seq.duration[i] &
     rot.duration[i] > mix.duration[i]){outcome[i] = "rotation.win"}
  if(seq.duration[i] > rot.duration[i] & 
     seq.duration[i] > mix.duration[i]){outcome[i] = "sequence.win"}
  if(mix.duration[i] > seq.duration[i] &
     mix.duration[i] > rot.duration[i]){outcome[i] = "mixture.win"}
  if(rot.duration[i] == seq.duration[i] & 
     rot.duration[i] == mix.duration[i]){outcome[i] = "draw"}
  if(rot.duration[i] > seq.duration[i] & 
     rot.duration[i] == mix.duration[i]){outcome[i] = "sequence.loss"}
  if(seq.duration[i] > rot.duration[i] & 
     seq.duration[i] == mix.duration[i]){outcome[i] = "rotation.loss"}
}

table(outcome, cross.resistance)

outcome.df = data.frame(table(outcome.cross.df$outcome, outcome.cross.df$cross.resistance))
outcome.df$proportion = outcome.df$Freq/5000
outcome.df$outcome = factor(outcome.df$Var1, levels = c("mixture.win", "draw",
                                                        "sequence.loss", "rotation.loss"))
outcome.df$cross.resistance = c(rep(-0.3, 4),
                                rep(-0.2, 4),
                                rep(-0.1, 4),
                                rep(0, 4),
                                rep(0.1, 4),
                                rep(0.2, 4),
                                rep(0.3, 4))


ggplot(outcome.df, aes(x=cross.resistance, 
                       y=proportion, 
                       fill=outcome)) + 
  geom_area(alpha = 0.8)+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  
  scale_fill_manual(values = c("#3690c0", "#b2df8a", "#ffff33", "#f03b20"))+
  ylab("Proportion of Simulations")+
  xlab("Degree of Cross Resistance")+
  labs(fill = "Outcome")+
  theme_classic()

  
prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

range(prop.diff.seq.mix)
range(prop.diff.seq.rot)
range(prop.diff.rot.mix)

operational.outcome = c()
for(i in 1:length(seq.duration)){
  
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "mixture vs sequence only"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){operational.outcome[i] = "mixture vs rotation only"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){operational.outcome[i] = "mixture vs rotation and sequence"}
  
  if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] > 0.1 ){operational.outcome[i] = "rotation vs mixture and sequence"}
  
  if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] < -0.1 ){operational.outcome[i] = "sequence vs mixture and rotation"}
}


table(operational.outcome, cross.resistance)



