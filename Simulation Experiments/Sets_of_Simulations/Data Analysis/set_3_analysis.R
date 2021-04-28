#Read in datasets from simulations from set 1:
sequences.set.1.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set1.csv")
rotations.set.1.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set1.csv")

sequences.rotations.set.1.df = rbind(sequences.set.1.df, rotations.set.1.df)


#Read in Mixtures (set 3)::
mixtures.set.3.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/mixtures.set.3.csv")

#####performance vs sequences####
sequences.set.1.df.2insecticides = sequences.set.1.df%>%
  dplyr::filter(Number.of.Insecticides == 2)


diff.duration.mix.seq = mixtures.set.3.df$simulation.duration - sequences.set.1.df.2insecticides$simulation.duration
diff.intensity.mix.seq = mixtures.set.3.df$mean.resistance.intensity. - sequences.set.1.df.2insecticides$mean.resistance.intensity.
diff.failure.mix.seq= mixtures.set.3.df$exceedance.generations.deployed - sequences.set.1.df.2insecticides$exceedance.generations.deployed
diff.peak.mix.seq = mixtures.set.3.df$peak.resistance - sequences.set.1.df.2insecticides$peak.resistance
sim.replicate = seq(1, 15000, by = 1)

diff.mix.seq = data.frame(diff.duration.mix.seq,
                          diff.intensity.mix.seq,
                          diff.failure.mix.seq,
                          diff.peak.mix.seq,
                          sim.replicate)

#seq wins
nrow(diff.mix.seq%>%
       dplyr::filter(diff.duration.mix.seq < 0))

#mix wins
nrow(diff.mix.seq%>%
       dplyr::filter(diff.duration.mix.seq > 0))#4747

#draws
nrow(diff.mix.seq%>%
       dplyr::filter(diff.duration.mix.seq == 0))


#Sort out the draws:
draws.mix.seq = diff.mix.seq%>%
  dplyr::filter(diff.duration.mix.seq == 0)

ggplot(draws.mix.seq, aes(x = sim.replicate, y=diff.failure.mix.seq)) +
  geom_point()+ 
  geom_hline(yintercept = 0, size = 1)+
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in Number of Control Failure Generations")+ 
  ylim(-100, 100)+ #set as the maximum range
  theme_classic()

#performance vs rotations:
rotations.set.1.df.2insecticides = rotations.set.1.df%>%
  dplyr::filter(Number.of.Insecticides == 2)


diff.duration.mix.rot = mixtures.set.3.df$simulation.duration - rotations.set.1.df.2insecticides$simulation.duration
diff.duration.mix.rot = data.frame(diff.duration.mix.rot)

#rot wins
nrow(diff.duration.mix.rot%>%
       dplyr::filter(diff.duration.mix.rot < 0))

#mix wins
nrow(diff.duration.mix.rot%>%
       dplyr::filter(diff.duration.mix.rot > 0))#3466

#draws
nrow(diff.duration.mix.rot%>%
       dplyr::filter(diff.duration.mix.rot == 0))

##

seq.duration = sequences.set.1.df.2insecticides$simulation.duration
rot.duration = rotations.set.1.df.2insecticides$simulation.duration
mix.duration = mixtures.set.3.df$simulation.duration

prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

prop.diff.df = data.frame(prop.diff.seq.mix, 
                          prop.diff.seq.rot,
                          prop.diff.rot.mix)


nrow(prop.diff.df%>%
       dplyr::filter(prop.diff.seq.mix >= 0.1))#4109
4109/4747

nrow(prop.diff.df%>%
       dplyr::filter(prop.diff.rot.mix >= 0.1))#2958
2958/3466

outcome = c()
for(i in 1:length(seq.duration)){
  
  if(seq.duration[i] == rot.duration[i] & mix.duration[i]){outcome[i] = "draw"}
  if(rot.duration[i] > mix.duration[i] & seq.duration[i]){outcome[i] = "rotation win"}  
  if(seq.duration[i] > mix.duration[i] & rot.duration[i]){outcome[i] = "sequence win"}  
  if(mix.duration[i] > rot.duration[i] & seq.duration[i]){outcome[i] = "mixture win"} 
  if(mix.duration[i] == rot.duration[i] & 
     mix.duration[i] > seq.duration[i]){outcome[i] = "sequence loss"}
}


#mixture.operational.win.vs.sequence
#mixture.operational.win.vs.rotation
#mixture.operational.win
#no.operational.win

#Assign operational outcomes (e.g. win by 10% or more)
operational.outcome = c()
for(i in 1:length(seq.duration)){
  
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "sequence only"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){operational.outcome[i] = "rotation only"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){operational.outcome[i] = "rotation and sequence"}
  if(prop.diff.seq.mix[i] < 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = NA}
}

table(operational.outcome)
2958+1151

outcome.df = data.frame(seq.duration,
                        rot.duration,
                        mix.duration,
                        outcome)

mixtures.set.3.df$outcome = outcome
mixtures.set.3.df$operational.outcome = operational.outcome

set.3.tree.fit = rpart(outcome ~ 
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage,
                       data = mixtures.set.3.df,
                       minsplit = 200,
                       method = "class")

set.3.tree.fit.operational = rpart(operational.outcome ~ 
                                     Heritability +
                                     Fitness.Cost +
                                     Male.Insecticide.Exposure+
                                     Female.Insecticide.Exposure+
                                     Dispersal +
                                     Intervention.Coverage,
                                   data = mixtures.set.3.df,
                                   minsplit = 250,
                                   method = "class")

par(mfrow=c(2,1))

rpart.plot(set.3.tree.fit,
           type = 0,
           tweak = 1,
           extra = 100,
           main = "A",
           box.palette = list("#b2df8a", "#3690c0"))

rpart.plot(set.3.tree.fit.operational,
           type = 0,
           tweak = 1,
           extra = 100,
           main = "B",
           box.palette = list("#c51b8a", "#ffff33"))
par(mfrow=c(1,1))
