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
       dplyr::filter(diff.duration.mix.seq > 0))

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
  ylab("Difference in Polygenic Resistance Score")+ 
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
       dplyr::filter(diff.duration.mix.rot > 0))

#draws
nrow(diff.duration.mix.rot%>%
       dplyr::filter(diff.duration.mix.rot == 0))

##




