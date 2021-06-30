###Set 13
sequences.set.13 = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set13.csv")
rotations.set.13 = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set13.csv")


duration.diff = sequences.set.13$simulation.duration - rotations.set.13$simulation.duration

range(duration.diff)

hist(duration.diff)

table(duration.diff)
4957/5000


resistance.difference = sequences.set.13$mean.resistance.intensity. - rotations.set.13$mean.resistance.intensity.

hist(resistance.difference)

peak.difference = sequences.set.13$peak.resistance - rotations.set.13$peak.resistance

operational.failure.diff = sequences.set.13$exceedance.generations.deployed - rotations.set.13$exceedance.generations.deployed

hist(operational.failure.diff)

set13 = data.frame(duration.diff, resistance.difference,
                   peak.difference, operational.failure.diff)


A = ggplot(set13, aes(x=duration.diff))+
  geom_histogram()+
  xlab("Difference in Simulation Duration")+
  ggtitle("A")+
  theme_classic()

B = ggplot(set13, aes(x=resistance.difference))+
  geom_histogram()+
  xlab("Difference in mean PRS to deployed insecticide")+
  ggtitle("B")+
  theme_classic()

C = ggplot(set13, aes(x=peak.difference))+
  geom_histogram()+
  xlab("Difference in peak PRS")+
  ggtitle("C")+
  theme_classic()

D = ggplot(set13, aes(x=operational.failure.diff))+
  geom_histogram()+
  xlab("Difference in control failure generations")+
  ggtitle("D")+
  theme_classic()


gridExtra::grid.arrange(A, B, C, D, nrow = 2, ncol = 2)


