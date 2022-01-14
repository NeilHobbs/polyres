#Set 32:
#Mixtures, Rotations, Sequences with Cross Selection
 #But not refugia or fitness costs.
library(ggplot2)
#read in the datasets:

mixtures.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/mixtures.set.32.csv")
rotation.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set.32.csv")
sequence.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set.32.csv")

##First compare rotations and sequences:

diff.duration.seq.rot = sequence.df$simulation.duration - rotation.df$simulation.duration
cross.selection.vals = sequence.df$cross.selection
rot.duration = rotation.df$simulation.duration
seq.duration = sequence.df$simulation.duration
diff.duration.seq.mix = sequence.df$simulation.duration - mixtures.df$simulation.duration
diff.duration.rot.mix = rotation.df$simulation.duration - mixtures.df$simulation.duration

rot.seq.df = data.frame(diff.duration.seq.rot, cross.selection.vals,
                        rot.duration, seq.duration,
                        diff.duration.seq.mix, diff.duration.rot.mix)


ggplot(rot.seq.df, aes(x=diff.duration.seq.rot,
                       fill = as.factor(cross.selection.vals)))+
  geom_histogram(binwidth = 10, colour = "black")+
    scale_fill_manual(values = c("#8e0152", "#c51b7d", "#de77ae",
                                 "#999999", "#7fbc41", "#4d9221", "#276419"))+
  facet_wrap(~cross.selection.vals)+
  xlab("Difference in Simulation Duration")+
  theme_classic()+
  theme(legend.position = "none")


ggplot(rot.seq.df, aes(x=diff.duration.seq.mix,
                       fill = as.factor(cross.selection.vals)))+
  geom_histogram(binwidth = 10, colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#c51b7d", "#de77ae",
                               "#999999", "#7fbc41", "#4d9221", "#276419"))+
  facet_wrap(~cross.selection.vals)+
  xlab("Difference in Simulation Duration")+
  theme_classic()+
  theme(legend.position = "none")

ggplot(rot.seq.df, aes(x=diff.duration.rot.mix,
                       fill = as.factor(cross.selection.vals)))+
  geom_histogram(binwidth = 10, colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#c51b7d", "#de77ae",
                               "#999999", "#7fbc41", "#4d9221", "#276419"))+
  facet_wrap(~cross.selection.vals)+
  xlab("Difference in Simulation Duration")+
  theme_classic()+
  theme(legend.position = "none")





