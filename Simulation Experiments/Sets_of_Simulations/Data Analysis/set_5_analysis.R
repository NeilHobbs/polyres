#Set 5 Analysis
#Comparing Mixtures, Sequences and Rotations where insecticides had previously been deployed (PRS starts at 50).
library(devtools)
load_all()
library(epiR)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

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

table(outcome)
16569 /35000*100 #draws
16057/35000*100 #mixture win versus sequence and rotation
57/35000*100#rotation loss versus mixture and sequence
2317/35000*100 #sequence loss versus mixture and rotation


table(outcome, cross.resistance)

outcome.cross.df = data.frame(seq.duration, rot.duration, mix.duration, cross.resistance, outcome)


outcome.df = data.frame(table(outcome.cross.df$outcome, outcome.cross.df$cross.resistance))
outcome.df$proportion = outcome.df$Freq/5000
outcome.df$outcome = factor(outcome.df$Var1, levels = c("draw", "mixture.win", "rotation.loss",  
                                                        "sequence.loss"))
outcome.df$cross.resistance = c(rep(-0.3, 4),
                                rep(-0.2, 4),
                                rep(-0.1, 4),
                                rep(0, 4),
                                rep(0.1, 4),
                                rep(0.2, 4),
                                rep(0.3, 4))

outcome.df.set.5 = outcome.df

ggplot(outcome.df, aes(x=cross.resistance, 
                       y=proportion, 
                       fill=outcome)) + 
  geom_area()+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  
  scale_fill_manual(values = c("#b2df8a","#3690c0", "#f03b20",  "#ffff33"))+
  ylab("Proportion of Simulations")+
  xlab("Degree of Cross Selection")+
  labs(fill = "Outcome:")+
  guides(fill=guide_legend(nrow = 1, byrow=TRUE))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12,
                                   angle = 45),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

  
prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

range(prop.diff.seq.mix)
range(prop.diff.seq.rot)
range(prop.diff.rot.mix)

operational.outcome = c()
for(i in 1:length(seq.duration)){
  if(prop.diff.seq.mix[i] < 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "no operational win"}
    if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "sequence operational loss"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){operational.outcome[i] = "rotation operational loss"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){operational.outcome[i] = "mixture operational win"}
  
}
table(operational.outcome)

table(operational.outcome, outcome)

operational.outcome.df = data.frame(table(operational.outcome, outcome.cross.df$cross.resistance))

operational.outcome.df$proportion = operational.outcome.df$Freq/5000

operational.outcome.df$cross.resistance = c(rep(-0.3, 4),
                                            rep(-0.2, 4),
                                            rep(-0.1, 4),
                                            rep(0, 4),
                                            rep(0.1, 4),
                                            rep(0.2, 4),
                                            rep(0.3, 4))


ggplot(operational.outcome.df, aes(x=cross.resistance, 
                       y=proportion, 
                       fill=operational.outcome)) + 
  geom_area()+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  
  scale_fill_manual(values = c("#b2df8a","#3690c0", "#f03b20",  "#ffff33"))+
  ylab("Proportion of Simulations")+
  xlab("Degree of Cross Selection")+
  labs(fill = "Outcome:")+
  guides(fill=guide_legend(nrow = 1, byrow=TRUE))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12,
                                   angle = 45),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

