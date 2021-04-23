#Set 4 Analysis: Mixtures, Seqences and Rotations in the Presence of Cross Resistance.
library(devtools)
load_all()
library(epiR)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#Analysis to compare sequences and rotations in the absence of cross resistance

#Read in datasets from simulations from set 1:
sequences.set.1.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set1.csv")
rotations.set.1.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set1.csv")

#Read in datasets from simulations from set 2:
sequences.set.2.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set.2.csv")
rotations.set.2.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set.2.csv")


#Get only 10 deployment frequency, 2 insecticides from set 1 - these are the zero cross resistance simulations.
sequences.zero.cross.df = sequences.set.1.df%>%
  dplyr::filter(Number.of.Insecticides == 2 & Deployment.Interval == 10)%>%
  dplyr::mutate(cross.resistance = 0)%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides")%>%
  mutate(sim.parameter.set = seq(1, 5000, 1))


rotations.zero.cross.df = rotations.set.1.df%>%
  dplyr::filter(Number.of.Insecticides == 2 & Deployment.Interval == 10)%>%
  dplyr::mutate(cross.resistance = 0)%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides")%>%
  mutate(sim.parameter.set = seq(1, 5000, 1))

rotations.set.2.df = rotations.set.2.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides")%>%
  mutate(sim.parameter.set = rep(seq(1, 5000, 1),6))

sequences.set.2.df = sequences.set.2.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides")%>%
  mutate(sim.parameter.set = rep(seq(1, 5000, 1),6))



#Read in Mixtures:
mixtures.set.3.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/mixtures.set.3.csv")

mixtures.zero.cross.df = mixtures.set.3.df%>%
  dplyr::filter(deployment.interval == 10)%>%
  dplyr::rename("Number.of.Insecticides" = "insecticides.in.sim")%>%
  dplyr::mutate(cross.resistance = 0)%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                                                                              "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                                                                              "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides")%>%
  mutate(sim.parameter.set = seq(1, 5000, 1))

mixtures.set.4.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/mixtures.set.4.csv")

mixtures.set.4.df = mixtures.set.4.df%>%
  dplyr::rename("Number.of.Insecticides" = "insecticides.in.sim")%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides")%>%
  mutate(sim.parameter.set = rep(seq(1, 5000, 1),6))

seq.rot.mix.cross.df = rbind(rotations.zero.cross.df, 
                             rotations.set.2.df,      
                             sequences.zero.cross.df,
                             sequences.set.2.df,
                             mixtures.zero.cross.df, 
                             mixtures.set.4.df)


seq.cross.df = rbind(sequences.zero.cross.df,
                     sequences.set.2.df)

rot.cross.df = rbind(rotations.zero.cross.df, 
                     rotations.set.2.df)

mix.cross.df = rbind(mixtures.zero.cross.df,
                     mixtures.set.4.df)


rot.duration = rot.cross.df$simulation.duration
seq.duration = seq.cross.df$simulation.duration
mix.duration = mix.cross.df$simulation.duration
cross.resistance = mix.cross.df$cross.resistance
outcome = c()


for(i in 1:length(rot.duration)){
  if(rot.duration[i] > seq.duration[i] & mix.duration[i]){outcome[i] = "rotation.win"}
  if(seq.duration[i] > rot.duration[i] & mix.duration[i]){outcome[i] = "sequence.win"}
  if(mix.duration[i] > seq.duration[i] & rot.duration[i]){outcome[i] = "mixture.win"}
  if(rot.duration[i] == seq.duration[i] & mix.duration[i]){outcome[i] = "draw"}
    
}

outcome.cross.df = data.frame(rot.duration,
                              seq.duration,
                              mix.duration,
                              cross.resistance,
                              outcome)



outcome.df = data.frame(table(outcome.cross.df$outcome, outcome.cross.df$cross.resistance))
outcome.df$proportion = outcome.df$Freq/5000
cross.resistance.vec = c(rep(-0.3, 3), rep(-0.2, 3), rep(-0.1, 3),
                         rep(0, 3), rep(0.1, 3), rep(0.2, 3), rep(0.3, 3))

outcome.df$Var1 = factor(outcome.df$Var1, levels = c("mixture.win", "draw",
                         "sequence.win", "rotation.win"))
outcome.df$cross.resistance = cross.resistance.vec

ggplot(outcome.df, aes(x=cross.resistance, 
                     y=proportion, 
                     fill=Var1)) + 
  geom_area(alpha = 0.8)+
  scale_fill_manual(values = c("#3690c0", "#b2df8a", "#ff7f00"))+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  ylab("Proportion of Simulations")+
  xlab("Degree of Cross Resistance")+
  theme_classic()+
  theme(legend.position = "none")


temp.df = data.frame(seq.cross.df, outcome)

library(rpart)
library(partykit)
library(rpart.plot)

set.4.tree.fit = rpart(outcome ~
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage+
                         cross.resistance,
                       method = "class",
                       data = temp.df
                )

rpart.plot(set.4.tree.fit,
           type = 0,
           tweak = 1,
           extra = 100,
           box.palette = list("#b2df8a", "#3690c0", "#ff7f00"))

seq.rot.mix.cross.df$max.duration = 500

seq.rot.mix.cross.glm = glm(cbind(simulation.duration, max.duration)~
                              strategy+
                              Heritability +
                              Fitness.Cost +
                              Male.Insecticide.Exposure+
                              Female.Insecticide.Exposure+
                              Dispersal +
                              Intervention.Coverage+
                              cross.resistance,
                            family = "binomial",
                            data = seq.rot.mix.cross.df)

library(mgcv)
set4.gam.disperal = gam(cbind(simulation.duration, max.duration) ~ 
                          strategy +
                          Heritability +
                          Fitness.Cost +
                          Male.Insecticide.Exposure+
                          Female.Insecticide.Exposure+
                          s(Dispersal) +
                          Intervention.Coverage+
                          cross.resistance,
                        data = seq.rot.mix.cross.df,
                        family = "binomial")
plot(set4.gam.disperal) #wiggliness remains

set4.gam.coverage = gam(cbind(simulation.duration, max.duration) ~ 
                          strategy +
                          Heritability +
                          Fitness.Cost +
                          Male.Insecticide.Exposure+
                          Female.Insecticide.Exposure+
                          Dispersal +
                          s(Intervention.Coverage)+
                          cross.resistance,
                        data = seq.rot.mix.cross.df,
                        family = "binomial")
plot(set4.gam.coverage)




#Spline values same as used when cross resistance not present.
seq.rot.mix.cross.df$dispersal.spline1 = ifelse(seq.rot.mix.cross.df$Dispersal > 0.201, seq.rot.mix.cross.df$Dispersal-0.201, 0)
seq.rot.mix.cross.df$dispersal.spline2  = ifelse(seq.rot.mix.cross.df$Dispersal > 0.376, seq.rot.mix.cross.df$Dispersal-0.376, 0 )
seq.rot.mix.cross.df$coverage.spline = ifelse(seq.rot.mix.cross.df$Intervention.Coverage > 0.500, seq.rot.mix.cross.df$Intervention.Coverage-0.500, 0)

seq.rot.mix.cross.glm = glm(cbind(simulation.duration, max.duration)~
                              strategy+
                              Heritability +
                              Fitness.Cost +
                              Male.Insecticide.Exposure+
                              Female.Insecticide.Exposure+
                              Dispersal +
                              dispersal.spline1+
                              dispersal.spline2+
                              Intervention.Coverage+
                              coverage.spline+
                              cross.resistance,
                            family = "binomial",
                            data = seq.rot.mix.cross.df)

summary(seq.rot.mix.cross.glm)

library(MASS)
confint(seq.rot.mix.cross.glm)
