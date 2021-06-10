#set 4 and 5 combined:::
library(devtools)
load_all()
library(epiR)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

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

prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

operational.outcome = c()
for(i in 1:length(seq.duration)){
  
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "sequence loss"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){operational.outcome[i] = "rotation loss"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){operational.outcome[i] = "mixture win"}
  if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] > 0.1 ){operational.outcome[i] = "rotation win"}
  if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] < -0.1 ){operational.outcome[i] = "sequence win"}
  if(prop.diff.seq.mix[i] < 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "no operational win"}
}


set.4.df = data.frame(seq.cross.df, outcome, operational.outcome)
set.4.df = set.4.df%>%
  dplyr::select("Heritability",
                "Male.Insecticide.Exposure",
                "Female.Insecticide.Exposure",
                "Fitness.Cost",
                "Intervention.Coverage",
                "Dispersal",
                "cross.resistance",
                "outcome",
                "operational.outcome")%>%
  dplyr::mutate(start.resistance = 0)
  
  
  
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

prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

operational.outcome = c()
for(i in 1:length(seq.duration)){
  
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "sequence loss"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){operational.outcome[i] = "rotation loss"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){operational.outcome[i] = "mixture win"}
    if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] > 0.1 ){operational.outcome[i] = "rotation win"}
    if(prop.diff.rot.mix[i] < -0.1 &
     prop.diff.seq.rot[i] < -0.1 ){operational.outcome[i] = "sequence win"}
  if(prop.diff.seq.mix[i] < 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "no operational win"}
}

set.5.df = data.frame(set.5.seqeunces,
                      outcome,
                      operational.outcome)
set.5.df = set.5.df%>%
  dplyr::select("Heritability",
                "Male.Insecticide.Exposure",
                "Female.Insecticide.Exposure",
                "Fitness.Cost",
                "Intervention.Coverage",
                "Dispersal",
                "cross.resistance",
                "outcome",
                "operational.outcome")%>%
  dplyr::mutate(start.resistance = 50)


set.4and5.df = rbind(set.4.df, set.5.df)
set.4and5.df$operational.outcome = as.factor(set.4and5.df$operational.outcome)

set.4and5.df = set.4and5.df%>%
  dplyr::rename(cross.selection = "cross.resistance")
levels(set.4and5.df$operational.outcome) = gsub(" ", "\n", levels(set.4and5.df$operational.outcome))

levels(set.4and5.df$operational.outcome)
## set the seed to make your partition reproducible; using todays date
## 75% of the sample

sample.size = floor(0.75 * nrow(set.4and5.df))

set.seed(2405)#today date
train.ind = sample(seq_len(nrow(set.4and5.df)), size = sample.size)

data.train = set.4and5.df[train.ind, ]
data.test = set.4and5.df[-train.ind, ]




fit = rpart(operational.outcome~
              Heritability+ 
              Male.Insecticide.Exposure+
              Female.Insecticide.Exposure+ 
              Fitness.Cost+ 
              Intervention.Coverage+
              Dispersal+
              start.resistance+
              cross.selection, 
            data = data.train, 
            method = 'class',
            control = rpart.control(minsplit = 525, #each split is 1% of samples
                                    maxdepth = 5,
                                    cp = 0))

predict.unseen = predict(fit, data.test, type = 'class')
actual.outcome = data.test$operational.outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  


sum(correct.outcome)/length(correct.outcome)



rpart.plot(fit,
           type = 0,
           tweak = 1,
           extra = 100,
           legend.y = "NULL",
           box.palette = list("#377eb8",
                              "#b2df8a",
                              "#ffff33"),
           fallen.leaves = TRUE,
           uniform = TRUE)










##FOR POSTER USE ONLY 50%+ coverage as this forces a decision [draws common at low coverage]

set.4and5.df.high.coverage = set.4and5.df%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

sample.size = floor(0.75 * nrow(set.4and5.df.high.coverage))

set.seed(1205)#today date
train.ind = sample(seq_len(nrow(set.4and5.df.high.coverage)), size = sample.size)

data.train = set.4and5.df.high.coverage[train.ind, ]
data.test = set.4and5.df.high.coverage[-train.ind, ]


fit = rpart(operational.outcome~
              Heritability+ 
              Male.Insecticide.Exposure+
              Female.Insecticide.Exposure+ 
              Fitness.Cost+ 
              Intervention.Coverage+
              Dispersal+
              start.resistance+
              cross.resistance, 
            data = data.train, 
            method = 'class',
            control = rpart.control(minsplit = 350, #each split is 1% of samples
                                    maxdepth = 5,
                                    cp = 0))

predict.unseen = predict(fit, data.test, type = 'class')
actual.outcome = data.test$operational.outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  


sum(correct.outcome)/length(correct.outcome)



rpart.plot(fit,
           type = 0,
           tweak = 1.2,
           extra = 100,
           legend.y = "NULL",
           box.palette = list("#377eb8",
                              "#b2df8a",
                              "#ffff33"),
           fallen.leaves = TRUE,
           uniform = TRUE)



