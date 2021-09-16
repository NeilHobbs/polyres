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



####Comparing Mixtures, Rotations and Sequences
rot.duration = rotations.set.1.df.2insecticides$simulation.duration
seq.duration = sequences.set.1.df.2insecticides$simulation.duration
mix.duration = mixtures.set.3.df$simulation.duration

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

sum(is.na(outcome))#all accounted for

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
sum(is.na(operational.outcome))#all accounted for


rot.peak = rotations.set.1.df.2insecticides$peak.resistance
seq.peak = sequences.set.1.df.2insecticides$peak.resistance
mix.peak = mixtures.set.3.df$peak.resistance

rot.average = rotations.set.1.df.2insecticides$mean.resistance.intensity.
seq.average = sequences.set.1.df.2insecticides$mean.resistance.intensity.
mix.average = mixtures.set.3.df$mean.resistance.intensity.

rot.peak.survival = c()
rot.mean.survival = c()
seq.peak.survival = c()
seq.mean.survival = c()
mix.peak.survival = c()
mix.mean.survival = c()

for(i in 1:15000){
  
  rot.peak.survival[i] = round((resistance_to_bioassay_survival(mean.population.resistance = rot.peak[i],
                                                                sd.population.resistance = 0, #measured without error
                                                                nsim = 1,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1)*100), #convert to percentage
                               0)  
  
  rot.mean.survival[i] = round((resistance_to_bioassay_survival(mean.population.resistance = rot.average[i],
                                                                sd.population.resistance = 0, #measured without error
                                                                nsim = 1,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1)*100), #convert to percentage
                               0)
  
  seq.peak.survival[i] = round((resistance_to_bioassay_survival(mean.population.resistance = seq.peak[i],
                                                                sd.population.resistance = 0, #measured without error
                                                                nsim = 1,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1)*100), #convert to percentage
                               0)
  
  
  
  seq.mean.survival[i] = round((resistance_to_bioassay_survival(mean.population.resistance = seq.average[i],
                                                                sd.population.resistance = 0, #measured without error
                                                                nsim = 1,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1)*100), #convert to percentage
                               0)
  
  mix.peak.survival[i] = round((resistance_to_bioassay_survival(mean.population.resistance = mix.peak[i],
                                                                sd.population.resistance = 0, #measured without error
                                                                nsim = 1,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1)*100), #convert to percentage
                               0)
  
  
  
  mix.mean.survival[i] = round((resistance_to_bioassay_survival(mean.population.resistance = mix.average[i],
                                                                sd.population.resistance = 0, #measured without error
                                                                nsim = 1,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1)*100), #convert to percentage
                               0)
}



secondary.outcome.peak = c()
for(i in 1:15000){
  if(rot.peak.survival[i] < seq.peak.survival[i] &
     rot.peak.survival[i] < mix.peak.survival[i]){secondary.outcome.peak[i] = "rotation.win"}
  if(seq.peak.survival[i] < rot.peak.survival[i] & 
     seq.peak.survival[i] < mix.peak.survival[i]){secondary.outcome.peak[i] = "sequence.win"}
  if(mix.peak.survival[i] < seq.peak.survival[i] &
     mix.peak.survival[i] < rot.peak.survival[i]){secondary.outcome.peak[i] = "mixture.win"}
  if(rot.peak.survival[i] == seq.peak.survival[i] & 
     rot.peak.survival[i] == mix.peak.survival[i]){secondary.outcome.peak[i] = "draw"}
  if(rot.peak.survival[i] < seq.peak.survival[i] & 
     rot.peak.survival[i] == mix.peak.survival[i]){secondary.outcome.peak[i] = "sequence.loss"}
  if(seq.peak.survival[i] < rot.peak.survival[i] & 
     seq.peak.survival[i] == mix.peak.survival[i]){secondary.outcome.peak[i] = "rotation.loss"}
  if(mix.peak.survival[i] > rot.peak.survival[i]&
     mix.peak.survival[i] > seq.peak.survival[i]){secondary.outcome.peak[i] = "mixture.loss"}
}

#Check for any NAs: if sums to 15000 then all values were assigned
sum(!is.na(secondary.outcome.peak))

secondary.outcome.average = c()
for(i in 1:15000){
  if(rot.mean.survival[i] < seq.mean.survival[i] &
     rot.mean.survival[i] < mix.mean.survival[i]){secondary.outcome.average[i] = "rotation.win"}
  
  if(seq.mean.survival[i] < rot.mean.survival[i] & 
     seq.mean.survival[i] < mix.mean.survival[i]){secondary.outcome.average[i] = "sequence.win"}
  
  if(mix.mean.survival[i] < seq.mean.survival[i] &
     mix.mean.survival[i] < rot.mean.survival[i]){secondary.outcome.average[i] = "mixture.win"}
  
  if(rot.mean.survival[i] == seq.mean.survival[i] & 
     rot.mean.survival[i] == mix.mean.survival[i]){secondary.outcome.average[i] = "draw"}
  
  if(rot.mean.survival[i] < seq.mean.survival[i] & 
     rot.mean.survival[i] == mix.mean.survival[i]){secondary.outcome.average[i] = "sequence.loss"}
  
  if(seq.mean.survival[i] < rot.mean.survival[i] & 
     seq.mean.survival[i] == mix.mean.survival[i]){secondary.outcome.average[i] = "rotation.loss"}
  
  if(mix.mean.survival[i] > rot.mean.survival[i]&
     mix.mean.survival[i] > seq.mean.survival[i]){secondary.outcome.average[i] = "mixture.loss"}
}
#check for NAs, if sums to 15000 all values assigned
sum(!is.na(secondary.outcome.average)) #all accounted for


parameter.space.df = sequences.set.1.df.2insecticides%>%
  dplyr::select("Deployment.Interval", "Male.Insecticide.Exposure",
                "Heritability", "Female.Insecticide.Exposure", "Fitness.Cost",
                "Dispersal", "Intervention.Coverage")

outcome.df = data.frame(parameter.space.df, outcome, operational.outcome,
                        secondary.outcome.average, secondary.outcome.peak)


table(outcome.df$outcome)
#as percentage
table(outcome.df$outcome)/15000 * 100


table(outcome.df$operational.outcome)
#as percentange:
table(outcome.df$operational.outcome)/110000 * 100



#compare draws on secondary outcomes: can only compare secondary outcomes where duration is identical
draw.outcome.df = outcome.df%>%
  dplyr::filter(outcome == "draw")

table(draw.outcome.df$secondary.outcome.peak)



prop.diff.seq.mix = 1 - (sequences.set.1.df.2insecticides$simulation.duration/mixtures.set.3.df$simulation.duration)
prop.diff.seq.rot = 1 - (sequences.set.1.df.2insecticides$simulation.duration/rotations.set.1.df.2insecticides$simulation.duration)
prop.diff.rot.mix = 1 - (rotations.set.1.df.2insecticides$simulation.duration/mixtures.set.3.df$simulation.duration)
replicate = seq(1, 15000, 1)
Deployment.Interval = sequences.set.1.df.2insecticides$Deployment.Interval


comparison.df = data.frame(prop.diff.rot.mix,
                           prop.diff.seq.mix,
                           prop.diff.seq.rot,
                           replicate,
                           Deployment.Interval)

compare_rot_mix_plot = function(label.text.size = 5){
  
  pals = c("#fdb462",
           "#8dd3c7",
           "#fb8072")
  
  label.df = data.frame(
    text.label = c("Mixtures", "Rotations"),
    label_x_coord = c(16000, 16000), #have the label fairly central.
    label_y_coord = c(75, -55)) #Should be far enough away to not be overlapping any bars/points
  
  final.plot = ggplot(comparison.df, aes(x=replicate,
                                         y=prop.diff.rot.mix*100))+
    geom_point(aes(colour = as.factor(Deployment.Interval)),
               alpha = 0.8)+
    scale_colour_manual(values = pals)+
    geom_hline(yintercept = 0)+
    xlab("Simulation Replicate")+
    ylab("Percentage Difference Duration")+
    ylim(-100, 100)+
    xlim(0, 16000)+
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = label.text.size)+
    ggtitle("B: Rotations vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  return(final.plot)
}

compare_seq_mix_plot = function(label.text.size = 5){
  
  pals = c("#fdb462",
           "#8dd3c7",
           "#fb8072")
  
  label.df = data.frame(
    text.label = c("Mixtures", "Sequences"),
    label_x_coord = c(16000, 16000), #have the label fairly central.
    label_y_coord = c(75, -55)) #Should be far enough away to not be overlapping any bars/points

  
  final.plot = ggplot(comparison.df, aes(x=replicate,
                                         y=prop.diff.seq.mix*100))+
    geom_point(aes(colour = as.factor(Deployment.Interval)),
               alpha = 0.8)+
    scale_colour_manual(values = pals)+
    geom_hline(yintercept = 0)+
    xlab("Simulation Replicate")+
    ylab("Percentage Difference Duration")+
    ylim(-100, 100)+
    xlim(0, 16000)+
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = label.text.size)+
    ggtitle("A: Sequences vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  return(final.plot)
}

make_figure = function(label.text.size){
  
  plot.A = compare_seq_mix_plot(label.text.size = label.text.size)
  plot.B = compare_rot_mix_plot(label.text.size = label.text.size)

  layout = "
  AAAAAA
  BBBBBB"
  
  E = patchwork::wrap_plots(A = plot.A,
                            B = plot.B,
                            design = layout)
  
  
  return(E)
}

make_figure(label.text.size = 3)








