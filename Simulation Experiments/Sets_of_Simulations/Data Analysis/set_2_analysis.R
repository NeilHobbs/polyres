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
  dplyr::mutate(cross.resistance = 0)


rotations.zero.cross.df = rotations.set.1.df%>%
  dplyr::filter(Number.of.Insecticides == 2 & Deployment.Interval == 10)%>%
  dplyr::mutate(cross.resistance = 0)


#Get all datasets to have the same columns and column names
sequences.set.2.df = sequences.set.2.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides", "Deployment.Interval")%>%
  mutate(sim.parameter.set = rep(seq(1, 5000, 1), 6))

rotations.set.2.df = rotations.set.2.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides", "Deployment.Interval")%>%
  mutate(sim.parameter.set = rep(seq(1, 5000, 1), 6))


sequences.zero.cross.df = sequences.zero.cross.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides", "Deployment.Interval")%>%
  mutate(sim.parameter.set = seq(1, 5000, 1))


rotations.zero.cross.df = rotations.zero.cross.df%>%
  dplyr::select("simulation.duration", "mean.resistance.intensity.", "exceedance.generations", "exceedance.generations.deployed",
                "peak.resistance", "strategy", "cross.resistance", "Heritability", "Male.Insecticide.Exposure", "Female.Insecticide.Exposure",
                "Fitness.Cost", "Intervention.Coverage", "Dispersal", "Number.of.Insecticides", "Deployment.Interval")%>% 
  mutate(sim.parameter.set = seq(1, 5000, 1))

set.2.df = rbind(sequences.set.2.df, rotations.set.2.df, sequences.zero.cross.df, rotations.zero.cross.df)

make_comparison_dataframes = function(X, dataset){
  
  set.2.df.seq = dataset%>%
    dplyr::filter(strategy == "sequence")%>%
    dplyr::filter(cross.resistance == X)%>%
    dplyr::select("simulation.duration")%>%
    dplyr::rename(simulation.duration.seq = "simulation.duration")
  
  set.2.df.rot = dataset%>%
    dplyr::filter(strategy == "rotation")%>%
    dplyr::filter(cross.resistance == X)%>%
    dplyr::select("simulation.duration")%>%
    dplyr::rename(simulation.duration.rot = "simulation.duration")
  
  temp.df = cbind(set.2.df.seq, set.2.df.rot)
  
  return(temp.df)
}

set.2.df.neg0.3 = make_comparison_dataframes(-0.3, set.2.df)
set.2.df.neg.0.2 = make_comparison_dataframes(-0.2, set.2.df)
set.2.df.neg.0.1 = make_comparison_dataframes(-0.1, set.2.df)
set.2.df.0 = make_comparison_dataframes(0, set.2.df)
set.2.df.pos0.1 = make_comparison_dataframes(0.1, set.2.df)
set.2.df.pos0.2 = make_comparison_dataframes(0.2, set.2.df)
set.2.df.pos.03 = make_comparison_dataframes(0.3, set.2.df)

#Find win lose numbers between rotations and sequences:
win_lose_draw = function(dataset, X){

dataset$results = dataset$simulation.duration.rot - dataset$simulation.duration.seq

cross.resistance = X

draws = nrow(dataset%>%
  dplyr::filter(results == 0))/5000

rot.wins = nrow(dataset%>%
                  dplyr::filter(results > 0))/5000

seq.wins = nrow(dataset%>%
                  dplyr::filter(results < 0))/5000

df = cbind(draws, rot.wins, seq.wins, cross.resistance)

return(df)
}

dfneg03 = win_lose_draw(set.2.df.neg0.3, -0.3)
dfneg02 = win_lose_draw(set.2.df.neg.0.2, -0.2)
dfneg01 = win_lose_draw(set.2.df.neg.0.1, -0.1)
df0 = win_lose_draw(set.2.df.0, 0)
dfpos01 = win_lose_draw(set.2.df.pos0.1, 0.1)
dfpos02 = win_lose_draw(set.2.df.pos0.2, 0.2)
dfpos03 = win_lose_draw(set.2.df.pos.03, 0.3)

dfwinlosedraw = data.frame(rbind(dfneg03,
                      dfneg02,
                      dfneg01,
                      df0,
                      dfpos01,
                      dfpos02,
                      dfpos03))


proportions = c(dfwinlosedraw$rot.wins, dfwinlosedraw$draws, dfwinlosedraw$seq.wins)
cross.resistance = rep(seq(-0.3, 0.3, 0.1), 3)
outcomes = c(rep("rotation wins", 7), rep("draws", 7), rep("sequence wins", 7))

outcomes = factor(outcomes, levels = c("rotation wins", "draws", "sequence wins"))

df_cross = data.frame(proportions, cross.resistance, outcomes)

ggplot(df_cross, aes(x=cross.resistance, 
                     y=proportions, 
                     fill=outcomes)) + 
  geom_area(colour = "black", size = 1, alpha = 0.8)+
  scale_fill_manual(values = c("#984ea3", "#b2df8a", "#ff7f00"))+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  ylab("Proportion of Simulations")+
  xlab("Degree of Cross Resistance")+
  theme_classic()+
  theme(legend.position = "none")

#Calculate differences for draws::
cross.resistance.sequence.df = rbind(sequences.set.2.df, sequences.zero.cross.df)
cross.resistance.rotation.df = rbind(rotations.set.2.df, rotations.zero.cross.df)

difference.duration = cross.resistance.rotation.df$simulation.duration - cross.resistance.sequence.df$simulation.duration
difference.intensity = cross.resistance.rotation.df$mean.resistance.intensity. - cross.resistance.sequence.df$mean.resistance.intensity.
difference.failure = cross.resistance.rotation.df$exceedance.generations.deployed - cross.resistance.sequence.df$exceedance.generations.deployed
difference.peak = cross.resistance.rotation.df$peak.resistance - cross.resistance.sequence.df$peak.resistance
cross.resistance = cross.resistance.rotation.df$cross.resistance

cr.difference.df = data.frame(difference.duration,
                              difference.intensity,
                              difference.failure,
                              difference.peak,
                              cross.resistance)

cr.difference.df = rbind(cr.difference.df[0:15000, ], cr.difference.df[30001:35000, ], cr.difference.df[15001:30000, ])
cr.difference.df$sim.replicate = seq(1, 35000, by = 1)

#Compare the draws:
cr.draws.df = cr.difference.df%>%
  dplyr::filter(difference.duration == 0)

    #Control Failure Generations:

control.failure.label.df = data.frame(
  text.label = c("Favours Rotations", "Favours Sequences"),
  label_x_coord = c(36000, 36000), #have the label fairly central.
  label_y_coord = c(-20, 20)) #Should be far enough away to not be overlapping any bars/points

pal.2 = c("#8e0152", "#c51b7d", "#de77ae",
          "#999999", "#7fbc41", "#4d9221", "#276419")

ggplot(cr.draws.df, aes(x=sim.replicate, y=difference.failure))+
  geom_point(aes(colour = as.factor(cross.resistance)))+
  scale_color_manual(values = pal.2)+
  geom_hline(yintercept = 0, colour = "black",
             size = 1)+
  ylab("Difference in Control Failure Generations")+
  xlab("Simulation Replicate")+
  ylim(-35, 35)+
  geom_text(data = control.failure.label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
            angle = 270, size = 5)+
  guides(colour=guide_legend(title="Cross Resistance"))+
  theme_classic()+
  theme(legend.position = "bottom")


#Two Sided Partial Rank Correlation:: using epiR 
df_from_pcor_test_cr = function(CR,
                             data,
                             strategy){
  
  pcor.df = data%>%
    dplyr::filter(strategy == strategy)%>%
    dplyr::filter(cross.resistance == CR)%>%
    dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                  "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration")
  
  
  #Perform two sided partial rank correlation. 95% CIs
  pcor.output = epiR::epi.prcc(dat = pcor.df,
                               sided.test = 2,
                               conf.level = 0.95)
  
  
  estimate = pcor.output$est
  pvalue = pcor.output$p.value
  teststat = pcor.output$test.statistic
  lower.95.ci = pcor.output$lower
  upper.95.ci = pcor.output$upper
  cross.resistance = CR
  
  strategy = strategy
 
 #parameters in correct order::
  parameter = c("Insecticide Resistance Heritability", "Fitness Cost", "Male Insecticide Exposure",
                "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure")
  
  output = data.frame(estimate, lower.95.ci, upper.95.ci,  pvalue, teststat, strategy, parameter, cross.resistance)
  
  return(output)
}


pcor.seq.neg0.3 = df_from_pcor_test_cr(CR = -0.3, data = set.2.df, strategy = "sequence")
pcor.seq.neg0.2 = df_from_pcor_test_cr(CR = -0.2, data = set.2.df, strategy = "sequence")
pcor.seq.neg0.1 = df_from_pcor_test_cr(CR = -0.1, data = set.2.df, strategy = "sequence")
pcor.seq.0 = df_from_pcor_test_cr(CR = 0, data = set.2.df, strategy = "sequence")
pcor.seq.pos0.1 = df_from_pcor_test_cr(CR = 0.1, data = set.2.df, strategy = "sequence")
pcor.seq.pos0.2 = df_from_pcor_test_cr(CR = 0.2, data = set.2.df, strategy = "sequence")
pcor.seq.pos0.3 = df_from_pcor_test_cr(CR = 0.3, data = set.2.df, strategy = "sequence")

pcor.rot.neg0.3 = df_from_pcor_test_cr(CR = -0.3, data = set.2.df, strategy = "rotation")
pcor.rot.neg0.2 = df_from_pcor_test_cr(CR = -0.2, data = set.2.df, strategy = "rotation")
pcor.rot.neg0.1 = df_from_pcor_test_cr(CR = -0.1, data = set.2.df, strategy = "rotation")
pcor.rot.0 = df_from_pcor_test_cr(CR = 0, data = set.2.df, strategy = "rotation")
pcor.rot.pos0.1 = df_from_pcor_test_cr(CR = 0.1, data = set.2.df, strategy = "rotation")
pcor.rot.pos0.2 = df_from_pcor_test_cr(CR = 0.2, data = set.2.df, strategy = "rotation")
pcor.rot.pos0.3 = df_from_pcor_test_cr(CR = 0.3, data = set.2.df, strategy = "rotation")


pcor.cr = rbind(pcor.seq.neg0.3, 
                pcor.seq.neg0.2,
                pcor.seq.neg0.1,
                pcor.seq.0,
                pcor.seq.pos0.1,
                pcor.seq.pos0.2,
                pcor.seq.pos0.3,
                pcor.rot.neg0.3,
                pcor.rot.neg0.2,
                pcor.rot.neg0.1,
                pcor.rot.0,
                pcor.rot.pos0.1,
                pcor.rot.pos0.2,
                pcor.rot.pos0.3)


ggplot(pcor.cr, aes(x=estimate, y=parameter, xmin = lower.95.ci,
                        xmax = upper.95.ci, fill = as.factor(cross.resistance))) +
  geom_bar(stat = "identity", position = position_dodge(), aes(x=estimate)) +
  geom_errorbarh(position = position_dodge())+
  scale_fill_manual(values = pal.2)+
  geom_vline(xintercept = 0) +
 # xlim(-0.75, 0.75)+
  ylab("Parameter") +
  xlab("Correlation") +
  guides(fill=guide_legend(ncol=2,
                           title = "Cross Resistance")) +
  theme_bw()+
  facet_wrap(~strategy)


##Binomial GLM:
library(mgcv)
set.2.df$maximum.generations = 500

set2.gam.disperal = gam(cbind(simulation.duration, maximum.generations) ~ 
      strategy +
      Heritability +
      Fitness.Cost +
      Male.Insecticide.Exposure+
      Female.Insecticide.Exposure+
      s(Dispersal) +
      Intervention.Coverage+
      cross.resistance,
    data = set.2.df,
    family = "binomial")

plot.gam(set2.gam.disperal)#remains equally wiggly - use same splines as before.


set2.gam.coverage = gam(cbind(simulation.duration, maximum.generations) ~ 
                          strategy +
                          Heritability +
                          Fitness.Cost +
                          Male.Insecticide.Exposure+
                          Female.Insecticide.Exposure+
                          Dispersal +
                          s(Intervention.Coverage)+
                          cross.resistance,
                        data = set.2.df,
                        family = "binomial")

plot(set2.gam.coverage)

#Spline values same as used when cross resistance not present.
set.2.df$dispersal.spline1 = ifelse(set.2.df$Dispersal > 0.201, set.2.df$Dispersal-0.201, 0)
set.2.df$dispersal.spline2  = ifelse(set.2.df$Dispersal > 0.376, set.2.df$Dispersal-0.376, 0 )
set.2.df$coverage.spline = ifelse(set.2.df$Intervention.Coverage > 0.500, set.2.df$Intervention.Coverage-0.500, 0)


set.2.binomial.glm = glm(cbind(simulation.duration, maximum.generations) ~ 
                           strategy +
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
                         data = set.2.df,
                         family = "binomial")

summary(set.2.binomial.glm)
library(MASS)
confint(set.2.binomial.glm)


#Regression Trees.
seq.cross.set.2.df = set.2.df%>%
  dplyr::filter(strategy == "sequence")

rot.cross.set.2.df = set.2.df%>%
  dplyr::filter(strategy == "rotation")

seq.sim.duration = seq.cross.set.2.df$simulation.duration
rot.sim.duration = rot.cross.set.2.df$simulation.duration

outcome = c()
for(i in 1:nrow(seq.cross.set.2.df)){
  if(seq.sim.duration[i] > rot.sim.duration[i]){outcome[i] = "sequence.win"}
  if(seq.sim.duration[i] < rot.sim.duration[i]){outcome[i] = "rotation.win"}
  if(seq.sim.duration[i] == rot.sim.duration[i]){outcome[i] = "draw"}
}

outcome.df = data.frame(seq.cross.set.2.df,
                       outcome)

library(rpart)
library(rpart.plot)

set.2.tree.fit = rpart(outcome ~ 
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage+
                         cross.resistance,
                       data = outcome.df,
                       method = "class")



rpart.plot(set.2.tree.fit,
           type = 0,
           tweak = 1.2,
           extra = 100,
           box.palette = list("#b2df8a", "#984ea3", "#ff7f00"))
