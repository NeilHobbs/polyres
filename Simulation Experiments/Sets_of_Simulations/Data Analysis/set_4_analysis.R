#Set 4 Analysis: Mixtures, Seqences and Rotations in the Presence of Cross Resistance (PRS starts at 0)
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
table(outcome)

24568/35000
7954/35000
92/35000
2386/35000

table(outcome, cross.resistance)
1144/5000 #mixture wins at 0 cross resistance
2249/5000 #mixture wins at 0.3 cross resistance



prop.diff.seq.mix = 1 - (seq.duration/mix.duration)
prop.diff.seq.rot = 1 - (seq.duration/rot.duration)
prop.diff.rot.mix = 1 - (rot.duration/mix.duration)

operational.outcome = c()
for(i in 1:length(seq.duration)){
  
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "mixtures win vs sequence only"}
  if(prop.diff.rot.mix[i] >= 0.1 &
     prop.diff.seq.mix[i] < 0.1 ){operational.outcome[i] = "mixtures win vs rotation only"}
  if(prop.diff.seq.mix[i] >= 0.1 &
     prop.diff.rot.mix[i] >= 0.1 ){operational.outcome[i] = "mixtures win vs rotation & sequence"}
  if(prop.diff.seq.mix[i] < 0.1 &
     prop.diff.rot.mix[i] < 0.1 ){operational.outcome[i] = "no operational win"}
}

table(operational.outcome)
6979+2136+123

6979/35000

6979/9238
2136/9238
123/9238

outcome.cross.df = data.frame(rot.duration,
                              seq.duration,
                              mix.duration,
                              cross.resistance,
                              outcome,
                              operational.outcome)


A = table(outcome.cross.df$operational.outcome,
      outcome.cross.df$outcome)

kable(A)

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
                                

outcome.df.set.4 = outcome.df

ggplot(outcome.df, aes(x=cross.resistance, 
                     y=proportion, 
                     fill=outcome)) + 
  geom_area(alpha = 0.8)+
  scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
  scale_fill_manual(values = c("#3690c0", "#b2df8a", "#ffff33", "#f03b20"))+
  ylab("Proportion of Simulations")+
  xlab("Degree of Cross Selection")+
  labs(fill = "Outcome")+
  theme_classic()

# #Used for poster presentation
# outcome.df$outcome = factor(outcome.df$Var1, levels = c("draw", "rotation.loss", "mixture.win",
#                                                         "sequence.loss"))
# 
# ggplot(outcome.df, aes(x=cross.resistance, 
#                        y=proportion, 
#                        fill=outcome)) + 
#   geom_area(alpha = 0.8)+
#   scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3))+
#   scale_fill_manual(values = c("#b2df8a","#f03b20", "#3690c0", "#ffff33"))+
#   ylab("Proportion of Simulations")+
#   xlab("Degree of Cross Selection")+
#   labs(fill = "Outcome:")+
#   theme_classic()+
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 15),
#         legend.title = element_text(size = 15),
#         axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 12,
#                                    angle = 45),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15))




outcome.df.set.4 = data.frame(seq.cross.df, outcome, operational.outcome)

outcome.df.set.4.noNA = outcome.df.set.4%>%
  dplyr::filter(!is.na(operational.outcome))%>%
  dplyr::mutate(cross.resistance = as.factor(cross.resistance))

outcome.df.set.4.noNA = data.frame(table(outcome.df.set.4.noNA$operational.outcome, outcome.df.set.4.noNA$cross.resistance))
  

ggplot(outcome.df.set.4.noNA, aes(x=Var1, y = Freq/5000))+
  geom_col(position="dodge",
           aes(fill = Var2))+
  scale_fill_manual(values = c("#8e0152", "#c51b7d", "#de77ae",
                                         "#999999", "#7fbc41", "#4d9221", "#276419"))+
  ylab("Proportion of Simulations")+
  xlab("Operationally Relevant Outcome")+
  theme_classic()+
  guides(fill=guide_legend(title="Cross Selection"))

##Used for poster
# ggplot(outcome.df.set.4.noNA, aes(x=Var1, y = Freq/5000))+
#   geom_col(position="dodge",
#            aes(fill = Var2))+
#   scale_fill_manual(values = c("#8e0152", "#c51b7d", "#de77ae",
#                                "#999999", "#7fbc41", "#4d9221", "#276419"))+
#   ylab("Proportion of Simulations")+
#   xlab("Operationally Relevant Outcome")+
#   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))+
#   theme_classic()+
#   guides(fill=guide_legend(title="Cross Selection:"))+
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 15),
#         legend.title = element_text(size = 15),
#         axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 12),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15))



set.4.tree.fit = rpart(outcome ~
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage+
                         cross.resistance,
                       method = "class",
                       data = outcome.df.set.4
                )


set.4.tree.fit.operational = rpart(operational.outcome ~
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage+
                         cross.resistance,
                       method = "class",
                       minsplit = 250,
                       data = outcome.df.set.4
)

par(mfrow = c(2,1))
rpart.plot(set.4.tree.fit,
           type = 0,
           tweak = 1,
           main="A",
           extra = 100,
           box.palette = list( "#b2df8a", "#3690c0"))


rpart.plot(set.4.tree.fit.operational,
           type = 0,
           main = "B",
           tweak = 1,
           extra = 100,
           box.palette = list("#c51b8a", "#ffff33"))

par(mfrow = c(1,1))


#Plots for poster
set.4.tree.fit.operational.poster = rpart(operational.outcome ~
                                     Heritability +
                                     Fitness.Cost +
                                     Male.Insecticide.Exposure+
                                     Female.Insecticide.Exposure+
                                     Dispersal +
                                     Intervention.Coverage+
                                     cross.resistance,
                                   method = "class",
                              control = rpart.control(maxdepth = 10,
                                                      minbucket = 525),
                                   data = outcome.df.set.4
)

rpart.plot(set.4.tree.fit.operational.poster,
           type = 0,
           tweak = 1.2,
           extra = 100,
           box.palette = list("#3690c0", "#ffff33", "#b2df8a", "#f03b20"))



temp.df = outcome.df.set.4%>%
  dplyr::select("Heritability",
                "Male.Insecticide.Exposure",
                "Female.Insecticide.Exposure",
                "Fitness.Cost",
                "Intervention.Coverage",
                "Dispersal",
                "cross.resistance",
                "operational.outcome")
## set the seed to make your partition reproducible; using todays date
## 75% of the sample size
sample.size = floor(0.75 * nrow(temp.df))

set.seed(1205)#today date
train.ind = sample(seq_len(nrow(temp.df)), size = sample.size)

data.train = temp.df[train.ind, ]
data.test = temp.df[-train.ind, ]


fit = rpart(operational.outcome~
            Heritability+ #measurable
            #Male.Insecticide.Exposure+
            Female.Insecticide.Exposure+ #measurable(?)
            Fitness.Cost+ #measurable
            #Intervention.Coverage+ 
            Dispersal+
            cross.resistance, #measurable
            data = data.train, 
            method = 'class',
           control = rpart.control(minsplit = 175, #each split is 0.5% of samples
                                   maxdepth = 5,
                                   cp = 0))

predict_unseen = predict(fit, data.test, type = 'class')

table.mat = table(data_test$operational.outcome, predict_unseen)
table.mat

accuracy.test = sum(diag(table.mat)) / sum(table.mat)
accuracy.test

rpart.plot(fit,
           type = 0,
           tweak = 1.2,
           extra = 100,
           box.palette = list("#3690c0", "#ffff33", "#b2df8a", "#f03b20"))






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
