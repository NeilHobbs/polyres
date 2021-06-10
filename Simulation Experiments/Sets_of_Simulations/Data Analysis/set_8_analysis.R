#Sequences Rotations Set 8
library(devtools)
load_all()
library(epiR)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

#Analysis to compare sequences and rotations in the absence of cross resistance

#Read in datasets from simulations from set 1:
sequences.set.8.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set8.csv")
rotations.set.8.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set8.csv")

sequences.rotations.set.8.df = rbind(sequences.set.8.df, rotations.set.8.df)

#Find difference in simulations durations:
sequences.duration = sequences.set.8.df$simulation.duration
rotations.duration = rotations.set.8.df$simulation.duration

#Calculate difference in duration
duration.difference = sequences.duration-rotations.duration

#Calculate proportion difference 
proportion.difference = 1 - (sequences.duration/rotations.duration)

outcome = ifelse(sequences.duration > rotations.duration,
                             yes = "Sequence.Win",
                             no = ifelse(rotations.duration > sequences.duration,
                                         yes = "Rotation.Win",
                                         no = "Draw"))

sequences.set.8.df$duration.difference = duration.difference
sequences.set.8.df$proportion.difference = proportion.difference

operational.outcome = ifelse(proportion.difference >= 0.1,
                             yes = "Rotation.Operational.Win",
                             no = ifelse(proportion.difference <= -0.1,
                                         yes = "Sequence.Operational.Win",
                                         no = "Operational.Draw"))

sequences.set.8.df$operational.outcome = operational.outcome 
table(outcome)
table(operational.outcome)


9060/45000
1087/45000
34853/45000

692/9060
222/1087


make_diff_duration_plot = function(){
  #Creates a dataset for putting in labels for putting in the 
  label.df = data.frame(
    text.label = c("Favours Sequences", "Favours Rotations"),
    label_x_coord = c(46000, 46000), #have the label fairly central.
    label_y_coord = c(50, -50)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq = data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(100, 100, 100)
  )
  
wins.df = sequences.set.8.df%>%
  dplyr::filter(duration.difference != 0)
 

  #Scatterplot of difference in duration (n=7033).
  figure = ggplot(wins.df, aes(x = X, y=duration.difference)) +
    geom_point(aes(colour = as.factor(insecticides.in.sim)))+ 
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    geom_hline(yintercept = 0, size = 1)+
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in Simulation Duration (Generations)")+ 
    ylim(-100, 100)+#set as the maximum range
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = 5)+
    geom_label(data = label.df.depfreq, aes(label = text.label,
                                            x=label_x_coord,
                                            y=label_y_coord),
               fill = "orchid")+
    theme_classic()+
    theme(legend.position = "none")
  
  return(figure)
}

make_diff_duration_plot()

operational.outcome.df = sequences.set.8.df%>%
  dplyr::filter(operational.outcome != "Operational.Draw")



set.8.tree.fit = rpart(operational.outcome ~ 
                         Heritability +
                         Fitness.Cost +
                         Male.Insecticide.Exposure+
                         Female.Insecticide.Exposure+
                         Dispersal +
                         Intervention.Coverage,
                       data = operational.outcome.df,
                       method = "class")

rpart.plot(set.8.tree.fit,
           type = 0,
           tweak = 1.2,
           extra = 100,
           box.palette = list("#984ea3", "#ff7f00"))



sequences.rotations.set.8.df$maximum.generations = 500
sequences.rotations.set.8.df$dispersal.spline1 = ifelse(sequences.rotations.set.8.df$Dispersal > 0.201, sequences.rotations.set.8.df$Dispersal-0.201, 0)
sequences.rotations.set.8.df$dispersal.spline2  = ifelse(sequences.rotations.set.8.df$Dispersal > 0.376, sequences.rotations.set.8.df$Dispersal-0.376, 0 )
sequences.rotations.set.8.df$coverage.spline = ifelse(sequences.rotations.set.8.df$Intervention.Coverage > 0.500, sequences.rotations.set.8.df$Intervention.Coverage-0.500, 0)

set.8.binomial.glm = glm(cbind(simulation.duration, maximum.generations) ~ 
                           strategy +
                           Heritability +
                           Fitness.Cost +
                           as.factor(Number.of.Insecticides)+
                           Male.Insecticide.Exposure+
                           Female.Insecticide.Exposure+
                           Dispersal +
                           dispersal.spline1+
                           dispersal.spline2+
                           Intervention.Coverage +
                           coverage.spline+
                           as.factor(Deployment.Interval),
                         data = sequences.rotations.set.8.df,
                         family = "binomial")

summary(set.8.binomial.glm)
library(MASS)
confint(set.8.binomial.glm)


