#Set 1::: Analysis to compare sequences and rotations in the absence of cross resistance

#SIMULATION SET 1: Comparing Sequences against Rotations:
  #Compared at 2,3 and 4 insecticides and 5, 10 and 20 deployment intervals.
  #There is no cross resistance/selection between the insecticides. 

#load in the required packages:
library(devtools)
load_all("polyres") #loads in the polyres package to allow simulation plotting examples
library(epiR) #used for partial rank correlation
library(dplyr) #used for general data manipulation
library(ggplot2) #used for data visualisation
library(rpart) #used for regression decision trees
library(rpart.plot) #used to plot the regression decision trees

#Simulation data obtained from
#Sets_of_Simulations --> Running Simulations --> set_1_rotations & set_1_sequences

#Read in datasets from simulations from set 1:
sequences.set.1.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set1.csv")
rotations.set.1.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set1.csv")

#Combine together to make a single dataframe of the set 1 simulations. 
sequences.rotations.set.1.df = rbind(sequences.set.1.df, rotations.set.1.df)

#Find difference in simulations durations:
sequences.duration = sequences.set.1.df$simulation.duration
rotations.duration = rotations.set.1.df$simulation.duration

#Calculate difference in duration sequence - rotations (negative number favours rotations, postive sequence)
duration.difference = sequences.duration-rotations.duration 

#Calculate proportion difference
proportion.difference = 1 - (sequences.duration/rotations.duration)


#Find differences in control failure generations:
difference.control.failure.generations = sequences.set.1.df$exceedance.generations.deployed - rotations.set.1.df$exceedance.generations.deployed

#Find difference in mean intensity to deployed insecticide:
difference.mean.intensity = sequences.set.1.df$mean.resistance.intensity. - rotations.set.1.df$mean.resistance.intensity.

#Find difference in peak polygenic resistance score:
difference.peak = sequences.set.1.df$peak.resistance - rotations.set.1.df$peak.resistance

#vectors of parameters:
sim.replicate = seq(1, 45000, 1)
insecticides = sequences.set.1.df$Number.of.Insecticides
deployment.interval = sequences.set.1.df$Deployment.Interval

#have a dataframe that contains only the simulation parameters:
parameters.df = rotations.set.1.df%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration")


#create data frame for comparison:
seq.rot.set.1 = data.frame(sim.replicate, insecticides, deployment.interval, duration.difference,
                           difference.mean.intensity, difference.control.failure.generations, difference.peak,
                           proportion.difference)


#Calculate wins, loses and draws:
duration.draws = nrow(seq.rot.set.1%>%
  dplyr::filter(duration.difference == 0))
print(duration.draws) #37967 / 45000

#Sequence wins
sequence.wins = nrow(seq.rot.set.1%>%
                       dplyr::filter(duration.difference > 0))
print(sequence.wins)#0 / 45000

#Rotation wins
rotation.wins = nrow(seq.rot.set.1%>%
                       dplyr::filter(duration.difference < 0))
print(rotation.wins)#7033 / 45000

#Calculate as percentages
7033/45000*100 #percentage rotation wins
37967/45000*100 #perentage draws

#Operationally relevant wins: 
#Operationally relevant wins are defined as having a simulation duration 10%+ longer than the other
operationally.relevant.wins = nrow(seq.rot.set.1%>%
                                     dplyr::filter(proportion.difference >= 0.1))
print(operationally.relevant.wins)#4292
4292/7033 #proportion operationally relevant wins for rotations.

#find the shortest simulation duration
min(sequences.rotations.set.1.df$simulation.duration)

rot.duration = rotations.set.1.df$simulation.duration
seq.duration = sequences.set.1.df$simulation.duration

outcome = c()
for(i in 1:length(rot.duration)){
  if(rot.duration[i] > seq.duration[i]){outcome[i] = "rotation.win"}
  if(seq.duration[i] > rot.duration[i]){outcome[i] = "sequence.win"}
  if(rot.duration[i] == seq.duration[i]){outcome[i] = "draw"}
}

prop.diff.seq.rot = 1 - (seq.duration/rot.duration)

operational.outcome = c()
for(i in 1:length(seq.duration)){
  if(prop.diff.seq.rot[i] > 0.1 ){operational.outcome[i] = "rotation win"}
  if(prop.diff.seq.rot[i] < -0.1 ){operational.outcome[i] = "sequence win"}
  if(prop.diff.seq.rot[i] < 0.1 ){operational.outcome[i] = "no operational win"}
}

sequences.set.1.df$outcome = outcome
sequences.set.1.df$operational.outcome = operational.outcome

#For the draws calculate the proportion differences in control failure gens, peak and mean bioassay survival.
#Find differences in control failure generations:
prop.diff.control.failure = rotations.set.1.df$exceedance.generations.deployed/sequences.set.1.df$exceedance.generations.deployed

#Find difference in mean intensity to deployed insecticide:
prop.diff.mean.intensity = rotations.set.1.df$mean.resistance.intensity./sequences.set.1.df$mean.resistance.intensity.

#Plot only for rotation wins (n=7033).
make_diff_duration_plot = function(){
#Creates a dataset for putting in labels for putting in the 
label.df = data.frame(
  text.label = c("Favours Sequences", "Favours Rotations"),
  label_x_coord = c(46000, 46000), #have the label fairly central.
  label_y_coord = c(200, -200)) #Should be far enough away to not be overlapping any bars/points

label.df.depfreq = data.frame(
  text.label =c("5 Generations", "10 Generations", "20 Generations"),
  label_x_coord = c(7500, 22500, 37500),
  label_y_coord = c(360, 360, 360)
)
rot.wins = seq.rot.set.1%>%
  dplyr::filter(duration.difference < 0)

#Scatterplot of difference in duration (n=7033).
figure = ggplot(rot.wins, aes(x = sim.replicate, y=duration.difference)) +
  geom_point(aes(colour = as.factor(insecticides)),
             alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
  geom_vline(xintercept = 15000, linetype = "dashed") +
  geom_vline(xintercept = 30000, linetype = "dashed")+
  geom_hline(yintercept = 0, size = 1)+
  xlab("Simulation Replicate Pair")+ 
  ylab("Difference in Simulation Duration (Generations)")+ 
  ylim(-360, 360)+#set as the maximum range
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

#Compare the draws (n = 37967)
make_diff_mean_intensity_plot = function(){
  #Creates a dataset for putting in labels for putting in the 
  label.df = data.frame(
    text.label = c("Favours Rotations", "Favours Sequences"),
    label_x_coord = c(46000, 46000), #have the label fairly central.
    label_y_coord = c(3, -3)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq = data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(-5, -5, -5)
  )
  
  draws = seq.rot.set.1%>%
    dplyr::filter(duration.difference == 0)%>%
    dplyr::rowwise()%>%
    mutate(diff.bioassay.survival = (resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                               mean.population.resistance = difference.mean.intensity,
                                                               michaelis.menten.slope = 1, 
                                                               half.population.bioassay.survival.resistance = 900,
                                                               sd.population.resistance = 0, 
                                                               nsim = 1)*100))
  
  #This is currently Figure 2
  #Scatterplot of difference in duration (n=16815).
  figure = ggplot(draws, aes(x = sim.replicate, y=diff.bioassay.survival)) +
    geom_point(aes(colour = as.factor(insecticides)),
               alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    geom_hline(yintercept = 0, size = 1)+
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in Mean Bioassay Survival(%)")+ 
    ylim(-6, 6)+#set as the maximum range
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
make_diff_mean_intensity_plot()

make_control_failure_plot = function(){
  #Creates a dataset for putting in labels for putting in the 
  label.df = data.frame(
    text.label = c("Favours Rotations", "Favours Sequences"),
    label_x_coord = c(46000, 46000), #have the label fairly central.
    label_y_coord = c(50, -50)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq = data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(-90, -90, -90)
  )
  
  draws = seq.rot.set.1%>%
    dplyr::filter(duration.difference == 0)
  
  #This is currently Figure 2
  #Scatterplot of difference in duration (n=16815).
  figure = ggplot(draws, aes(x = sim.replicate, y=difference.control.failure.generations)) +
    geom_point(aes(colour = as.factor(insecticides)),
               alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    geom_hline(yintercept = 0, size = 1)+
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in the number of control failure generations")+ 
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
make_control_failure_plot()

make_diff_peak_survival_plot = function(){
  #Creates a dataset for putting in labels for putting in the 
  label.df = data.frame(
    text.label = c("Favours Rotations", "Favours Sequences"),
    label_x_coord = c(46000, 46000), #have the label fairly central.
    label_y_coord = c(5, -5)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq = data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(-9, -9, -9)
  )
  
  #draws with higher peak seq
  draws = seq.rot.set.1%>%
    dplyr::filter(duration.difference == 0)%>%
    dplyr::rowwise()%>%
    mutate(diff.bioassay.survival = (resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                     mean.population.resistance = difference.peak,
                                                                     michaelis.menten.slope = 1, 
                                                                     half.population.bioassay.survival.resistance = 900,
                                                                     sd.population.resistance = 0, 
                                                                     nsim = 1)*100))
  
  
  #This is currently Figure 2
  #Scatterplot of difference in duration (n=16815).
  figure = ggplot(draws, aes(x = sim.replicate, y=diff.bioassay.survival)) +
    geom_point(aes(colour = as.factor(insecticides)),
               alpha = 0.3)+ #unclear if a barchart is the best option. As it hides the zeroes. Figure out lollipop?
    geom_vline(xintercept = 15000, linetype = "dashed") +
    geom_vline(xintercept = 30000, linetype = "dashed")+
    geom_hline(yintercept = 0, size = 1)+
    xlab("Simulation Replicate Pair")+ 
    ylab("Difference in Mean Bioassay Survival(%)")+ 
    ylim(-10, 10)+#set as the maximum range
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
make_diff_peak_survival_plot()



#Do partial rank correlation:
df_from_pcor_test = function(number.insecticides,
                             irm.strategy,
                             deploy.interval,
                             data){
  
  pcor.df = data%>%
    dplyr::filter(strategy == strategy)%>%
    dplyr::filter(insecticides.in.sim == number.insecticides)%>%
    dplyr::filter(dep.freq == deploy.interval)%>%
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
  deployment.interval = deploy.interval
  
  
  insecticides = as.factor(number.insecticides)
  strategy = irm.strategy
  IRM.Strategy = stringr::str_c(irm.strategy, " with ", number.insecticides, " insecticides")
  
  #parameters in correct order::
  parameter = c("Insecticide Resistance Heritability", "Fitness Cost", "Male Insecticide Exposure",
                "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure")
  
  output = data.frame(estimate, lower.95.ci, upper.95.ci,  pvalue, teststat, insecticides, strategy, parameter, deployment.interval, IRM.Strategy)
  
  return(output)
}

pcor.seq.2.5.df = df_from_pcor_test(2, "sequence", 5, sequences.rotations.set.1.df)
pcor.seq.3.5.df = df_from_pcor_test(3, "sequence", 5, sequences.rotations.set.1.df)
pcor.seq.4.5.df = df_from_pcor_test(4, "sequence", 5, sequences.rotations.set.1.df)
pcor.rot.2.5.df = df_from_pcor_test(2, "rotation", 5, sequences.rotations.set.1.df)
pcor.rot.3.5.df = df_from_pcor_test(3, "rotation", 5, sequences.rotations.set.1.df)
pcor.rot.4.5.df = df_from_pcor_test(4, "rotation", 5, sequences.rotations.set.1.df)

pcor.seq.2.10.df = df_from_pcor_test(2, "sequence", 10, sequences.rotations.set.1.df)
pcor.seq.3.10.df = df_from_pcor_test(3, "sequence", 10, sequences.rotations.set.1.df)
pcor.seq.4.10.df = df_from_pcor_test(4, "sequence", 10, sequences.rotations.set.1.df)
pcor.rot.2.10.df = df_from_pcor_test(2, "rotation", 10, sequences.rotations.set.1.df)
pcor.rot.3.10.df = df_from_pcor_test(3, "rotation", 10, sequences.rotations.set.1.df)
pcor.rot.4.10.df = df_from_pcor_test(4, "rotation", 10, sequences.rotations.set.1.df)

pcor.seq.2.20.df = df_from_pcor_test(2, "sequence", 20, sequences.rotations.set.1.df)
pcor.seq.3.20.df = df_from_pcor_test(3, "sequence", 20, sequences.rotations.set.1.df)
pcor.seq.4.20.df = df_from_pcor_test(4, "sequence", 20, sequences.rotations.set.1.df)
pcor.rot.2.20.df = df_from_pcor_test(2, "rotation", 20, sequences.rotations.set.1.df)
pcor.rot.3.20.df = df_from_pcor_test(3, "rotation", 20, sequences.rotations.set.1.df)
pcor.rot.4.20.df = df_from_pcor_test(4, "rotation", 20, sequences.rotations.set.1.df)

pcor.df.all = rbind(pcor.seq.2.5.df,
                    pcor.seq.3.5.df,
                    pcor.seq.4.5.df, 
                    pcor.rot.2.5.df, 
                    pcor.rot.3.5.df, 
                    pcor.rot.4.5.df,
                    pcor.seq.2.10.df,
                    pcor.seq.3.10.df,
                    pcor.seq.4.10.df, 
                    pcor.rot.2.10.df, 
                    pcor.rot.3.10.df, 
                    pcor.rot.4.10.df,
                    pcor.seq.2.20.df,
                    pcor.seq.3.20.df,
                    pcor.seq.4.20.df, 
                    pcor.rot.2.20.df, 
                    pcor.rot.3.20.df, 
                    pcor.rot.4.20.df)

#write.csv(pcor.df.all, ".//set.1.partial.rank.correlation.csv")
print(pcor.df.all)
make_pcor_plot = function(){
  
  pcor.df.all = pcor.df.all%>%
    dplyr::rename(`IRM Strategy` = IRM.Strategy)
  
  
  pals = c("#b8e186", "#7fbc41", "#4d9221", "#f1b6da", "#de77ae", "#c51b7d")
  
  
  #https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
  #Artem Sokolov
  shift_legend3 = function(p) {
    pnls = cowplot::plot_to_gtable(p) %>% 
      gtable::gtable_filter("panel") %>%
      with(setNames(grobs, layout$name)) %>% 
      purrr::keep(~identical(.x,zeroGrob()))
    
    if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
    
    lemon::reposition_legend( p, "center", panel=names(pnls) )
  }
  
  facet.labels.deployment.interval = c("5 Generation Deployment Interval",
                                       "10 Generation Deployment Interval",
                                       "20 Generation Deployment Interval")
  
  #This is FIGURE 6
  figure = shift_legend3(ggplot(pcor.df.all, aes(x=estimate, y=parameter, xmin = lower.95.ci,
                                                   xmax = upper.95.ci, fill = `IRM Strategy`)) +
                             geom_bar(stat = "identity", position = position_dodge(), aes(x=estimate)) +
                             geom_errorbarh(position = position_dodge())+
                             scale_fill_manual(values = pals)+
                             geom_vline(xintercept = 0) +
                             xlim(-0.75, 0.75)+
                             ylab("Parameter") +
                             xlab("Correlation") +
                             guides(fill=guide_legend(ncol=2)) +
                             theme_bw()+
                             facet_wrap(~deployment.interval,
                                        ncol = 2))
  return(figure)
}
make_pcor_plot()

colnames(sequences.rotations.set.1.df)

#use GAMs to check for nonlinear relationships:

sequences.rotations.set.1.df$maximum.generations = 500

gam.heritability = mgcv::gam(cbind(simulation.duration, maximum.generations) ~ 
                               strategy +
                               s(Heritability) +
                               Fitness.Cost +
                               Number.of.Insecticides+
                               Male.Insecticide.Exposure+
                               Female.Insecticide.Exposure+
                               Dispersal+
                               Intervention.Coverage +
                               Deployment.Interval,
                             data = sequences.rotations.set.1.df,
                             family = "binomial")

plot(gam.heritability)#broadly linear, as expected

gam.fitness.cost = mgcv::gam(cbind(simulation.duration, maximum.generations) ~ 
                               strategy +
                               Heritability +
                               s(Fitness.Cost) +
                               Number.of.Insecticides+
                               Male.Insecticide.Exposure+
                               Female.Insecticide.Exposure+
                               Dispersal+
                               Intervention.Coverage +
                               Deployment.Interval,
                             data = sequences.rotations.set.1.df,
                             family = "binomial")

plot(gam.fitness.cost)#wiggly but generally trending upwards

gam.male.exposure = mgcv::gam(cbind(simulation.duration, maximum.generations) ~ 
                               strategy +
                               Heritability +
                               Fitness.Cost +
                               Number.of.Insecticides+
                               s(Male.Insecticide.Exposure)+
                                Female.Insecticide.Exposure+
                               Dispersal+Intervention.Coverage +
                               Deployment.Interval,
                             data = sequences.rotations.set.1.df,
                             family = "binomial")

plot(gam.male.exposure)#linear relationship


gam.female.exposure= mgcv::gam(cbind(simulation.duration, maximum.generations) ~ 
                                 strategy +
                                 Heritability +
                                 Fitness.Cost +
                                 Number.of.Insecticides+
                                 Male.Insecticide.Exposure+
                                 s(Female.Insecticide.Exposure)+
                                 Dispersal+
                                 Intervention.Coverage +
                                 Deployment.Interval,
                               data = sequences.rotations.set.1.df,
                               family = "binomial")


plot(gam.female.exposure)#linear


gam.dispersal= mgcv::gam(cbind(simulation.duration, maximum.generations) ~ 
                                 strategy +
                                 Heritability +
                                 Fitness.Cost +
                                 Number.of.Insecticides+
                                 Male.Insecticide.Exposure+
                                 Female.Insecticide.Exposure+
                                 s(Dispersal)+
                                 Intervention.Coverage +
                                 Deployment.Interval,
                               data = sequences.rotations.set.1.df,
                               family = "binomial")


plot(gam.dispersal)#hugely wiggly
#Look to put splines in at: 0.25 and 0.4. 

gam.coverage= mgcv::gam(cbind(simulation.duration, maximum.generations) ~ 
                           strategy +
                           Heritability +
                           Fitness.Cost +
                           Number.of.Insecticides+
                           Male.Insecticide.Exposure+
                           Female.Insecticide.Exposure+
                           Dispersal+
                           s(Intervention.Coverage) +
                           Deployment.Interval,
                         data = sequences.rotations.set.1.df,
                         family = "binomial")


plot(gam.coverage)#flat line to ~0.35, followed by linear decline. 
#spline at ~0.35

#Create the splines and maximise the likelihood:


par(mfrow=c(1,2))
plot(gam.dispersal)
plot(gam.coverage)
par(mfrow=c(1,1))


find.max.logLik = function(dspline1, dspline2, cspline, data){
  
  data$disperal.spline1 = ifelse(data$Dispersal > dspline1, data$Dispersal-dspline1, 0)
  data$dispersal.spline2  = ifelse(data$Dispersal > dspline2, data$Dispersal-dspline2, 0 )
  data$coverage.spline = ifelse(data$Intervention.Coverage > cspline, data$Intervention.Coverage-cspline, 0)
  
  temp.glm = glm(cbind(simulation.duration, maximum.generations) ~ 
                    strategy +
                    Heritability +
                    Fitness.Cost +
                    Number.of.Insecticides+
                    Male.Insecticide.Exposure*
                    Female.Insecticide.Exposure+
                    Dispersal+
                    disperal.spline1+
                    dispersal.spline2+
                    Intervention.Coverage+
                    coverage.spline+
                    Deployment.Interval,
                  data = data,
                  family = "binomial")
  
  return(logLik(temp.glm))
}

find.max.logLik(dspline1 = 0.201, 
                dspline2 = 0.376, 
                cspline = 0.500, 
                data = sequences.rotations.set.1.df)

#Add the splines into the dataset:
sequences.rotations.set.1.df$dispersal.spline1 = ifelse(sequences.rotations.set.1.df$Dispersal > 0.201, sequences.rotations.set.1.df$Dispersal-0.201, 0)
sequences.rotations.set.1.df$dispersal.spline2  = ifelse(sequences.rotations.set.1.df$Dispersal > 0.376, sequences.rotations.set.1.df$Dispersal-0.376, 0 )
sequences.rotations.set.1.df$coverage.spline = ifelse(sequences.rotations.set.1.df$Intervention.Coverage > 0.500, sequences.rotations.set.1.df$Intervention.Coverage-0.500, 0)


#Run a binomial GLM:
set.1.binomial.glm = glm(cbind(simulation.duration, maximum.generations) ~ 
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
                         data = sequences.rotations.set.1.df,
                         family = "binomial")

summary(set.1.binomial.glm)
library(MASS)
confint(set.1.binomial.glm)


ggplot(sequences.rotations.set.1.df, aes(x= as.factor(dep.freq), y=exceedance.generations.deployed
                                         ))+
  geom_boxplot()+
  xlab("Deployment Frequency in Generations")+
  ylab("Control Failure Generations")+
  theme_classic()


#Understand what is going on in the sequence simulations that are 100+ generations shorter than rotations.
#I think the issue is to do with sequences actually being withdrawn (and therefore cannot be used again 
#until they reach the 5% threshold). Rotations, because they are deployed one after another, do not worry about returning
#back to 5% as all the insecticides will be expected to be withdrawn in the following deployment intervals. 
#Re-run all the simulations where the duration difference was 100+ generations but at 9% return threshold
sequences.set.1.df$rotation.duration = rotations.duration
sequences.set.1.df$duration.difference.rotation = sequences.set.1.df$simulation.duration - rotations.duration

sequences.set.1.df_short = sequences.set.1.df%>%
  dplyr::filter(duration.difference.rotation <= -100)

temp.list.sequence.hgher.return = list()
for(v in 1:nrow(sequences.set.1.df_short)){
  
  temp =  get_simulation_dataframe(run_simulation_intervention(number.of.insecticides = sequences.set.1.df_short$Number.of.Insecticides[v],
                                                               exposure.scaling.factor = 10,
                                                               nsim = 1,
                                                               minimum.insecticide.resistance.heritability = sequences.set.1.df_short$Heritability[v],
                                                               maximum.insecticide.resistance.heritability = sequences.set.1.df_short$Heritability[v],
                                                               minimum.male.insecticide.exposure = sequences.set.1.df_short$Male.Insecticide.Exposure[v],
                                                               maximum.male.insecticide.exposure = sequences.set.1.df_short$Male.Insecticide.Exposure[v],
                                                               minimum.female.insecticide.exposure = sequences.set.1.df_short$Female.Insecticide.Exposure[v],
                                                               maximum.female.insecticide.exposure = sequences.set.1.df_short$Female.Insecticide.Exposure[v],
                                                               resistance.cost = sequences.set.1.df_short$Fitness.Cost[v],
                                                               starting.treatment.site.intensity = 0,
                                                               starting.refugia.intensity = 0,
                                                               min.intervention.coverage = sequences.set.1.df_short$Intervention.Coverage[v],
                                                               max.intervention.coverage = sequences.set.1.df_short$Intervention.Coverage[v],
                                                               min.dispersal.rate = sequences.set.1.df_short$Dispersal[v],
                                                               max.dispersal.rate = sequences.set.1.df_short$Dispersal[v],
                                                               maximum.generations = 500, #appoximately 50 years
                                                               irm.strategy = "sequence", 
                                                               half.population.bioassay.survival.resistance = 900, 
                                                               withdrawal.threshold.value = 0.10, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                               return.threshold.value = 0.09, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                               deployment.frequency = sequences.set.1.df_short$Deployment.Interval[v], #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                               maximum.resistance.value = 25000 #have arbitrarily high just in case
  ) , 500, sequences.set.1.df_short$Number.of.Insecticides[v])
  
  
  sim.dur = max(temp$time.in.generations) 
  
  temp.list.sequence.hgher.return[[v]] = sim.dur
}

sequences.set.1.higherreturn = do.call(rbind, temp.list.sequence.hgher.return)

sequences.set.1.df_short$duration.update = sequences.set.1.higherreturn

sequences.set.1.df_short$difference.update = sequences.set.1.df_short$duration.update - sequences.set.1.df_short$rotation.duration
sequences.set.1.df_short$replicate = seq(1, nrow(sequences.set.1.df_short), 1)
hist(sequences.set.1.df_short$difference.update)

sequences.set.1.df_short_space = sequences.set.1.df_short%>%
  dplyr::filter(difference.update <= - 100)%>%
  dplyr::select("Heritability",
         "Dispersal",
         "Female.Insecticide.Exposure",
         "Male.Insecticide.Exposure",
         "Intervention.Coverage",
         "Fitness.Cost")
plot(sequences.set.1.df_short_space)
#High end of heritability [0.23-0.29]
#~Whole  range of Dispersal [0.2 - 0.8]
#High Female Exposure [0.75 - 0.9]
#Mid-High Male Exposure [0.65 - 0.95]
#Mid-High Intervention Coverage [0.6 - 0.8]
#high fitness cost [0.15-0.18]




example.parameters = sequences.set.1.df_short[1406 , ]
print(example.parameters)

return_thresholds_plot = function(){

Seq5 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.05,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)

Seq9 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "sequence", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.09,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)

Rot5 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "rotation", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.05,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)

Rot9 = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                               exposure.scaling.factor = 10,
                                                                               nsim = 1,
                                                                               minimum.insecticide.resistance.heritability = 0.2741695,
                                                                               maximum.insecticide.resistance.heritability = 0.2741695,
                                                                               minimum.male.insecticide.exposure = 0.7326781,
                                                                               maximum.male.insecticide.exposure = 0.7326781,
                                                                               minimum.female.insecticide.exposure = 0.7946055,
                                                                               maximum.female.insecticide.exposure = 0.7946055,
                                                                               resistance.cost = 0.1836585,
                                                                               starting.treatment.site.intensity = 0,
                                                                               starting.refugia.intensity = 0,
                                                                               min.intervention.coverage = 0.8778145,
                                                                               max.intervention.coverage = 0.8778145,
                                                                               min.dispersal.rate = 0.3401694 ,
                                                                               max.dispersal.rate = 0.3401694 ,
                                                                               maximum.generations = 500,
                                                                               irm.strategy = "rotation", 
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               withdrawal.threshold.value = 0.1, 
                                                                               return.threshold.value = 0.09,
                                                                               deployment.frequency = 5, 
                                                                               maximum.resistance.value = 25000),
                                number.of.insecticides = 3,
                                maximum.generations = 500)



plot_simulation_temp = function(simulation.dataframe, #created from the get_simulation_dataframe() function
                                half.population.bioassay.survival.resistance, #must be same as used in the simulation
                                withdrawal.threshold, #must be the same as used in the simulation
                                return.threshold,
                                plot.title){ #must be the same as used in the simulation
  
  
  #Convert the insecticide resistance intensity into bioassay survival(%).
  #This is more intuitive and operationally relevant to visualise.
  simulation.dataframe = simulation.dataframe%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                      mean.population.resistance = resistance.intensity,
                                                                      michaelis.menten.slope = 1, 
                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                      sd.population.resistance = 0, 
                                                                      nsim = 1) * 100)#As a percentage)
  #Make as separate dataframes: 1 for treatment, 1 for refugia
  temp.df.treatment = simulation.dataframe%>%
    dplyr::filter(site == "treatment")
  
  
  #Create a plot for the treatment site: this will have the deployment sequence and the threshold lines.
  treatment.plot = ggplot(data = temp.df.treatment, aes(x=time.in.generations, 
                                                        y = bioassay.survival, 
                                                        colour = insecticide.tracked,))+
    geom_point(aes(x=time.in.generations, y=(withdrawal.threshold*100), #Make a line of the deployed insecticide at %
                   colour=insecticide.deployed, #colours should match the plots
                   alpha = 0.3)) +
    geom_point(aes(x=time.in.generations, #Line indicating the return threshold.
                   y=(return.threshold * 100)), 
               colour="grey", 
               alpha = 0.3) +  #keep it fairly faint.
    geom_line(data =temp.df.treatment, aes(x=time.in.generations, 
                                           y=bioassay.survival, #aleady in %
                                           colour=insecticide.tracked))+ #matches deployed colour
    scale_y_continuous(limits = c(0, ifelse(max(simulation.dataframe$bioassay.survival) < (withdrawal.threshold*100),
                                            yes = (withdrawal.threshold*100), no = max(simulation.dataframe$bioassay.survival) + 1)),
                       breaks = c(0, (return.threshold*100), (withdrawal.threshold*100)))+ #make sure there are labels at the important bits
    xlim(0, 500)+
    ylab("Survival in Bioassay (%)") +
    xlab("Time in Generations") +
    ggtitle(plot.title)+
    theme_classic()+
    theme(legend.position = "none") #Colour label is irrelevant; insecticide number can be inferred from the order of deployment
  
  
  
  return(treatment.plot)
}

seq5.plot = plot_simulation_temp(simulation.dataframe = Seq5,
                                 half.population.bioassay.survival.resistance = 900,
                                 withdrawal.threshold = 0.1,
                                 return.threshold = 0.05,
                                 plot.title = "A")

seq9.plot = plot_simulation_temp(simulation.dataframe = Seq9,
                                 half.population.bioassay.survival.resistance = 900,
                                 withdrawal.threshold = 0.1,
                                 return.threshold = 0.09,
                                 plot.title = "B")

rot5.plot = plot_simulation_temp(simulation.dataframe = Rot5,
                                 half.population.bioassay.survival.resistance = 900,
                                 withdrawal.threshold = 0.1,
                                 return.threshold = 0.05,
                                 plot.title = "C")

rot9.plot = plot_simulation_temp(simulation.dataframe = Rot9,
                                 half.population.bioassay.survival.resistance = 900,
                                 withdrawal.threshold = 0.1,
                                 return.threshold = 0.09,
                                 plot.title = "D")


final.plot = gridExtra::grid.arrange(seq5.plot, seq9.plot,
                        rot5.plot, rot9.plot)
return(final.plot)
}
return_thresholds_plot()

#set 1::: Regression Tree::
library(rpart)
library(rpart.plot)

set.seed(1705)#today date
sample.size = floor(0.75 * nrow(sequences.set.1.df))
train.ind = sample(seq_len(nrow(sequences.set.1.df)), size = sample.size)

data.train = sequences.set.1.df[train.ind, ] #33750 rows
data.test = sequences.set.1.df[-train.ind, ]

set.1.classification.tree = rpart(outcome ~
                                    Number.of.Insecticides +
                                    Deployment.Interval +
                                    Intervention.Coverage+
                                   # Fitness.Cost+
                                   # Dispersal+
                                    Female.Insecticide.Exposure,
                                   # Male.Insecticide.Exposure,
                                  data = data.train,
                                  method = "class",
                                  control = rpart.control(minsplit = 338, #each split is 1% of samples
                                                          maxdepth = 5,
                                                          cp = 0)
                                  )

predict.unseen = predict(set.1.classification.tree, data.test, type = 'class')
actual.outcome = data.test$outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  


sum(correct.outcome)/length(correct.outcome)

rpart.plot(set.1.classification.tree, 
           type = 0,
           extra = 100)




















