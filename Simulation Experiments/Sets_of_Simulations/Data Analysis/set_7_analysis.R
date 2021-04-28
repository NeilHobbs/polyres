library(devtools)
load_all()
library(epiR)
library(dplyr)
library(ggplot2)

#Analysis to compare sequences and rotations in the absence of cross resistance

#Read in datasets from simulations from set 7:
sequences.set.7.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/sequence.set7.csv")
rotations.set.7.df = read.csv("Simulation Experiments/Sets_of_Simulations/Data from Simulations/rotation.set7.csv")

sequences.rotations.set.7.df = rbind(sequences.set.7.df, rotations.set.7.df)

#Find difference in simulations durations:
sequences.duration = sequences.set.7.df$simulation.duration
rotations.duration = rotations.set.7.df$simulation.duration

#Calculate difference in duration
duration.difference = sequences.duration-rotations.duration

#Calculate proportion difference 
proportion.difference = 1 - (sequences.duration/rotations.duration)



#Find differences in control failure generations:
difference.control.failure.generations = sequences.set.7.df$exceedance.generations.deployed - rotations.set.7.df$exceedance.generations.deployed

#Find difference in mean intensity to deployed insecticide:
difference.mean.intensity = sequences.set.7.df$mean.resistance.intensity. - rotations.set.7.df$mean.resistance.intensity.

#Find difference in peak resistance intensity:
difference.peak = sequences.set.7.df$peak.resistance - rotations.set.7.df$peak.resistance

#vectors of parameters:
sim.replicate = seq(1, 45000, 1)
insecticides = sequences.set.7.df$Number.of.Insecticides
deployment.interval = sequences.set.7.df$Deployment.Interval

#parameters:
parameters.df = rotations.set.7.df%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure")

#create data frame for analysis:
seq.rot.set.7 = data.frame(sim.replicate, insecticides, deployment.interval, duration.difference,
                           difference.mean.intensity, difference.control.failure.generations, difference.peak,
                           proportion.difference, parameters.df)


#Calculate wins, loses and draws:
duration.draws = nrow(seq.rot.set.7%>%
                        dplyr::filter(duration.difference == 0))
print(duration.draws) #39510 / 45000

#Sequence wins
sequence.wins = nrow(seq.rot.set.7%>%
                       dplyr::filter(duration.difference > 0))
print(sequence.wins)#0 / 45000

#Rotation wins
rotation.wins = nrow(seq.rot.set.7%>%
                       dplyr::filter(duration.difference < 0))
print(rotation.wins)#5490 / 45000

5490/45000*100
39510/45000*100

#Operationally relevant wins:
operationally.relevant.wins = seq.rot.set.7%>%
                       dplyr::filter(proportion.difference >= 0.1)
nrow(operationally.relevant.wins)#2227
2227/5490

#4292 operationally relevant wins from set 1.
(4292 - 2227)/4292

#Plot only for rotation wins.
make_diff_duration_plot = function(){
  #Creates a dataset for putting in labels for putting in the 
  label.df = data.frame(
    text.label = c("Favours Sequences", "Favours Rotations"),
    label_x_coord = c(46000, 46000), #have the label fairly central.
    label_y_coord = c(75, -75)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.depfreq = data.frame(
    text.label =c("5 Generations", "10 Generations", "20 Generations"),
    label_x_coord = c(7500, 22500, 37500),
    label_y_coord = c(120, 120, 120)
  )
  
  rot.wins = seq.rot.set.7%>%
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
    ylim(-150, 150)+#set as the maximum range
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
  
  draws = seq.rot.set.7%>%
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
  
  draws = seq.rot.set.7%>%
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
    ylim(-110, 110)+#set as the maximum range
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
  
  draws = seq.rot.set.7%>%
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

pcor.seq.2.5.df = df_from_pcor_test(2, "sequence", 5, sequences.rotations.set.7.df)
pcor.seq.3.5.df = df_from_pcor_test(3, "sequence", 5, sequences.rotations.set.7.df)
pcor.seq.4.5.df = df_from_pcor_test(4, "sequence", 5, sequences.rotations.set.7.df)
pcor.rot.2.5.df = df_from_pcor_test(2, "rotation", 5, sequences.rotations.set.7.df)
pcor.rot.3.5.df = df_from_pcor_test(3, "rotation", 5, sequences.rotations.set.7.df)
pcor.rot.4.5.df = df_from_pcor_test(4, "rotation", 5, sequences.rotations.set.7.df)

pcor.seq.2.10.df = df_from_pcor_test(2, "sequence", 10, sequences.rotations.set.7.df)
pcor.seq.3.10.df = df_from_pcor_test(3, "sequence", 10, sequences.rotations.set.7.df)
pcor.seq.4.10.df = df_from_pcor_test(4, "sequence", 10, sequences.rotations.set.7.df)
pcor.rot.2.10.df = df_from_pcor_test(2, "rotation", 10, sequences.rotations.set.7.df)
pcor.rot.3.10.df = df_from_pcor_test(3, "rotation", 10, sequences.rotations.set.7.df)
pcor.rot.4.10.df = df_from_pcor_test(4, "rotation", 10, sequences.rotations.set.7.df)

pcor.seq.2.20.df = df_from_pcor_test(2, "sequence", 20, sequences.rotations.set.7.df)
pcor.seq.3.20.df = df_from_pcor_test(3, "sequence", 20, sequences.rotations.set.7.df)
pcor.seq.4.20.df = df_from_pcor_test(4, "sequence", 20, sequences.rotations.set.7.df)
pcor.rot.2.20.df = df_from_pcor_test(2, "rotation", 20, sequences.rotations.set.7.df)
pcor.rot.3.20.df = df_from_pcor_test(3, "rotation", 20, sequences.rotations.set.7.df)
pcor.rot.4.20.df = df_from_pcor_test(4, "rotation", 20, sequences.rotations.set.7.df)

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

#write.csv(pcor.df.all, ".//set.7.partial.rank.correlation.csv")
#print(pcor.df.all)
make_pcor_plot = function(){
  
  pcor.df.all = pcor.df.all%>%
    dplyr::rename(`IRM Strategy` = IRM.Strategy)
  
  #This is Figure 65
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

colnames(sequences.rotations.set.7.df)

#use GAMs to check for nonlinear relationships:

sequences.rotations.set.7.df$maximum.generations = 500

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
                             data = sequences.rotations.set.7.df,
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
                             data = sequences.rotations.set.7.df,
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
                              data = sequences.rotations.set.7.df,
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
                               data = sequences.rotations.set.7.df,
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
                         data = sequences.rotations.set.7.df,
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
                        data = sequences.rotations.set.7.df,
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
                data = sequences.rotations.set.7.df)

#Add the splines into the dataset:
sequences.rotations.set.7.df$dispersal.spline1 = ifelse(sequences.rotations.set.7.df$Dispersal > 0.201, sequences.rotations.set.7.df$Dispersal-0.201, 0)
sequences.rotations.set.7.df$dispersal.spline2  = ifelse(sequences.rotations.set.7.df$Dispersal > 0.376, sequences.rotations.set.7.df$Dispersal-0.376, 0 )
sequences.rotations.set.7.df$coverage.spline = ifelse(sequences.rotations.set.7.df$Intervention.Coverage > 0.500, sequences.rotations.set.7.df$Intervention.Coverage-0.500, 0)


#Run a binomial GLM:
set.7.binomial.glm = glm(cbind(simulation.duration, maximum.generations) ~ 
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
                         data = sequences.rotations.set.7.df,
                         family = "binomial")

summary(set.7.binomial.glm)
library(MASS)
confint(set.7.binomial.glm)


ggplot(sequences.rotations.set.7.df, aes(x= as.factor(dep.freq), y=exceedance.generations.deployed
))+
  geom_boxplot()+
  xlab("Deployment Frequency in Generations")+
  ylab("Control Failure Generations")+
  theme_classic()








#see what is going on in the operationally relevant wins:::

operationally.relevant.wins.4 = operationally.relevant.wins%>%
  dplyr::filter(insecticides == 4)

operationally.relevant.wins.3 = operationally.relevant.wins%>%
  dplyr::filter(insecticides == 3)

operationally.relevant.wins.2 = operationally.relevant.wins%>%
  dplyr::filter(insecticides == 2)


plot(operationally.relevant.wins[, 9:14])

av.exposure = ((operationally.relevant.wins$Male.Insecticide.Exposure * operationally.relevant.wins$Female.Insecticide.Exposure) + operationally.relevant.wins$Female.Insecticide.Exposure)/2


ggplot(operationally.relevant.wins, aes(x=Intervention.Coverage,
                                        y=Dispersal))+
  geom_point()+
  geom_smooth()

operationally.relevant.wins%>%
  dplyr::filter(sim.replicate == 5333)



find_critical_parameter_value = function(X.heritability,
                                         X.male,
                                         X.female,
                                         X.fitness,
                                         X.dispersal,
                                         X.coverage,
                                         X.dep, 
                                         start.resistance){

seq.df = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                        exposure.scaling.factor = 10,
                                                                        nsim = 1,
                                                                        minimum.insecticide.resistance.heritability = 0.2741695 + X.heritability,
                                                                        maximum.insecticide.resistance.heritability = 0.2741695 + X.heritability,
                                                                        minimum.male.insecticide.exposure = 0.7326781 + X.male,
                                                                        maximum.male.insecticide.exposure = 0.7326781 + X.male,
                                                                        minimum.female.insecticide.exposure = 0.7946055 + X.female,
                                                                        maximum.female.insecticide.exposure = 0.7946055 + X.female,
                                                                        resistance.cost = 0.1836585 + X.fitness,
                                                                        starting.treatment.site.intensity = start.resistance,
                                                                        starting.refugia.intensity = start.resistance,
                                                                        min.intervention.coverage = 0.8778145 + X.coverage,
                                                                        max.intervention.coverage = 0.8778145 + X.coverage,
                                                                        min.dispersal.rate = 0.3401694 + X.dispersal,
                                                                        max.dispersal.rate = 0.3401694 + X.dispersal,
                                                                        maximum.generations = 500,
                                                                        irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                                                                        half.population.bioassay.survival.resistance = 900,
                                                                        withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                        return.threshold.value = 0.09, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                        deployment.frequency = 5 + X.dep, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                        maximum.resistance.value = 25000),
                         maximum.generations = 500, number.of.insecticides = 3)

seq.dur = max(seq.df$time.in.generations)


rot.df = get_simulation_dataframe(simulation.array = run_simulation_intervention(number.of.insecticides = 3,
                                                                                 exposure.scaling.factor = 10,
                                                                                 nsim = 1,
                                                                                 minimum.insecticide.resistance.heritability = 0.2741695 + X.heritability,
                                                                                 maximum.insecticide.resistance.heritability = 0.2741695 + X.heritability,
                                                                                 minimum.male.insecticide.exposure = 0.7326781 + X.male,
                                                                                 maximum.male.insecticide.exposure = 0.7326781 + X.male,
                                                                                 minimum.female.insecticide.exposure = 0.7946055 + X.female,
                                                                                 maximum.female.insecticide.exposure = 0.7946055 + X.female,
                                                                                 resistance.cost = 0.1836585 + X.fitness,
                                                                                 starting.treatment.site.intensity = start.resistance,
                                                                                 starting.refugia.intensity = start.resistance,
                                                                                 min.intervention.coverage = 0.8778145 + X.coverage,
                                                                                 max.intervention.coverage = 0.8778145 + X.coverage,
                                                                                 min.dispersal.rate = 0.3401694 + X.dispersal,
                                                                                 max.dispersal.rate = 0.3401694 + X.dispersal,
                                                                                 maximum.generations = 500,
                                                                                 irm.strategy = "rotation", #will be sequence or rotation (plus mixture later on),
                                                                                 half.population.bioassay.survival.resistance = 900,
                                                                                 withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                 return.threshold.value = 0.09, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                 deployment.frequency = 5 + X.dep, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                 maximum.resistance.value = 25000),
                                  maximum.generations = 500, number.of.insecticides = 3)

# A = plot_simulation(simulation.dataframe = seq.df,
#                     half.population.bioassay.survival.resistance = 900,
#                     withdrawal.threshold = 0.1,
#                     return.threshold = 0.09)
# 
# B = plot_simulation(simulation.dataframe = rot.df,
#                     half.population.bioassay.survival.resistance = 900,
#                     withdrawal.threshold = 0.1,
#                     return.threshold = 0.09)

diff.dur = max(seq.df$time.in.generations) - max(rot.df$time.in.generations)

# the.plot = gridExtra::grid.arrange(A, B)
return(diff.dur)
}


find_critical_parameter_value(X.heritability = 0, #can reduce by -0.063 until no operational difference
                              X.male = 0,
                              X.female = 0,
                              X.fitness = 0,
                              X.dispersal = 0,
                              X.coverage = 0,
                              X.dep = 0,
                              start.resistance = 20)


