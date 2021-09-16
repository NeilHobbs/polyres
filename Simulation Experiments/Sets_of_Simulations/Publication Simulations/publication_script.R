#Document for the running of simulations and data analysis used for publication:

#install required packages if necessary

#load required packages : packages can be installed: install.packages("packagename")
library(magrittr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
library(epiR)
library(mgcv)
library(mass)
library(rpart)
library(rpart.plot)
library(devtools) #for polyres package [will need to figure out how to allow install from github]
load_all()

############################################
## 1. Setting Up and Running Simulations  ##
############################################


# ##Step 1: Create a data frame for the parameter space: ##hashed out to prevent re-sampling
# 
# parameter.space.df = data.frame(lhs::randomLHS(5000, 6)) #20,000 random samples of the 6 input parameters.
# 
# 
# #Rename and sample from the uniform distributions
# parameter.space.df = parameter.space.df%>%
#   dplyr::rename(Heritability = X1)%>%
#   dplyr::rename(`Male Insecticide Exposure` = X2)%>%
#   dplyr::rename(`Female Insecticide Exposure` = X3)%>%
#   dplyr::rename(`Fitness Cost` = X4)%>%
#   dplyr::rename(`Intervention Coverage` = X5)%>%
#   dplyr::rename(Dispersal = X6)%>%
#   dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))%>%
#   dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))%>% #
#   dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))%>% #Defaults from Ian
#   dplyr::mutate(`Fitness Cost` = qunif(`Fitness Cost`, 0.01, 0.2))%>%
#   dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.1, 0.9))%>%
#   dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))
# 
# #use cross selection values of -0.5 to 0.5 (by 0.1)
# cross.selection.values = seq(-0.5, 0.5, 0.1)
# cross.selection.values = rep(cross.selection.values, 5000)
# cross.selection.values = sort(cross.selection.values, decreasing = TRUE)
# start.resistance.values = c(rep(0, 55000), rep(50, 55000))
# cross.selection.values = c(cross.selection.values, cross.selection.values)
# 
# parameter.space.df = do.call("rbind", replicate(22, parameter.space.df, simplify = FALSE))
# parameter.space.df$cross.selection.values = cross.selection.values
# parameter.space.df$start.resistance.values = start.resistance.values
# 
# 
# write.csv(parameter.space.df, "paramater.space.df.publication.csv")
# 
# #read in the dataset
# #parameter.space.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")
# 
# 
# #When running simulations we extract the:
#     # -Primary Outcome = Simulation Duration
#     # -Secondary Outcome = Mean resisance to deployed insecticide
#     # -Secondary Outcome = Control failure generations (deployed and not deployed)
#     # -Secondary Outcome = Peak Resistance.
# 
# 
# #Do the sequence runs: run in chunks of 40,000
# 
# 
# temp.list.sequence = list()
# 
# for(v in 1:nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "sequence",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   strategy = "sequence"
# 
#   dep.freq = c(10)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "sequence"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
#                       cross.selection, start.resistance)
# 
#   temp.list.sequence[[v]] = temp_2
# }
# 
# sequence.df = do.call(rbind, temp.list.sequence)
# sequence.df.publication = cbind(sequence.df, parameter.space.df)
# 
# write.csv(sequence.df.publication, ".//sequence.set.publication.csv")
# 
# 
# #Do the Rotation Runs
# temp.list.rotation = list()
# for(v in 1: nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
#                                                                                exposure.scaling.factor = 10,
#                                                                                nsim = 1,
#                                                                                minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                maximum.generations = 500, #appoximately 50 years
#                                                                                irm.strategy = "rotation",
#                                                                                half.population.bioassay.survival.resistance = 900,
#                                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                maximum.resistance.value = 25000, #have arbitrarily high just in case
#                                                                                min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                    maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   dep.freq = c(10)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity = c(temp_treatment%>%
#                                      dplyr::filter(insecticide.deployed == insecticide.tracked)%>%
#                                      summarise(mean(resistance.intensity)))
# 
#   strategy = "rotation"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq,
#                       cross.selection, start.resistance)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = nrow(parameter.space.df))
# 
#   temp.list.rotation[[v]] = temp_2
# }
# 
# rotation.df = do.call(rbind, temp.list.rotation)
# rotation.df.publication = cbind(rotation.df, parameter.space.df)
# 
# write.csv(rotation.df.publication, ".//rotation.set.publication.csv")
# 
# 
# #Then do mixtures::
# temp.list.mixtures = list()
# for(v in 1:nrow(parameter.space.df)){
# 
#   temp =  get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
#                                                                                                       exposure.scaling.factor = 10,
#                                                                                                       nsim = 1,
#                                                                                                       minimum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                                       maximum.insecticide.resistance.heritability = parameter.space.df$Heritability[v],
#                                                                                                       minimum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                                       maximum.male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                                                                                                       minimum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                                       maximum.female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                                                                                                       resistance.cost = parameter.space.df$Fitness.Cost[v],
#                                                                                                       starting.treatment.site.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                                       starting.refugia.intensity = parameter.space.df$start.resistance.values[v],
#                                                                                                       min.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                                       max.intervention.coverage = parameter.space.df$Intervention.Coverage[v],
#                                                                                                       min.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                                       max.dispersal.rate = parameter.space.df$Dispersal[v],
#                                                                                                       maximum.generations = 500,
#                                                                                                       irm.deployment.strategy = "mixtures", #single, mixtures
#                                                                                                       mixture.strategy = "mix.sequential.discrete", #can be: random.mixtures; pyrethroid.plus; mix.sequential.continous; mix.sequential.discrete
#                                                                                                       irm.switch.strategy = "sequence", #will be sequence or rotation;default should be sequence
#                                                                                                       half.population.bioassay.survival.resistance = 900,
#                                                                                                       withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                                                                       return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                                                                       deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                                                                       maximum.resistance.value = 25000,
#                                                                                                       conversion.factor = 0.48, #point estimates from linear regression
#                                                                                                       intercept = 0.15, #point estimates from linear regression
#                                                                                                       min.cross.selection = parameter.space.df$cross.selection.values[v],
#                                                                                                       max.cross.selection = parameter.space.df$cross.selection.values[v]),
#                                             maximum.generations = 500, number.of.insecticides = 2)
# 
# 
#   temp_treatment = temp%>% #only need to know for the treatment site as this is where the decisions are made from.
#     dplyr::filter(site == "treatment")
# 
#   simulation.duration = max(temp_treatment$time.in.generations) #Duration of simulation
# 
#   exceedance.generations = nrow(temp_treatment%>%
#                                   dplyr::filter(resistance.intensity > 100))#The number of generations where resistance.intensity > withdrawal threshold regardless of whether the insecticide is in deployment
# 
#   #note, as in mixture will be the same for mixture part 1 and mixture part 2 - only count based on part1.
#   exceedance.generations.deployed = nrow(temp_treatment%>% #The number of generations where resistance.intensity > withdrawal threshold when the insecticide is in deployment
#                                            dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                            dplyr::filter(resistance.intensity > 100))
# 
#   peak.resistance = max(temp_treatment$resistance.intensity)
# 
#   insecticides.in.sim = c(2)
# 
#   dep.freq = c(10)
# 
#   cross.selection = parameter.space.df$cross.selection.values[v]
# 
#   start.resistance = parameter.space.df$start.resistance.values[v]
# 
#   average.resistance.intensity.1 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.1 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   average.resistance.intensity.2 = c(temp_treatment%>%
#                                        dplyr::filter(deployed.mixture.part.2 == insecticide.tracked)%>%
#                                        summarise(mean(resistance.intensity)))
# 
#   strategy = "mixture"
# 
#   temp_2 = data.frame(simulation.duration, average.resistance.intensity.1, average.resistance.intensity.2, exceedance.generations,
#                       exceedance.generations.deployed, peak.resistance, strategy, insecticides.in.sim, dep.freq, cross.selection, start.resistance)
# 
#   svMisc::progress(value = v, progress.bar = TRUE, console = TRUE,
#                    max.value = nrow(parameter.space.df))
# 
#   temp.list.mixtures[[v]] = temp_2
# }
# 
# mixture.df = do.call(rbind, temp.list.mixtures)
# mixture.df.publication = cbind(mixture.df, parameter.space.df)
# 
# write.csv(mixture.df.publication, ".//mixture.df.publication.csv")


    ##The simulation running has been been deliberately hashed out. To run highlight lines 23 to 303.
      #then press crtl+shft+c and this should unhash all those lines. 
      #The simulations can take a long time time to run. We therefore letting each 
      #strategy run separately after each has finished. Depending on your computer,
      #it may be worth running each for loop in chunks of say 50000. 
      


########################################
## 2. DATA VISUALISATION              ##
########################################

#Make plot of the polygenic resistance score (and its conversion to field survival)

polygenic.resistance.values = seq(0, 900, by = 0.1)
bioassay.values = c()
for(i in 1:length(polygenic.resistance.values)){
bioassay.values[i] = resistance_to_bioassay_survival(mean.population.resistance = polygenic.resistance.values[i],
                                                     nsim = 1,
                                                     sd.population.resistance = 0, #measure without error,
                                                     michaelis.menten.slope = 1,
                                                     maximum.bioassay.survival.proportion = 1)
}

field.survival = c()
for(i in 1:length(bioassay.values)){
  
  field.survival[i] = convert_bioassay_survival_to_field(bioassay.survival = bioassay.values[i],
                                                         conversion.factor = 0.48,
                                                         intercept = 0.15)
  
}




example.df = data.frame(polygenic.resistance.values, bioassay.values, field.survival)


ggplot(example.df, aes(x=polygenic.resistance.values,
                       y = bioassay.values))+
  geom_line(aes(x=polygenic.resistance.values,
                 y=bioassay.values*100),
             colour = "black",
            size = 3,
            alpha = 0.7)+
  geom_line(aes(x=polygenic.resistance.values,
                 y=field.survival*100),
             colour = "green",
            size = 3,
            alpha = 0.7)+
  geom_segment(aes(x=0, y=10, xend=100, yend=10),
               linetype = "dashed",
               size = 1.5,
               colour = "blue")+
  geom_segment(aes(x=100, y=0, xend=100, yend=10),
               linetype = "dashed",
               size = 1.5,
               colour = "blue")+
  geom_segment(aes(x=0, y=50, xend=900, yend=50),
               linetype = "dashed",
               size = 1.5,
               colour = "orange")+
  geom_segment(aes(x=900, y=0, xend=900, yend=50),
               linetype = "dashed",
               size = 1.5,
               colour = "orange")+
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 900))+
  xlab("Mean Population Polygenic Resistance Score")+
  ylab("Survival (%)")+
  theme_classic()+
  theme(text = element_text(size=20))


## Read in the data sets
sequence.set = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/sequence.set.publication.csv")
rotation.set = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/rotation.set.publication.csv")
mixture.set = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/mixture.df.publication.csv")
parameter.space.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")


prop.diff.seq.mix = 1 - (sequence.set$simulation.duration/mixture.set$simulation.duration)
prop.diff.seq.rot = 1 - (sequence.set$simulation.duration/rotation.set$simulation.duration)
prop.diff.rot.mix = 1 - (rotation.set$simulation.duration/mixture.set$simulation.duration)
replicate = seq(1, 110000, 1)
cross.selection = sequence.set$cross.selection.values
start.resistance = sequence.set$start.resistance

comparison.df = data.frame(prop.diff.rot.mix,
                           prop.diff.seq.mix,
                           prop.diff.seq.rot,
                           replicate,
                           cross.selection,
                           start.resistance)

compare_rot_mix_plot = function(label.text.size = 5,
                                annotation.size = 2){
  
  pals = c("#a50026",
           "#d73027",
           "#f46d43",
           "#fdae61",
           "#fee090",
           "#5aae61",
           "#e0f3f8",
           "#abd9e9",
           "#74add1",
           "#4575b4",
           "#313695")
  
  label.df = data.frame(
    text.label = c("Mixtures", "Rotations"),
    label_x_coord = c(115000, 115000), #have the label fairly central.
    label_y_coord = c(75, -55)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.2 = data.frame(
    text.label =c("Novel", "Pre-Used"),
    label_x_coord = c(22500, 85000),
    label_y_coord = c(95, 95)
  )
  
  final.plot = ggplot(comparison.df, aes(x=replicate,
                                         y=prop.diff.rot.mix*100))+
    geom_point(aes(colour = as.factor(cross.selection)),
               alpha = 0.2)+
    scale_colour_manual(values = pals)+
    geom_vline(xintercept = 55000, linetype = "dashed") +
    geom_hline(yintercept = 0)+
    xlab("Simulation Replicate")+
    ylab("Percentage Difference Duration")+
    ylim(-100, 100)+
    xlim(0, 115000)+
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = label.text.size)+
    geom_label(data = label.df.2, aes(label = text.label,
                                      x=label_x_coord,
                                      y=label_y_coord),
               fill = c("orchid1", "orchid3"),
               size = annotation.size)+
    ggtitle("C: Rotations vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  return(final.plot)
}
compare_rot_seq_plot = function(label.text.size = 5,
                                annotation.size = 2){
  
  pals = c("#a50026",
           "#d73027",
           "#f46d43",
           "#fdae61",
           "#fee090",
           "#5aae61",
           "#e0f3f8",
           "#abd9e9",
           "#74add1",
           "#4575b4",
           "#313695")
  
  label.df = data.frame(
    text.label = c("Rotations", "Sequences"),
    label_x_coord = c(115000, 115000), #have the label fairly central.
    label_y_coord = c(75, -55)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.2 = data.frame(
    text.label =c("Novel", "Pre-Used"),
    label_x_coord = c(22500, 85000),
    label_y_coord = c(95, 95)
  )
  
  final.plot = ggplot(comparison.df, aes(x=replicate,
                                         y=prop.diff.seq.rot*100))+
    geom_point(aes(colour = as.factor(cross.selection)),
               alpha = 0.2)+
    scale_colour_manual(values = pals)+
    geom_vline(xintercept = 55000, linetype = "dashed") +
    geom_hline(yintercept = 0)+
    xlab("Simulation Replicate")+
    ylab("Percentage Difference Duration")+
    ylim(-100, 100)+
    xlim(0, 115000)+
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = label.text.size)+
    geom_label(data = label.df.2, aes(label = text.label,
                                      x=label_x_coord,
                                      y=label_y_coord),
               fill = c("orchid1", "orchid3"),
               size = annotation.size)+
    ggtitle("A: Sequences vs Rotations")+
    theme_classic()+
    theme(legend.position = "none")
  
  return(final.plot)
}
compare_seq_mix_plot = function(label.text.size = 5,
                                annotation.size = 2){
  
  pals = c("#a50026",
           "#d73027",
           "#f46d43",
           "#fdae61",
           "#fee090",
           "#5aae61",
           "#e0f3f8",
           "#abd9e9",
           "#74add1",
           "#4575b4",
           "#313695")
  
  label.df = data.frame(
    text.label = c("Mixtures", "Sequences"),
    label_x_coord = c(115000, 115000), #have the label fairly central.
    label_y_coord = c(75, -55)) #Should be far enough away to not be overlapping any bars/points
  
  label.df.2 = data.frame(
    text.label =c("Novel", "Pre-Used"),
    label_x_coord = c(22500, 85000),
    label_y_coord = c(95, 95)
  )
  
  final.plot = ggplot(comparison.df, aes(x=replicate,
                                         y=prop.diff.seq.mix*100))+
    geom_point(aes(colour = as.factor(cross.selection)),
               alpha = 0.2)+
    scale_colour_manual(values = pals)+
    geom_vline(xintercept = 55000, linetype = "dashed") +
    geom_hline(yintercept = 0)+
    xlab("Simulation Replicate")+
    ylab("Percentage Difference Duration")+
    ylim(-100, 100)+
    xlim(0, 115000)+
    geom_text(data = label.df, aes(label = text.label,
                                   x=label_x_coord,
                                   y=label_y_coord),
              angle = 270, size = label.text.size)+
    geom_label(data = label.df.2, aes(label = text.label,
                                      x=label_x_coord,
                                      y=label_y_coord),
               fill = c("orchid1", "orchid3"),
               size = annotation.size)+
    ggtitle("B: Sequences vs Mixtures")+
    theme_classic()+
    theme(legend.position = "none")
  
  return(final.plot)
}
get_figure_2_legend = function(){
  
  pals = c("#a50026",
           "#d73027",
           "#f46d43",
           "#fdae61",
           "#fee090",
           "#5aae61",
           "#e0f3f8",
           "#abd9e9",
           "#74add1",
           "#4575b4",
           "#313695")
  
  #Scatterplot of difference in duration (n=7033).
  figure = ggplot(comparison.df, aes(x = replicate, y=prop.diff.seq.mix)) +
    geom_point(aes(colour = as.factor(cross.selection)),
               alpha = 0.8,
               size = 6)+
    scale_colour_manual(values = pals)+
    guides(colour=guide_legend(ncol=1, title="Cross Selection"))+
    theme_classic()+
    theme(legend.text=element_text(size=10),
          legend.title=element_text(size=10))
  
  fig.legend = ggpubr::as_ggplot(ggpubr::get_legend(figure))
  
  return(fig.legend)
}

make_figure_2 = function(label.text.size,
                         annotation.size){
  
  plot.A = compare_rot_seq_plot(label.text.size = label.text.size,
                                annotation.size = annotation.size)
  plot.B = compare_seq_mix_plot(label.text.size = label.text.size,
                                annotation.size = annotation.size)
  plot.C = compare_rot_mix_plot(label.text.size = label.text.size,
                                annotation.size = annotation.size)
  plot.D = get_figure_2_legend()
  
  layout = "
  AAAAAAD
  BBBBBBD
  CCCCCCD"
  
  E = patchwork::wrap_plots(A = plot.A,
                            B = plot.B,
                            C = plot.C,
                            D = plot.D,
                            design = layout)
  
  
  return(E)
}

make_figure_2(label.text.size = 3,
              annotation.size = 3)


#summarise the datasets
summary(sequence.set)
summary(rotation.set)
summary(mixture.set)

#How many simulations ran to completion
nrow(sequence.set%>%
  dplyr::filter(simulation.duration == 500))
69368/1100

nrow(rotation.set%>%
       dplyr::filter(simulation.duration == 500))
72530/1100

nrow(mixture.set%>%
       dplyr::filter(simulation.duration == 500))
101536/1100
##########################################
## 3. Comparing Sequences and Rotations ##
##########################################
rot.duration = rotation.set$simulation.duration
seq.duration = sequence.set$simulation.duration
prop.diff.seq.rot = 1 - (sequence.set$simulation.duration/rotation.set$simulation.duration)

rot.seq.outcome = c()
rot.seq.operational.outcome = c()
for(i in 1:110000){
  if(rot.duration[i] > seq.duration[i]){rot.seq.outcome[i] = "rotation.win"}
  if(rot.duration[i] < seq.duration[i]){rot.seq.outcome[i] = "sequence.win"}
  if(rot.duration[i] == seq.duration[i]){rot.seq.outcome[i] = "draw"}
  
  if(prop.diff.seq.rot[i] >= 0.1){rot.seq.operational.outcome[i] = "rotation win"}
  if(prop.diff.seq.rot[i] <= -0.1){rot.seq.operational.outcome[i] = "sequence win"}
  if(prop.diff.seq.rot[i] < 0.1 & 
     prop.diff.seq.rot[i] > -0.1){rot.seq.operational.outcome[i] = "no operational win"}
  
}

table(rot.seq.outcome)
table(rot.seq.outcome)/1100

table(rot.seq.operational.outcome)
table(rot.seq.operational.outcome)/1100

#proportion rotation operational wins to rotation wins:
8717/19473*100

#proportion sequence operational wins to sequence wins:
5480/16540 * 100


###Compare the draws

rot.peak = rotation.set$peak.resistance
seq.peak = sequence.set$peak.resistance
rot.average = rotation.set$mean.resistance.intensity.
seq.average = sequence.set$mean.resistance.intensity.

rot.peak.survival = c()
rot.mean.survival = c()
seq.peak.survival = c()
seq.mean.survival = c()

for(i in 1:110000){
  
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
}


rot.seq.secondary.outcome.peak = c()
rot.seq.secondary.outcome.mean = c()
for(i in 1:110000){
  if(seq.peak.survival[i] < rot.peak.survival[i]){rot.seq.secondary.outcome.peak[i] = "sequence.win"}
  if(seq.peak.survival[i] > rot.peak.survival[i]){rot.seq.secondary.outcome.peak[i] = "rotation.win"} 
  if(seq.peak.survival[i] == rot.peak.survival[i]){rot.seq.secondary.outcome.peak[i] = "draw"}
  
  if(seq.mean.survival[i] < rot.mean.survival[i]){rot.seq.secondary.outcome.mean[i] = "sequence.win"}
  if(seq.mean.survival[i] > rot.mean.survival[i]){rot.seq.secondary.outcome.mean[i] = "rotation.win"} 
  if(seq.mean.survival[i] == rot.mean.survival[i]){rot.seq.secondary.outcome.mean[i] = "draw"}
  
}

sum(is.na(rot.seq.secondary.outcome.mean))

rot.seq.outcome.df = data.frame(rot.seq.secondary.outcome.peak,
                                rot.seq.secondary.outcome.mean,
                                rot.seq.outcome,
                                rot.seq.operational.outcome,
                                parameter.space.df)

rot.seq.outcome.df$starting.status = ifelse(rot.seq.outcome.df$start.resistance.values == 0,
                                    yes = "novel",
                                    no = "pre-used")



rot.seq.outcome.df.draws = rot.seq.outcome.df%>%
  dplyr::filter(rot.seq.outcome == "draw")


table(rot.seq.outcome.df.draws$rot.seq.secondary.outcome.peak)
round(((table(rot.seq.outcome.df.draws$rot.seq.secondary.outcome.peak)/73987)*100), 2)

table(rot.seq.outcome.df.draws$rot.seq.secondary.outcome.mean)
round(((table(rot.seq.outcome.df.draws$rot.seq.secondary.outcome.mean)/73987)*100), 2)







##Fit model 1:

sample.size = floor(0.75 * nrow(rot.seq.outcome.df))

train.ind = sample(seq_len(nrow(rot.seq.outcome.df)), size = sample.size)

data.train = rot.seq.outcome.df[train.ind, ]
data.test = rot.seq.outcome.df[-train.ind, ]


rot.seq.fit.model1 = rpart(rot.seq.operational.outcome~
              Heritability +
              Fitness.Cost +
              Male.Insecticide.Exposure+
              Female.Insecticide.Exposure+
              Dispersal+
              Intervention.Coverage +
              starting.status+
              cross.selection.values,
            data = data.train, 
            method = 'class',
            control = rpart.control(minbucket = 200, #each split is ~0.25% of samples
                                    maxdepth = 6,
                                    cp = 0))

predict.unseen = predict(rot.seq.fit.model1, data.test, type = 'class')
actual.outcome = data.test$rot.seq.operational.outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  

sum(correct.outcome)/length(correct.outcome)

#Of those that were wrong, how wrong were they::
prediction.df = data.frame(predict.unseen, actual.outcome)

incorrect.prediction.df = prediction.df%>%
  dplyr::filter(predict.unseen != actual.outcome)

#could deploy a loss::
wrongly.deploys.sequence = incorrect.prediction.df%>%
  dplyr::filter(predict.unseen == "no operational win")%>%
  dplyr::filter(actual.outcome == "sequence loss" |
                  actual.outcome == "mixture win")

wrongly.deploys.rotation = incorrect.prediction.df%>%
  dplyr::filter(predict.unseen == "no operational win")%>%
  dplyr::filter(actual.outcome == "rotation loss"|
                  actual.outcome == "mixture win")

wrongly.deploys.mixture = incorrect.prediction.df%>%
  dplyr::filter(predict.unseen == "no operational win")%>%
  dplyr::filter(actual.outcome == "mixture win")

rpart.plot(rot.seq.fit.model1,
           type = 5,
           tweak = 1.05,
           extra = 100,
           legend.y = "NULL",
           box.palette = list("#377eb8",
                              "#b2df8a",
                              "#ffff33"),
           fallen.leaves = TRUE,
           uniform = TRUE)
title("A: Regression Classification Tree -  Sequences and Rotations", cex=1)



rot.seq.fit.model2 = rpart(rot.seq.operational.outcome~
                             Female.Insecticide.Exposure+
                             Intervention.Coverage +
                             starting.status+
                             cross.selection.values,
                           data = data.train, 
                           method = 'class',
                           control = rpart.control(minbucket = 200, #each split is ~0.5% of samples
                                                   maxdepth = 6,
                                                   cp = 0))

predict.unseen = predict(rot.seq.fit.model2, data.test, type = 'class')
actual.outcome = data.test$rot.seq.operational.outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  

sum(correct.outcome)/length(correct.outcome)

rpart.plot(rot.seq.fit.model2,
           type = 0,
           tweak = 1,
           extra = 100,
           legend.y = "NULL",
           box.palette = list("#377eb8",
                              "#b2df8a",
                              "#ffff33"),
           fallen.leaves = TRUE,
           uniform = TRUE)
title("B: Regression Classification Tree -  Sequences and Rotations", cex=1)










####Comparing Mixtures, Rotations and Sequences
rot.duration = rotation.set$simulation.duration
seq.duration = sequence.set$simulation.duration
mix.duration = mixture.set$simulation.duration

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


rot.peak = rotation.set$peak.resistance
seq.peak = sequence.set$peak.resistance
mix.peak = mixture.set$peak.resistance

rot.average = rotation.set$mean.resistance.intensity.
seq.average = sequence.set$mean.resistance.intensity.
mix.average = mixture.set$mean.resistance.intensity.

rot.peak.survival = c()
rot.mean.survival = c()
seq.peak.survival = c()
seq.mean.survival = c()
mix.peak.survival = c()
mix.mean.survival = c()

for(i in 1:110000){
  
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
for(i in 1:110000){
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

#Check for any NAs: if sums to 110000 then all values were assigned
sum(!is.na(secondary.outcome.peak))

secondary.outcome.average = c()
for(i in 1:110000){
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
#check for NAs, if sums to 110000 all values assigned
sum(!is.na(secondary.outcome.average)) #all accounted for


parameter.space.df = read.csv("Simulation Experiments/Sets_of_Simulations/Publication Simulations/paramater.space.df.publication.csv")

outcome.df = data.frame(parameter.space.df, outcome, operational.outcome,
                        secondary.outcome.average, secondary.outcome.peak)



outcome.df$starting.status = ifelse(outcome.df$start.resistance.values == 0,
                                    yes = "novel",
                                    no = "pre-used")



table(outcome.df$outcome)
#as percentage
table(outcome.df$outcome)/110000 * 100


table(outcome.df$operational.outcome)
#as percentange:
table(outcome.df$operational.outcome)/110000 * 100



#compare draws on secondary outcomes: can only compare secondary outcomes where duration is identical
draw.outcome.df = outcome.df%>%
  dplyr::filter(outcome == "draw")

table(draw.outcome.df$secondary.outcome.peak)

round(table(draw.outcome.df$secondary.outcome.peak)/68753*100, 2)

table(draw.outcome.df$secondary.outcome.average)
round(table(draw.outcome.df$secondary.outcome.average)/68753*100, 2)





#######################################################################
## 3. Data Analysis: Sensitivity Analysis ; Partial Rank Correlation ##
#######################################################################

#Partial Rank Correlation:: (uses EpiR)
df_from_pcor_test = function(irm.strategy,
                             data,
                             start.resistance,
                             cross.selection){
  
  pcor.df = data%>%
    dplyr::filter(strategy == strategy)%>%
    dplyr::filter(start.resistance == start.resistance)%>%
    dplyr::filter(cross.selection == cross.selection)%>%
        dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                  "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration")
  
  
  #Perform two sided partial rank correlation. 95% CIs
  pcor.output = epiR::epi.prcc(dat = pcor.df,
                               sided.test = 2,
                               conf.level = 0.95)
  
  x = ifelse(start.resistance == 0, 
             yes = "novel insecticides",
             no = "pre-used insecticdes")
  
  Simulation.Conditions = stringr::str_c(irm.strategy, " with ", x)
  
  estimate = pcor.output$est
  pvalue = pcor.output$p.value
  teststat = pcor.output$test.statistic
  lower.95.ci = pcor.output$lower
  upper.95.ci = pcor.output$upper

  strategy = irm.strategy
  cross.selection = cross.selection
  
  #parameters in correct order::
  parameter = c("Insecticide Resistance Heritability", "Fitness Cost", "Male Insecticide Exposure",
                "Intervention Coverage", "Mosquito Dispersal", "Female Insecticide Exposure")
  
  output = data.frame(estimate, lower.95.ci, upper.95.ci,  pvalue, teststat, strategy, Simulation.Conditions, parameter, cross.selection)
  
  return(output)
}

pcor_df_function = function(X,
                            dataset){

cross.selection.vals = seq(-0.5, 0.5, by = 0.1)
start.resist.vals = c(0, 50)

pcor.list = list()
pcor.list2 = list()
for(j in 1:2){
  for(i in 1:11){
  pcor.list[[i]] = df_from_pcor_test(X, dataset, start.resist.vals[j], cross.selection.vals[i])
  }
  pcor.list2[[j]] = pcor.list
}

pcor.df = do.call(rbind, do.call(rbind, pcor.list2))

return(pcor.df)
}


pcor.seq.df = pcor_df_function("sequence", sequence.set)
pcor.rot.df = pcor_df_function("rotation", rotation.set)
pcor.mix.df = pcor_df_function("mixture", mixture.set)


pcor.df.all = rbind(pcor.seq.df,
                    pcor.rot.df,
                    pcor.mix.df)

make_pcor_plot = function(){
  
  pals = c("#74c476", "#006d2c", "#fb6a4a", "#a50f15", "#4eb3d3", "#084081")

  
  label.df = data.frame(
    text.label = c("Increases Resistance", "Decreases Resistance"),
    label_x_coord = c(-0.5, 0.5), #have the label fairly central.
    label_y_coord = c("Mosquito Dispersal", "Mosquito Dispersal")) #Should be far enough away to not be overlapping any bars/points

  figure = ggplot(pcor.df.all, aes(x=estimate, y=parameter, xmin =lower.95.ci,
                       xmax = upper.95.ci, fill = Simulation.Conditions)) +
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbarh(position = position_dodge())+
    scale_fill_manual(values = pals)+
    geom_vline(xintercept = 0) +
    xlim(-0.75, 0.75)+
    ylab("Parameter") +
    xlab("Partial Rank Correlation") +
    guides(fill=guide_legend(ncol=3)) +
    theme_classic()+
    facet_wrap(~as.factor(cross.selection))+
    theme(legend.position = "bottom",
          legend.justification = "left")
  
    return(figure)
}

make_pcor_plot()


####################################################
## 4. Generalised Linear (and Additive) Modelling ##
####################################################

##Generalised Linear (and Addditive) Modelling::
  #join the 3 dataframes together::

sequence.set.glm = sequence.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration",
                "strategy", "start.resistance", "cross.selection")

rotation.set.glm = rotation.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration",
                "strategy", "start.resistance", "cross.selection")

mixture.set.glm = mixture.set%>%
  dplyr::select("Heritability", "Fitness.Cost", "Male.Insecticide.Exposure",
                "Intervention.Coverage", "Dispersal", "Female.Insecticide.Exposure", "simulation.duration",
                "strategy", "start.resistance", "cross.selection")


df.for.glm = rbind(sequence.set.glm,
                   rotation.set.glm,
                   mixture.set.glm)


##First do Generalised Additive Models to Check for non-linear Relationship

df.for.glm$maximum.generations = 500

df.for.glm$starting.status = ifelse(df.for.glm$start.resistance == 0,
                                    yes = "novel",
                                    no = "pre-used")

df.for.glm.2 = df.for.glm%>%
  dplyr::filter(simulation.duration != 500)

gam.heritability = mgcv::bam(simulation.duration ~ 
                               as.factor(strategy)+
                               s(Heritability) +
                               Fitness.Cost +
                               Male.Insecticide.Exposure+
                               Female.Insecticide.Exposure+
                               Dispersal+
                               Intervention.Coverage +
                             data = df.for.glm.2,
                             family = "poisson")

plot(gam.heritability)#broadly linear, as expected

gam.fitness.cost = mgcv::bam(simulation.duration ~ 
                               as.factor(strategy) +
                               Heritability +
                               s(Fitness.Cost) +
                               Male.Insecticide.Exposure+
                               Female.Insecticide.Exposure+
                               Dispersal+
                               Intervention.Coverage+
                             data = df.for.glm.2,
                             family = "poisson")

plot(gam.fitness.cost)#wiggly but generally trending upwards

gam.male.exposure = mgcv::bam(simulation.duration ~ 
                                as.factor(strategy)+
                                Heritability +
                                Fitness.Cost +
                                s(Male.Insecticide.Exposure)+
                                Female.Insecticide.Exposure+
                                Dispersal+
                                Intervention.Coverage,
                              data = df.for.glm.2,
                              family = "poisson")

plot(gam.male.exposure)#linear relationship


gam.female.exposure = mgcv::bam(simulation.duration ~ 
                                 as.factor(strategy)+
                                 Heritability +
                                 Fitness.Cost +
                                 Male.Insecticide.Exposure+
                                 s(Female.Insecticide.Exposure)+
                                 Dispersal+
                                 Intervention.Coverage,
                               data = df.for.glm.2,
                               family = "poisson")


plot(gam.female.exposure)#linear


gam.dispersal= mgcv::bam(simulation.duration ~ 
                           as.factor(strategy)+
                           Heritability +
                           Fitness.Cost +
                           Male.Insecticide.Exposure+
                           Female.Insecticide.Exposure+
                           s(Dispersal)+
                           Intervention.Coverage,
                         data = df.for.glm.2,
                         family = "poisson")

plot(gam.dispersal)#hugely wiggly
#Look to put splines in at: ~0.3, ~0.8
summary(gam.dispersal)

gam.coverage= mgcv::bam(simulation.duration ~ 
                          as.factor(strategy)+
                          Heritability +
                          Fitness.Cost +
                          Male.Insecticide.Exposure+
                          Female.Insecticide.Exposure+
                          Dispersal+
                          s(Intervention.Coverage),
                        data = df.for.glm.2,
                        family = "poisson")


plot(gam.coverage) #broadly linear


#find the location of the splines which maximise the likelihood
find.max.logLik = function(dspline1, dspline2,  data){
  
  data$disperal.spline1 = ifelse(data$Dispersal > dspline1, data$Dispersal-dspline1, 0)
  data$dispersal.spline2  = ifelse(data$Dispersal > dspline2, data$Dispersal-dspline2, 0 )

  
  temp.glm = glm(simulation.duration ~ 
                   strategy +
                   Heritability +
                   Fitness.Cost +
                   Male.Insecticide.Exposure+
                   Female.Insecticide.Exposure+
                   Dispersal+
                   disperal.spline1+
                   dispersal.spline2+
                   Intervention.Coverage+
                   starting.status+
                   cross.selection,
                 data = data,
                 family = "poisson")
  
  return(logLik(temp.glm))
}

find.max.logLik(dspline1 = 0.238, 
                dspline2 = 0.785, 
                data = df.for.glm.2)

df.for.glm$disperal.spline1 = ifelse(df.for.glm$Dispersal > 0.238, df.for.glm$Dispersal-0.387, 0)
df.for.glm$dispersal.spline2  = ifelse(df.for.glm$Dispersal > 0.785, df.for.glm$Dispersal-0.486, 0 )



poisson.glm =  glm(simulation.duration ~ 
                     strategy +
                     Heritability +
                     Fitness.Cost +
                     Male.Insecticide.Exposure+
                     Female.Insecticide.Exposure+
                     Dispersal+
                     disperal.spline1+
                     dispersal.spline2+
                     Intervention.Coverage+
                     starting.status +
                     cross.selection,
                     data = df.for.glm.2,
                   family = "poisson")

AER::dispersiontest(poisson.glm) #overdispersed

neg.bin.glm =  MASS::glm.nb(simulation.duration ~ 
                              strategy +
                              Heritability +
                              Fitness.Cost +
                              Male.Insecticide.Exposure+
                              Female.Insecticide.Exposure+
                              Dispersal+
                              disperal.spline1+
                              dispersal.spline2+
                              Intervention.Coverage+
                              starting.status +
                              cross.selection,
                            data = df.for.glm.2)
  
summary(neg.bin.glm)
confint(neg.bin.glm)


##########################################################################
##5. Regression Classification Trees: Mixtures, Rotations and Sequences ##
##########################################################################

sample.size = floor(0.75 * nrow(outcome.df))

train.ind = sample(seq_len(nrow(outcome.df)), size = sample.size)

data.train = outcome.df[train.ind, ]
data.test = outcome.df[-train.ind, ]

mix.rot.seq.fit.model1 = rpart(operational.outcome~
                                 Heritability +
                                 Fitness.Cost +
                                 Male.Insecticide.Exposure+
                                 Female.Insecticide.Exposure+
                                 Dispersal+
                                 Intervention.Coverage +
                                 starting.status+
                                 cross.selection.values, 
                               data = data.train, 
                               method = 'class',
                               control = rpart.control(minbucket = 200, #each split is ~0.5% of samples
                                                       maxdepth = 6,
                                                       cp = 0))

predict.unseen = predict(mix.rot.seq.fit.model1, data.test, type = 'class')
actual.outcome = data.test$operational.outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  

sum(correct.outcome)/length(correct.outcome)

#Of those that were wrong, how wrong were they::
  prediction.df = data.frame(predict.unseen, actual.outcome)

incorrect.prediction.df = prediction.df%>%
  dplyr::filter(predict.unseen != actual.outcome)

#could deploy a loss::
wrongly.deploys.sequence = incorrect.prediction.df%>%
  dplyr::filter(predict.unseen == "no operational win")%>%
  dplyr::filter(actual.outcome == "sequence loss" |
                actual.outcome == "mixture win")

wrongly.deploys.rotation = incorrect.prediction.df%>%
  dplyr::filter(predict.unseen == "no operational win")%>%
  dplyr::filter(actual.outcome == "rotation loss"|
                  actual.outcome == "mixture win")

wrongly.deploys.mixture = incorrect.prediction.df%>%
  dplyr::filter(predict.unseen == "no operational win")%>%
  dplyr::filter(actual.outcome == "mixture win")


rpart.plot(mix.rot.seq.fit.model1,
           type = 5,
           tweak = 1,
           extra = 100,
           legend.y = "NULL",
           box.palette = list("#f03b20",
                              "#377eb8"),
           fallen.leaves = TRUE,
           uniform = TRUE)
title("A: Regression Classification Tree - Sequences, Rotations and Mixtures")



mix.rot.seq.fit.model2 = rpart(operational.outcome~
                                 Female.Insecticide.Exposure+
                                 Intervention.Coverage +
                                 starting.status+
                                 cross.selection.values, 
                               data = data.train, 
                               method = 'class',
                               control = rpart.control(minbucket = 400, #each split is ~0.5% of samples
                                                       maxdepth = 10,
                                                       cp = 0))

predict.unseen = predict(mix.rot.seq.fit.model2, data.test, type = 'class')
actual.outcome = data.test$operational.outcome

correct.outcome = ifelse(predict.unseen == actual.outcome,
                         yes = 1,
                         no = 0)  

sum(correct.outcome)/length(correct.outcome)


rpart.plot(mix.rot.seq.fit.model2,
           type = 5,
           tweak = 1.08,
           extra = 100,
           legend.y = "NULL",
           box.palette = list("#377eb8",
                              "#b2df8a",
                              "#ffff33"),
           fallen.leaves = TRUE,
           uniform = TRUE)
title("B: Regression Classification Tree - Sequences, Rotations and Mixtures")
















#Example plots of simulations::::
sample.to.visualise = dplyr::sample_n(mixture.set[1:55000, ], 1) #limit to those starting at 0. 

sample.to.visualise = mixture.set%>%
  dplyr::filter(X.1 == 28963)


print(sample.to.visualise)

plot.seq.df = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
                                                                              exposure.scaling.factor = 10,
                                                                              nsim = 1,
                                                                              minimum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              maximum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              minimum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              maximum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              minimum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              maximum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              resistance.cost = sample.to.visualise$Fitness.Cost,
                                                                              starting.treatment.site.intensity = sample.to.visualise$start.resistance.values,
                                                                              starting.refugia.intensity = sample.to.visualise$start.resistance.values,
                                                                              min.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              max.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              min.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              max.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              maximum.generations = 500, 
                                                                              irm.strategy = "sequence",
                                                                              half.population.bioassay.survival.resistance = 900,
                                                                              withdrawal.threshold.value = 0.1, 
                                                                              return.threshold.value = 0.08, 
                                                                              deployment.frequency = 10, 
                                                                              maximum.resistance.value = 25000, 
                                                                              min.cross.selection = sample.to.visualise$cross.selection.values,
                                                                              max.cross.selection = sample.to.visualise$cross.selection.values),
                                  maximum.generations = 500, number.of.insecticides = 2)

plot.rot.df = get_simulation_dataframe(run_simulation_intervention_cross_selection(number.of.insecticides = 2,
                                                                              exposure.scaling.factor = 10,
                                                                              nsim = 1,
                                                                              minimum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              maximum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                              minimum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              maximum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                              minimum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              maximum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                              resistance.cost = sample.to.visualise$Fitness.Cost,
                                                                              starting.treatment.site.intensity = sample.to.visualise$start.resistance.values,
                                                                              starting.refugia.intensity = sample.to.visualise$start.resistance.values,
                                                                              min.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              max.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                              min.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              max.dispersal.rate = sample.to.visualise$Dispersal,
                                                                              maximum.generations = 500, 
                                                                              irm.strategy = "rotation",
                                                                              half.population.bioassay.survival.resistance = 900,
                                                                              withdrawal.threshold.value = 0.1, 
                                                                              return.threshold.value = 0.08, 
                                                                              deployment.frequency = 10, 
                                                                              maximum.resistance.value = 25000, 
                                                                              min.cross.selection = sample.to.visualise$cross.selection.values,
                                                                              max.cross.selection = sample.to.visualise$cross.selection.values),
                                  maximum.generations = 500, number.of.insecticides = 2)

plot.mix.df = seq.df = get_simulation_dataframe_mixtures(run_simulation_intervention_test_mixtures_cross_selection(number.of.insecticides = 2,
                                                                                            exposure.scaling.factor = 10,
                                                                                            nsim = 1,
                                                                                            minimum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                                            maximum.insecticide.resistance.heritability = sample.to.visualise$Heritability,
                                                                                            minimum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                                            maximum.male.insecticide.exposure = sample.to.visualise$Male.Insecticide.Exposure,
                                                                                            minimum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                                            maximum.female.insecticide.exposure = sample.to.visualise$Female.Insecticide.Exposure,
                                                                                            resistance.cost = sample.to.visualise$Fitness.Cost,
                                                                                            starting.treatment.site.intensity = sample.to.visualise$start.resistance.values,
                                                                                            starting.refugia.intensity = sample.to.visualise$start.resistance.values,
                                                                                            min.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                                            max.intervention.coverage = sample.to.visualise$Intervention.Coverage,
                                                                                            min.dispersal.rate = sample.to.visualise$Dispersal,
                                                                                            max.dispersal.rate = sample.to.visualise$Dispersal,
                                                                                            maximum.generations = 500, 
                                                                                            irm.deployment.strategy = "mixtures", 
                                                                                            mixture.strategy = "mix.sequential.discrete",
                                                                                            irm.switch.strategy = "sequence", 
                                                                                            half.population.bioassay.survival.resistance = 900,
                                                                                            withdrawal.threshold.value = 0.1, 
                                                                                            return.threshold.value = 0.08, 
                                                                                            deployment.frequency = 10, 
                                                                                            maximum.resistance.value = 25000, 
                                                                                            min.cross.selection = sample.to.visualise$cross.selection.values,
                                                                                            max.cross.selection = sample.to.visualise$cross.selection.values,
                                                                                            conversion.factor = 0.48,
                                                                                            intercept = 0.15),
                                                maximum.generations = 500, number.of.insecticides = 2)



###only plot the intervention sites:: convert resistance to bioassay
  plot.seq.df = plot.seq.df%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(mean.population.resistance = resistance.intensity,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      sd.population.resistance = 0, #measured without error
                                                                      nsim = 1,
                                                                      michaelis.menten.slope = 1)*100)
  
  
  

  plot.rot.df = plot.rot.df%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(mean.population.resistance = resistance.intensity,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      sd.population.resistance = 0, #measured without error
                                                                      nsim = 1,
                                                                      michaelis.menten.slope = 1)*100)
  
  
  
  plot.mix.df = plot.mix.df%>%
    dplyr::filter(site == "treatment")%>%
    dplyr::rowwise()%>%
    dplyr::mutate(bioassay.survival = resistance_to_bioassay_survival(mean.population.resistance = resistance.intensity,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      sd.population.resistance = 0, #measured without error
                                                                      nsim = 1,
                                                                      michaelis.menten.slope = 1)*100)


panel_sequence = ggplot(plot.seq.df, aes(x=time.in.generations,
                        y=bioassay.survival,
                        colour = as.character(insecticide.tracked)))+
  geom_line(size = 2.5, alpha = 0.7)+
  scale_colour_manual(values = c("#e31a1c", "#377eb8"))+
  geom_point(aes(x=time.in.generations,
                y=10, colour = insecticide.deployed),
            size = 1)+
  geom_hline(yintercept = 8)+
  geom_segment(aes(x=500, xend=500, y=0, yend = 10),
               linetype ="dashed", 
               alpha = 0.5, 
               colour = "orange")+
  ylim(0, 10)+
  ggtitle("IRM Strategy: SEQUENCE")+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(text = element_text(size = 20))
  

panel_rotation = ggplot(plot.rot.df, aes(x=time.in.generations,
                        y=bioassay.survival,
                        colour = as.character(insecticide.tracked)))+
  geom_line(size = 2.5, alpha = 0.7)+
  scale_colour_manual(values = c("#e31a1c", "#377eb8"))+
  geom_point(aes(x=time.in.generations,
                y=10, colour = insecticide.deployed,
                alpha = 0.6), size = 1)+
  geom_hline(yintercept = 8)+
  geom_segment(aes(x=500, xend=500, y=0, yend = 10),
               linetype ="dashed", 
               alpha = 0.5, 
               colour = "orange")+
  ylim(0, 10)+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("IRM Strategy: ROTATION")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(text = element_text(size = 20))
  
panel_rotation

plot.mix.df.1 = plot.mix.df%>%
  dplyr::filter(insecticide.tracked == 1)

plot.mix.df.2 = plot.mix.df%>%
  dplyr::filter(insecticide.tracked == 2)


panel_mixture = ggplot(plot.mix.df, aes(x=time.in.generations,
                        y=bioassay.survival))+
  geom_line(data = plot.mix.df.1,
            aes(x=time.in.generations,
                y=bioassay.survival),
            colour = "#377eb8",
            size = 5, alpha =0.7)+
    geom_line(data = plot.mix.df.1,
              aes(x=time.in.generations,
                  y=bioassay.survival),
              size = 2,
              colour = "#e31a1c",
              alpha = 0.7)+
  geom_hline(yintercept = 10, colour = "#377eb8",
             size = 5)+
  geom_hline(yintercept = 10, colour = "#e31a1c",
             size = 2)+
  geom_hline(yintercept = 8)+
  geom_segment(aes(x=500, xend=500, y=0, yend = 10),
               linetype ="dashed", 
               alpha = 0.5, 
               colour = "orange")+
  ylim(0, 10)+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("IRM Strategy: MIXTURE")+
  theme_classic()+
  theme(text = element_text(size = 20))

panel_mixture

gridExtra::grid.arrange(panel_sequence,
                        panel_rotation,
                        panel_mixture,
                        nrow = 1)


####
#Demonstrating the Scaling of Beta (exposure scaling factor):::
response.values = response_to_insecticide_selection(exposure.scaling.factor = 10,
                                                    minimum.insecticide.resistance.heritability = 0.05,
                                                    maximum.insecticide.resistance.heritability = 0.30,
                                                    minimum.female.insecticide.exposure = 0.4,
                                                    maximum.female.insecticide.exposure = 0.9,
                                                    minimum.male.insecticide.exposure = 0,
                                                    maximum.male.insecticide.exposure = 1,
                                                    nsim = 100000)
calibrated =  10/response.values
df = data.frame(calibrated)

ggplot(df, aes(x=calibrated))+
  geom_histogram(bins = 100,
                 colour = "black",
                 fill = "lightblue",)+
  geom_vline(xintercept = 8, colour = "red",
             size = 2, alpha = 0.7)+
  geom_vline(xintercept = 12, colour = "red",
             size = 2, alpha = 0.7)+
  scale_x_continuous(breaks = seq(0, 80, by = 10))+
  xlab("Years to 10% Bioassay Survival")+
  ylab("Frequency")+
  theme_classic()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))





table_resistance_from_survival_and_sd(half.population.bioassay.survival.resistance = 900, 
                                      maximum.bioassay.survival.proportion = 1, 
                                      michaelis.menten.slope = 1, 
                                      bioassay.survival.values = c(0.01, 0.05, 0.1, 0.2, 0.5), 
                                      sd.population.values = c(0, 1, 5, 10, 25), 
                                      estimate.precision = 0.01, 
                                      nsim = 1000, 
                                      minimum.resistance.value = 0, 
                                      maximum.resistance.value = 25000)


df = table_resistance_from_survival_and_sd(half.population.bioassay.survival.resistance = 900, 
                                           maximum.bioassay.survival.proportion = 1, 
                                           michaelis.menten.slope = 1, 
                                           bioassay.survival.values = c(0, 0.05, 0.1, 0.2, 0.5), 
                                           sd.population.values = c(0, 1, 5, 10, 25), 
                                           estimate.precision = 0.001, 
                                           nsim = 1000, 
                                           minimum.resistance.value = 0, 
                                           maximum.resistance.value = 25000)

df = df%>%
  dplyr::mutate(resistance.values = round(resistance.values, digits = 3))

df

table_resistance_from_survival_and_sd(half.population.bioassay.survival.resistance = 900, 
                                      maximum.bioassay.survival.proportion = 1, 
                                      michaelis.menten.slope = 1, 
                                      bioassay.survival.values = 0.01, 
                                      sd.population.values = 25, 
                                      estimate.precision = 0.0000000001, 
                                      nsim = 10000000, 
                                      minimum.resistance.value = 0, 
                                      maximum.resistance.value = 25000)
